import argparse
import csv
import json
from pathlib import Path
from .preprocessing import load_papers, annual_counts
from .statistics import bootstrap_mean
from .regression import fit_poisson
from .network import build_graph, pagerank_sparse


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('command', choices=['annual','regression','network','validate','benchmark'])
    parser.add_argument('--wos', type=Path)
    parser.add_argument('--scopus', type=Path)
    parser.add_argument('--end-year', type=int, default=2025)
    parser.add_argument('--seed', type=int, default=42)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    if args.command == 'benchmark':
        from benchmarks.benchmark_network import main as benchmark
        benchmark()
        return
    folder = Path('New Publications') / ('Co-authorship Network' if args.command == 'network' else 'Annual')
    wos = args.wos or folder / 'WoS.csv'
    scopus = args.scopus or folder / 'Scopus.csv'
    try:
        if args.command == 'network':
            groups = []
            for path in (wos, scopus):
                with path.open(encoding='utf-8-sig', newline='') as file:
                    for row in csv.DictReader(file):
                        names = [' '.join(n.replace('.', '').replace(',', ' ').split()) for n in row['Authors'].split(';')]
                        names = sorted(set(n for n in names if n))
                        if len(names) > 1:
                            groups.append(names)
            graph = build_graph(groups)
            result = {'method': 'legacy combined author records, no publication deduplication', 'nodes': len(graph), 'edges': sum(map(len,graph.values()))//2, 'pagerank': pagerank_sparse(graph)}
        elif args.command == 'validate':
            from .validation import compare_annual
            result = compare_annual(wos, scopus, args.end_year)
        else:
            papers = load_papers(wos, scopus, args.end_year)
            counts = annual_counts(papers)
            values = [p['citations'] for p in papers if p['citations'] is not None]
            if args.command == 'regression':
                result = fit_poisson(list(counts), list(counts.values()))
            else:
                result = {'papers': len(papers), 'missing_doi': sum(p['doi'] is None for p in papers), 'missing_citations': len(papers)-len(values), 'annual_counts': counts, 'citation_mean_and_bootstrap_ci': bootstrap_mean(values, seed=args.seed) if values else None}
        output = json.dumps(result, indent=2, allow_nan=False)
        if args.output:
            args.output.write_text(output+'\n', encoding='utf-8')
        else:
            print(output)
    except (OSError, ValueError, KeyError, RuntimeError) as error:
        parser.exit(2, f'error: {error}\n')


if __name__ == '__main__':
    main()
