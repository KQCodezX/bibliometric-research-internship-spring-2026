import argparse
import json
from .preprocessing import load_papers, annual_counts
from .statistics import bootstrap_mean

parser = argparse.ArgumentParser()
parser.add_argument('wos')
parser.add_argument('scopus')
parser.add_argument('--end-year', type=int, default=2025)
args = parser.parse_args()
papers = load_papers(args.wos, args.scopus, args.end_year)
values = [p['citations'] for p in papers if p['citations'] is not None]
print(json.dumps({'papers': len(papers), 'missing_doi': sum(p['doi'] is None for p in papers), 'missing_citations': len(papers)-len(values), 'annual_counts': annual_counts(papers), 'citation_mean_and_bootstrap_ci': bootstrap_mean(values) if values else None}, indent=2))
