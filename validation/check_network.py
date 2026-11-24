import csv
import json
import math
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from analysis.coauthorship import load_network
from analysis.network import pagerank_sparse


def main():
    folder = Path('New Publications/Co-authorship Network')
    wos, scopus = folder/'WoS.csv', folder/'Scopus.csv'
    graph = load_network(wos, scopus)
    edges = {(a,b):w for a, neighbors in graph.items() for b,w in neighbors.items() if a<b}
    rank = pagerank_sparse(graph)
    with TemporaryDirectory() as tmp:
        prefix = str(Path(tmp)/'reference')
        subprocess.run(['Rscript','validation/network_reference.R',str(wos),str(scopus),prefix],check=True)
        with open(prefix+'-edges.csv',encoding='utf-8') as file:
            reference = {tuple(sorted((r['from'],r['to']))):int(r['weight']) for r in csv.DictReader(file)}
        with open(prefix+'-rank.csv',encoding='utf-8') as file:
            r_rank = {r['author']:float(r['rank']) for r in csv.DictReader(file)}
    same_nodes = rank.keys() == r_rank.keys()
    error = max((abs(rank[n]-r_rank[n]) for n in rank),default=0) if same_nodes else None
    corrected = load_network(wos,scopus,deduplicate=True)
    report = {'nodes':len(graph),'edges':len(edges),'edge_weights_match':edges==reference,
              'nodes_match':same_nodes,'max_rank_error':error,
              'deduplicated_nodes':len(corrected),'deduplicated_edges':sum(map(len,corrected.values()))//2}
    Path('network-validation.json').write_text(json.dumps(report,indent=2)+'\n')
    if edges != reference or not same_nodes or error is None or error > 1e-8:
        raise SystemExit('Network reference mismatch')


if __name__ == '__main__':
    main()
