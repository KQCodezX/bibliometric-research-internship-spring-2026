import csv
from .preprocessing import normalize_doi
from .network import build_graph


def load_network(wos, scopus, deduplicate=False):
    groups = []
    seen = set()
    for path in (wos, scopus):
        with open(path, encoding='utf-8-sig', newline='') as file:
            for row in csv.DictReader(file):
                doi = normalize_doi(row.get('DOI Link'))
                if deduplicate and doi:
                    if doi in seen:
                        continue
                    seen.add(doi)
                names = [' '.join(n.replace('.', '').replace(',', ' ').split())
                         for n in row['Authors'].split(';')]
                names = sorted(set(n for n in names if n))
                if len(names) > 1:
                    groups.append(names)
    return build_graph(groups)
