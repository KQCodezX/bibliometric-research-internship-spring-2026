import csv
from collections import Counter
from .preprocessing import load_papers, annual_counts


def legacy_annual(wos, scopus, end_year=2025):
    seen = set()
    counts = Counter()
    for path, column in [(wos,'Publication Year'),(scopus,'Year')]:
        with open(path, encoding='utf-8-sig', newline='') as file:
            for row in csv.DictReader(file):
                year = int(row[column])
                doi = row['DOI Link'] or None
                if year <= end_year and doi not in seen:
                    seen.add(doi)
                    counts[year] += 1
    return dict(sorted(counts.items()))


def compare_annual(wos, scopus, end_year=2025):
    legacy = legacy_annual(wos, scopus, end_year)
    updated = annual_counts(load_papers(wos, scopus, end_year))
    years = sorted(set(legacy) | set(updated))
    return {'legacy_counts': legacy, 'updated_counts': updated,
            'difference': {y: updated.get(y,0)-legacy.get(y,0) for y in years},
            'note': 'Legacy raw DOI grouping versus normalized DOI grouping; missing DOI records retained in updated method.'}
