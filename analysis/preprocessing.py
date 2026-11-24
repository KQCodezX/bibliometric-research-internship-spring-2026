import csv
from collections import Counter

def normalize_doi(doi):
    doi = (doi or '').strip().lower()
    for prefix in ('https://doi.org/', 'http://doi.org/', 'http://dx.doi.org/', 'https://dx.doi.org/', 'doi:'):
        if doi.startswith(prefix):
            doi = doi[len(prefix):].strip()
            break
    return doi if doi.startswith('10.') and '/' in doi else None


def load_papers(wos_file, scopus_file, end_year=2025):
    papers = []
    seen = set()
    for path, year_col, cite_col in [(wos_file, 'Publication Year', 'Times Cited, All Databases'), (scopus_file, 'Year', 'Cited by')]:
        with open(path, encoding='utf-8-sig', newline='') as file:
            reader = csv.DictReader(file)
            if not {year_col, cite_col, 'DOI Link'} <= set(reader.fieldnames or []):
                raise ValueError(f'Missing columns in {path}')
            for row in reader:
                year = int(row[year_col])
                if year > end_year:
                    continue
                doi = normalize_doi(row['DOI Link'])
                if doi and doi in seen:
                    continue
                citations = int(row[cite_col]) if row[cite_col].strip() else None
                if citations is not None and citations < 0:
                    raise ValueError('Negative citation count')
                if doi:
                    seen.add(doi)
                papers.append({'year': year, 'doi': doi, 'citations': citations})
    return papers


def annual_counts(papers):
    counts = Counter(p['year'] for p in papers)
    if not counts:
        return {}
    return {year: counts[year] for year in range(min(counts), max(counts) + 1)}
