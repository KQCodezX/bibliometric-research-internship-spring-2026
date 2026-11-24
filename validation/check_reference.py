import csv
import json
import math
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from analysis.validation import compare_annual, legacy_citations

def main():
    folder = Path('New Publications/Annual')
    wos, scopus = folder/'WoS.csv', folder/'Scopus.csv'
    with TemporaryDirectory() as tmp:
        output = Path(tmp)/'reference.csv'
        subprocess.run(['Rscript','validation/reference.R',str(wos),str(scopus),str(output)],check=True)
        with output.open() as file:
            rows = list(csv.DictReader(file))
            reference = {int(r['year']):int(r['count']) for r in rows}
    citations = legacy_citations(wos,scopus)
    citation_match = citations.keys() == reference.keys()
    for row in rows:
        actual = citations[int(row['year'])]
        expected = None if row['mean'] in ('NA','NaN','') else float(row['mean'])
        citation_match &= actual['total'] == float(row['total']) and (
            actual['mean'] is None if expected is None else actual['mean'] is not None and math.isclose(actual['mean'],expected,rel_tol=1e-10,abs_tol=1e-10))
    report = compare_annual(wos,scopus)
    report['legacy_citations_match_r'] = bool(citation_match)
    report['legacy_citations'] = citations
    report['r_counts'] = reference
    report['legacy_matches_r'] = reference == report['legacy_counts']
    Path('validation-report.json').write_text(json.dumps(report,indent=2)+'\n')
    if not report['legacy_matches_r'] or not citation_match:
        raise SystemExit('Legacy annual counts differ from R reference')

if __name__ == '__main__':
    main()
