import csv
import json
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from analysis.validation import compare_annual

def main():
    folder = Path('New Publications/Annual')
    wos, scopus = folder/'WoS.csv', folder/'Scopus.csv'
    with TemporaryDirectory() as tmp:
        output = Path(tmp)/'reference.csv'
        subprocess.run(['Rscript','validation/reference.R',str(wos),str(scopus),str(output)],check=True)
        with output.open() as file:
            reference = {int(r['year']):int(r['count']) for r in csv.DictReader(file)}
    report = compare_annual(wos,scopus)
    report['r_counts'] = reference
    report['legacy_matches_r'] = reference == report['legacy_counts']
    Path('validation-report.json').write_text(json.dumps(report,indent=2)+'\n')
    if not report['legacy_matches_r']:
        raise SystemExit('Legacy annual counts differ from R reference')

if __name__ == '__main__':
    main()
