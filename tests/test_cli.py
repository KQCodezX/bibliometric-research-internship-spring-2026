import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

class CliTests(unittest.TestCase):
    def test_commands(self):
        with tempfile.TemporaryDirectory() as tmp:
            w,s = Path(tmp)/'w.csv',Path(tmp)/'s.csv'
            w.write_text('Publication Year,"Times Cited, All Databases",DOI Link,Authors\n2020,5,10.1/a,A;B\n2021,6,10.1/b,B;C\n2022,7,10.1/c,A;C\n')
            s.write_text('Year,Cited by,DOI Link,Authors\n2020,5,10.1/a,A;B\n')
            for command in ('annual','regression','network','validate'):
                with self.subTest(command=command):
                    result = subprocess.run([sys.executable,'-m','analysis',command,'--wos',str(w),'--scopus',str(s)],capture_output=True,text=True,check=True)
                    self.assertIsInstance(json.loads(result.stdout),dict)
