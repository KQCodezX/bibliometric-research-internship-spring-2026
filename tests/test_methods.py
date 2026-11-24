import unittest
import tempfile
from pathlib import Path
import numpy as np
from analysis.preprocessing import load_papers, annual_counts
from analysis.statistics import bootstrap_mean, permutation_test
from analysis.regression import fit_poisson
from analysis.network import build_graph, pagerank, pagerank_sparse

class MethodsTest(unittest.TestCase):
    def test_ingestion(self):
        with tempfile.TemporaryDirectory() as folder:
            w, s = Path(folder)/'w.csv', Path(folder)/'s.csv'
            w.write_text('Publication Year,"Times Cited, All Databases",DOI Link\n2020,5,10.1/a\n2020,2,\n')
            s.write_text('Year,Cited by,DOI Link\n2020,9,https://doi.org/10.1/A\n2022,,\n2026,1,10.1/b\n')
            papers = load_papers(w,s)
            self.assertEqual(len(papers),3)
            self.assertEqual(papers[0]['citations'],5)
            self.assertEqual(annual_counts(papers),{2020:2,2021:0,2022:1})
    def test_bootstrap(self):
        self.assertEqual(bootstrap_mean([4,4]),(4,4,4))
        self.assertEqual(bootstrap_mean([1,2,3]),bootstrap_mean([1,2,3]))
    def test_invalid(self):
        with self.assertRaises(ValueError): bootstrap_mean([np.nan])
        with self.assertRaises(ValueError): fit_poisson([1,2,3],[0,0,0])
    def test_permutation(self):
        self.assertEqual(permutation_test([1,1],[1,1],99),(0,1))
        self.assertLess(permutation_test([0]*8,[10]*8,999)[1],.01)
    def test_regression(self):
        result = fit_poisson([2020,2021,2022,2023],[10,20,40,80])
        self.assertAlmostEqual(result['annual_rate_ratio'],2,places=4)
    def test_flat_regression(self):
        self.assertAlmostEqual(fit_poisson([1,2,3],[10,10,10])['annual_rate_ratio'],1)
    def test_network(self):
        graph = build_graph([['a','b'],['a','b'],['b','c'],['d']])
        a,b = pagerank(graph),pagerank_sparse(graph)
        self.assertAlmostEqual(sum(b.values()),1)
        for node in a: self.assertAlmostEqual(a[node],b[node],places=10)
    def test_empty_network(self):
        self.assertEqual(pagerank_sparse({}),{})
