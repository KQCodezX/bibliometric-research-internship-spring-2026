import random
from statistics import median
from time import perf_counter
from analysis.network import build_graph, pagerank, pagerank_sparse


def main():
    rng = random.Random(42)
    for n in (100,1000,5000):
        graph = build_graph([[str(i)] for i in range(n)] + [list(map(str,rng.sample(range(n),4))) for _ in range(n*3)])
        results = []
        for method in (pagerank,pagerank_sparse):
            method(graph)
            times = []
            for _ in range(3):
                start = perf_counter()
                rank = method(graph)
                times.append(perf_counter()-start)
            results.append((median(times),rank))
        error = max(abs(results[0][1][i]-results[1][1][i]) for i in graph)
        print(n, 'nodes', sum(map(len,graph.values())), 'directed edges', *(round(r[0]*1000,3) for r in results), 'ms (reference, sparse)', 'max error',error)

if __name__ == '__main__': main()
