from collections import Counter, defaultdict
from itertools import combinations

def build_graph(groups):
    graph = defaultdict(Counter)
    for group in groups:
        names = sorted(set(name.strip() for name in group if name.strip()))
        for name in names:
            graph[name]
        for a, b in combinations(names, 2):
            graph[a][b] += 1
            graph[b][a] += 1
    return dict(graph)


def pagerank(graph, damping=.85, tol=1e-10, max_iter=1000):
    if not graph:
        return {}
    if not 0 < damping < 1 or tol <= 0 or max_iter < 1:
        raise ValueError('Invalid PageRank parameters')
    n = len(graph)
    rank = dict.fromkeys(graph, 1 / n)
    weights = {node: sum(edges.values()) for node, edges in graph.items()}
    for _ in range(max_iter):
        dangling = sum(rank[node] for node in graph if not weights[node])
        new_rank = dict.fromkeys(graph, (1 - damping + damping * dangling) / n)
        for node, edges in graph.items():
            for other, weight in edges.items():
                new_rank[other] += damping * rank[node] * weight / weights[node]
        if sum(abs(new_rank[node] - rank[node]) for node in graph) < tol:
            return new_rank
        rank = new_rank
    raise RuntimeError('PageRank did not converge')


def pagerank_sparse(graph, damping=.85, tol=1e-10, max_iter=1000):
    import numpy as np
    from scipy.sparse import csr_matrix
    if not graph:
        return {}
    if not 0 < damping < 1 or tol <= 0 or max_iter < 1:
        raise ValueError('Invalid PageRank parameters')
    nodes = list(graph)
    index = {node: i for i, node in enumerate(nodes)}
    rows, cols, data = [], [], []
    for node, edges in graph.items():
        if any(w <= 0 for w in edges.values()):
            raise ValueError('Expected positive edge weights')
        total = sum(edges.values())
        for other, weight in edges.items():
            rows.append(index[other])
            cols.append(index[node])
            data.append(weight/total)
    n = len(nodes)
    matrix = csr_matrix((data, (rows, cols)), shape=(n,n))
    dangling = np.asarray(matrix.sum(axis=0)).ravel() == 0
    rank = np.full(n, 1/n)
    for _ in range(max_iter):
        new_rank = damping*(matrix @ rank)+(1-damping+damping*rank[dangling].sum())/n
        if np.abs(new_rank-rank).sum() < tol:
            return dict(zip(nodes, new_rank.tolist()))
        rank = new_rank
    raise RuntimeError('PageRank did not converge')
