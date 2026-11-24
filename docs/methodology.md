# Computational methods

Install with `python -m pip install .`. Run `python -m unittest discover -s tests -v`, `python -m benchmarks.benchmark_network`, and `python -m benchmarks.benchmark_statistics` from the repository root.

The analysis CLI accepts WoS and Scopus CSV paths: `python -m analysis WOS.csv SCOPUS.csv`. The original R analyses remain the research record. This extension has not been validated on the original datasets.

DOI matching prioritizes WoS; missing DOIs remain separate and may duplicate studies. Citation counts across databases and publication years are not directly comparable. Bootstrap intervals describe the pooled sample under independent sampling. Permutation tests assume exchangeability under the null. Poisson regression models annual counts with a log link, using centered and scaled years, maximum likelihood and inverse Fisher information. Wald intervals assume the model is appropriate; Pearson dispersion is a diagnostic, not a correction. Missing years should be treated as zero only when search coverage is complete. Author identity resolution and unique-paper grouping are required before constructing a network.

PageRank uses weighted edges and redistributes dangling mass uniformly. Both implementations use an L1 stopping criterion. Sparse storage scales with nodes and edges rather than a dense square matrix. Benchmark timings include matrix construction but exclude graph generation. Synthetic benchmark results are not evidence of real-data performance.

C++, DuckDB and Parquet are deferred until real workloads justify them.
