# Demographic Representation in Brain-Computer Interface Studies for Motor Recovery and Pain-Related Outcomes
### 🧠 ThinkNeuro Bibliometric Research Internship (Spring 2026)

🎨 **Project Status:** In progress for subsequent publications / Presented at **The National Premier Undergraduate Research Conference at Columbia University**.

---

## 📌 Project Overview
This repository contains the complete replication data, analysis pipelines, and publication-ready visualizations for our bibliometric study investigating participant demographic representation within Brain-Computer Interface (BCI) literature. 

This research maps out historical trends in clinical sample populations (spanning age, race, and sex/gender metrics) and analyzes how they align with global burdens of neurological and motor impairments. Additionally, it evaluates the structural landscape of the field via keyword co-occurrence and co-authorship networks.

---

## Repository structure

- `Spring 2026 Research Intensive Data Figures/`: original demographic analyses and figures.
- `New Publications/`: annual publication trends, co-authorship and keyword analyses.
- `analysis/`: Python preprocessing, statistical inference, Poisson regression and weighted graph algorithms.
- `benchmarks/`: reproducible synthetic performance experiments.
- `tests/`: automated numerical and ingestion tests.
- [Computational methodology](docs/methodology.md): assumptions and limitations.

## Running the Python analysis

Requires Python 3.10 or later.

```sh
python -m pip install .
python -m analysis annual
python -m analysis regression
python -m analysis network
python -m analysis validate
python -m unittest discover -s tests -v
python -m benchmarks.benchmark_network
python -m benchmarks.benchmark_statistics
```

The Python extension complements the original R analyses. Its numerical methods have been tested on synthetic inputs; validation against the original datasets and R results remains pending. Regression and graph methods are exposed through CLI subcommands. Run `python -m validation.check_reference` with R installed to compare legacy annual counts against an independent R reference. Network analysis currently follows the legacy combined-record methodology without publication deduplication. Full R parity is not yet established.
