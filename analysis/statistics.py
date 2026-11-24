import numpy as np


def bootstrap_mean(values, n_samples=2000, seed=42, batch_size=128):
    values = np.asarray(values, dtype=float)
    if values.ndim != 1 or not values.size or not np.isfinite(values).all():
        raise ValueError('Expected a nonempty finite vector')
    if n_samples < 2 or batch_size < 1:
        raise ValueError('Invalid sample or batch size')
    rng = np.random.default_rng(seed)
    means = np.empty(n_samples)
    for start in range(0, n_samples, batch_size):
        end = min(start + batch_size, n_samples)
        indices = rng.integers(values.size, size=(end-start, values.size))
        means[start:end] = values[indices].mean(axis=1)
    low, high = np.quantile(means, [.025, .975])
    return float(values.mean()), float(low), float(high)


def permutation_test(a, b, n_samples=4999, seed=42):
    a, b = np.asarray(a, dtype=float), np.asarray(b, dtype=float)
    if any(x.ndim != 1 or not x.size or not np.isfinite(x).all() for x in (a,b)) or n_samples < 1:
        raise ValueError('Invalid observations or sample count')
    observed = float(a.mean()-b.mean())
    pooled = np.concatenate((a,b))
    rng = np.random.default_rng(seed)
    extreme = 0
    for _ in range(n_samples):
        sample = rng.permutation(pooled)
        diff = sample[:a.size].mean()-sample[a.size:].mean()
        extreme += abs(diff) >= abs(observed)-1e-12
    return observed, (int(extreme)+1)/(n_samples+1)
