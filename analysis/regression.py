import numpy as np
from scipy.optimize import minimize
from scipy.special import gammaln


def fit_poisson(years, counts):
    years, counts = np.asarray(years, dtype=float), np.asarray(counts, dtype=float)
    if (years.ndim != 1 or years.shape != counts.shape or years.size < 3
            or not np.isfinite(years).all() or not np.isfinite(counts).all()
            or np.any(counts < 0) or np.any(counts != np.floor(counts))
            or np.unique(years).size != years.size or np.count_nonzero(counts) < 2):
        raise ValueError('Need distinct years and valid counts with at least two positive observations')
    center = years.mean()
    scale = years.std()
    X = np.column_stack((np.ones(years.size), (years-center)/scale))
    def objective(beta):
        eta = X @ beta
        mu = np.exp(eta)
        return float(np.sum(mu-counts*eta+gammaln(counts+1))), X.T @ (mu-counts)
    result = minimize(objective, [np.log(counts.mean()), 0], jac=True, method='L-BFGS-B', bounds=[(-30,30),(-30,30)])
    if not result.success or np.any(np.abs(result.x) >= 29.999):
        raise RuntimeError('Poisson fit failed or reached numerical bounds')
    mu = np.exp(X @ result.x)
    covariance = np.linalg.inv(X.T @ (mu[:,None]*X))
    slope = result.x[1]/scale
    se = np.sqrt(covariance[1,1])/scale
    return {'center_year': float(center), 'intercept': float(result.x[0]), 'annual_rate_ratio': float(np.exp(slope)), 'rate_ratio_ci': np.exp([slope-1.96*se,slope+1.96*se]).tolist(), 'pearson_dispersion': float(np.sum((counts-mu)**2/mu)/(years.size-2))}
