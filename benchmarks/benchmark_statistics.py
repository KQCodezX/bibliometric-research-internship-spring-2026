from time import perf_counter
from statistics import median
import numpy as np
from analysis.statistics import bootstrap_mean

if __name__ == '__main__':
    rng = np.random.default_rng(42)
    for n in (100,1000,5000):
        values = rng.poisson(10,n)
        for batch in (32,128,512):
            times = []
            for _ in range(3):
                start = perf_counter()
                result = bootstrap_mean(values,batch_size=batch)
                times.append(perf_counter()-start)
            print(n,batch,round(median(times)*1000,3),result)
