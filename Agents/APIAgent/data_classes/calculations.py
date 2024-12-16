'''
This module contains custom algebra functions to apply post-download to convert raw API data to TS
'''
from data_classes.ts_data_classes import *
from collections import Counter


def map_function_sum(ts: TimeSeriesInstance) -> TimeSeriesInstance:
    sum_values = Counter()
    for t, v in zip(ts.times, ts.values):
        if type(v) == str:
            v = float(v)
        sum_values[t] += v
    sk, sv = zip(*sum_values.items())
    return TimeSeriesInstance(src_iri=ts.src_iri, times=list(sk), values=list(sv))