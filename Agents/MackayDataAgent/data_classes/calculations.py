'''
This module contains custom functions to modify downloaded timeseries
'''
from data_classes.ts_data_classes import *
from collections import Counter


def algebra_sum(ts: TimeSeriesInstance) -> TimeSeriesInstance:
    sum_values = Counter()
    for t, v in zip(ts.times, ts.values):
        if type(v) == str:
            v = float(v)
        sum_values[t] += v
    sk, sv = zip(*sum_values.items())
    return TimeSeriesInstance(src_iri=ts.src_iri, times=list(sk), values=list(sv))


def retire(ts: TimeSeriesInstance, lifespan=20, predict_end=2050, interval=5) -> TimeSeriesInstance:
    # From a time step, remove
    ts_years =    [t.year for t in ts.times]
    ts_end = min(predict_end, ts_years[-1]+lifespan) # All will be retired (value 0) after a lifespan from last data
    #start to nearest interval
    start_preict = ts_years[-1]//interval*interval+interval
    ts_predict = [t for t in range(start_preict,  ts_end+interval, interval)]
    padded_values = ts.values
    padded_values.extend([ ts.values[-1] for _ in ts_predict])
    ts_years.extend(ts_predict)
    t_to_v = {t:v for t,v in zip(ts_years,padded_values)}
    ts_retire = [t-lifespan for t in ts_years]
    retire_vs = [  t_to_v[t]  if ts_retire[i] not in t_to_v else round(t_to_v[t] - t_to_v[ts_retire[i]],2) for i,t in enumerate(ts_years)]
    return TimeSeriesInstance(src_iri=ts.src_iri, times=ts_years, values=retire_vs)