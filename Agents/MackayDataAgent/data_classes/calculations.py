from data_classes.ts_data_classes import *
from collections import Counter
def sum_by_time(ts: TimeSeriesInstance)-> TimeSeriesInstance:
    sum_values = Counter()
    for t,v in zip(ts.times,ts.values):
        if type(v) == str:
            v = float(v)
        sum_values[t]+=v
    sk, sv = zip(*sum_values.items())
    return TimeSeriesInstance(src_iri=ts.src_iri,times=sk, values=sv)

#print(sum_by_time(TimeSeriesInstance(src_iri='',times=['2022','2022','2023'],values=['3','4','5'])))