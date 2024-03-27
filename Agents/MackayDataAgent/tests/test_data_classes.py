#All tests releated to data_classes module
import pytest

from data_classes.calculations import retire, algebra_sum
from data_classes.ts_data_classes import TimeSeriesInstance, parse_incomplete_time

def test_calculations_sum_by_time():
    testts = TimeSeriesInstance(src_iri='1', times=[2015,2020,2020], values=[0.06,0.43,0.82])
    out = algebra_sum(testts)
    assert out.times == [parse_incomplete_time(str(t)) for t in [2015,2020]]
    assert out.values == [0.06,1.25]

def test_calculations_retire():
    testts = TimeSeriesInstance(src_iri='1', times=[2015,2020,2025], values=[0.06,0.43,1.005])
    out = retire(testts)
    assert out.times == [ parse_incomplete_time(str(t)) for t in [2015, 2020, 2025, 2030, 2035, 2040, 2045]]
    assert out.values == [0.06, 0.43, 0.82, 0.82, 0.76, 0.39, 0.0]
