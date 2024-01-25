'''
This module contains Data classes for timeseries data and utility functions of time format conversion
'''

from dataclasses import dataclass, field
import datetime
from dateutil.parser import parser, parse
import time
import importlib
from dateutil.relativedelta import *
from typing import List

DEFAULT_TIME = datetime.datetime.strptime('01/01/01 00:00:00+0800', '%m/%d/%y %H:%M:%S%z')
TSTR_FORMATS = {'Instant': '%Y-%m-%dT%H:%M:%SZ', 'LocalDate': '%Y-%m-%d'}


# Time series data object as required by KG instaniate
@dataclass(frozen=True)
class TimeSeriesMeta:
    time_unit: str
    src_iri: str = None


@dataclass
class TimeSeriesInstance:
    times: list
    values: list
    src_iri: str = None

    def __post_init__(self):  # Convert times to datetime objects
        if type(self.times[0]) == str:
            self.times = [parse_incomplete_time(t) for t in self.times]
        elif type(self.times[0]) == float or type(self.times[0]) == int:
            self.times = [parse_incomplete_time(str(t)) for t in self.times]
        elif type(self.times[0]) != datetime.datetime:
            print(self.times)
            raise Exception('TimeSeriesInstance time list has to be either int, float, time str or datetime')


@dataclass
class ForecastMeta:
    name: str
    iri: str
    duration: float
    start_dt: datetime.datetime
    end_dt: datetime.datetime
    start: float = field(init=False)
    end: float = field(init=False)
    frequency: float
    unit_frequency: str
    unit_frequency_kg: str = field(init=False)
    model = "Prophet"

    def __post_init__(self):
        if self.unit_frequency == 'year':
            self.unit_frequency_kg = 'time:unitDay'
            self.frequency = self.frequency * 365
        elif self.unit_frequency == 'month':
            self.unit_frequency_kg = 'time:unitDay'
            self.frequency = self.frequency * 30
        self.start = parse_time_to_unix(self.start_dt)
        self.end = parse_time_to_unix(self.end_dt)


@dataclass
class KgAccessInfo:
    endpoint: str
    password: str = None
    user: str = None


PropertiesFileProtytype = {
    "db.url": "localhost:5432",
    "db.user": "postgres",
    "db.password": "111111",
    "sparql.query.endpoint": "",
    "sparql.update.endpoint": ""
}


def parse_incomplete_time(input_tstr: str) -> datetime.datetime:
    return parse(input_tstr, default=DEFAULT_TIME)


def parse_time_to_unix(dt: datetime.datetime) -> float:
    return time.mktime(
        dt.timetuple()) + dt.utcoffset().total_seconds()  # Add a local timezone offset, as time in rdb is not read with timezone somehow by TSClient


# Currently forcasting only allows time.Instant, instead of date
# convert date or other time to datetimestring to ensure Forecast works
def parse_time_to_format(input_time: datetime.datetime, time_class: str = "Instant") -> str:
    output_format = TSTR_FORMATS[time_class]
    return input_time.strftime(output_format)


def get_next_timeinstant_by_unit(unitname: str, last_time: datetime.datetime):
    try:
        diff = relativedelta(**{unitname + 's': 1})
        next_time = last_time + diff
        return next_time
    except AttributeError:
        print(
            'supplied timeunit does not exist in python datetime attributes. Has to be either year, month, day, hour, minute or second')


def get_time_list_within_bound(unit: str, last_time: datetime.datetime, end_time: datetime.datetime):
    # Allow other time type in future
    timevalues = []
    next_time = last_time
    while next_time < end_time:
        next_time = get_next_timeinstant_by_unit(unit, next_time)
        timevalues.append(next_time)
    return timevalues


def get_padded_TSInstance(src_iri: str, unit: str, last_time: datetime.datetime, end_time: datetime.datetime):
    # Allow other time type in future
    times = get_time_list_within_bound(unit, last_time, end_time)
    return TimeSeriesInstance(src_iri=src_iri, times=times, values=[0.0 for _ in range(len(times))])


def get_duration_in_days(start_dt: datetime.datetime, end_dt: datetime.datetime) -> float:
    diff = end_dt - start_dt
    return float(diff.days) + 365
