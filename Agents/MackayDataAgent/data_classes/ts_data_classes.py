#Data object prototypes for easy management
from dataclasses import dataclass,field
import datetime
from dateutil.parser import parser, parse
import time


DEFAULT_TIME = datetime.datetime.strptime('01/01/01 00:00:00', '%m/%d/%y %H:%M:%S')
TSTR_FORMATS = {'Instant': '%Y-%m-%dT%H:%M:%SZ', 'LocalDate':'%Y-%m-%d'}


def parse_incomplete_time(input_tstr:str)-> datetime.datetime:
    return parse(input_tstr, default=DEFAULT_TIME)

def parse_time_to_unix(input_tstr:str) -> float:
    dt = parse_incomplete_time(input_tstr)
    return time.mktime(dt.timetuple())+dt.utcoffset().total_seconds() #Add a local timezone offset, as time in rdb is not read with timezone somehow by TSClient

#Currently forcasting only allows time.Instant, instead of date
# convert date or other time to datetimestring to ensure Forecast works
def parse_time_to_format(input_time:str, time_class:str) -> str:
    output_format = TSTR_FORMATS[time_class]
    dt = parse_incomplete_time(input_time)
    return dt.strftime(output_format)

#Time series data object as required by KG instaniate
@dataclass(frozen=True)
class TimeSeriesMeta:
    time_unit: str
    src_iri: str = None


@dataclass(frozen=True)
class TimeSeriesInstance:
    times: list
    values: list
    src_iri: str = None


@dataclass
class ForecastMeta:
    name: str
    iri:str
    duration: float
    start_str:str
    end_str:str
    start:int = field(init=False)
    end:int = field(init=False)
    frequency: float
    unit_frequency: str
    model = "Prophet"

    def __post_init__(self):
        if self.unit_frequency == 'year':
            self.unit_frequency = 'unitDay'
            self.frequency = self.frequency * 365
            self.duration = self.duration * 365
            self.start = parse_time_to_unix(self.start_str)
            self.end = parse_time_to_unix(self.end_str)


@dataclass
class KgAccessInfo:
    endpoint:str
    password: str = None
    user: str = None


PropertiesFileProtytype = {
    "db.url":"localhost:5432",
    "db.user":"postgres",
    "db.password":"111111",
    "sparql.query.endpoint":"",
    "sparql.update.endpoint":""
}
