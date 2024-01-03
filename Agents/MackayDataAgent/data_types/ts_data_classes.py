from dataclasses import dataclass

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

PropertiesFileProtytype = {
    "db.url":"localhost:5432",
    "db.user":"postgres",
    "db.password":"111111",
    "sparql.query.endpoint":"",
    "sparql.update.endpoint":""
}
