from copcalculationagent.kg_operations.tsclient import TSClient
from copcalculationagent.kg_operations.kgclient import KGClient
from copcalculationagent.datamodel.data import DATACLASS, TIME_FORMAT

QUERY_ENDPOINT= "http://localhost:3846/blazegraph/namespace/ts_example/sparql"
a = KGClient(QUERY_ENDPOINT, QUERY_ENDPOINT)


ts_client = TSClient(kg_client= a)   
dates = ['2020-04-01T12:00:00.000Z', "2020-03-01T12:00:00.000Z", "2020-02-01T12:00:00.000Z"]
dataIRI = "http://statistics.data.gov.uk/id/statistical-geography/Test_001"
values = [1,2,3]
ts = TSClient.create_timeseries(dates, [dataIRI], [values])

with ts_client.connect() as conn:
    # Initialise time series in Blazegraph and PostgreSQL
    ts_client.tsclient.initTimeSeries([dataIRI], [DATACLASS], TIME_FORMAT, conn)
    # Add test time series data
    ts_client.tsclient.addTimeSeriesData(ts, conn)