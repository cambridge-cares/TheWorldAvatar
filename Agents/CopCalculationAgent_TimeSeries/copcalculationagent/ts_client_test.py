from copcalculationagent.kg_operations.tsclient import TSClient
from copcalculationagent.kg_operations.kgclient import KGClient
from copcalculationagent.datamodel.data import DATACLASS, TIME_FORMAT
from pyderivationagent.conf import config_derivation_agent

from copcalculationagent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from copcalculationagent.agent import COPCalculationAgent

import datetime as dt

def instantiate_data(ts_client):

    #dates = [(dt.datetime.now() - dt.timedelta(minutes=30*i)).strftime(TIME_FORMAT) for i in range(3)]
    dates = [
        "2020-01-01T12:00:00Z",
        "2020-02-01T12:00:00Z",
        "2020-03-01T12:00:00Z", ]

    code = '008'
    min_cop_iri = "http://statistics.data.gov.uk/id/statistical-geography/Test_min_" + code
    mean_cop_iri = "http://statistics.data.gov.uk/id/statistical-geography/Test_mean_" + code
    max_cop_iri = "http://statistics.data.gov.uk/id/statistical-geography/Test_max_" + code

    minvalues = [1,2,3]
    meanvalues = [4,5,6]
    maxvalues = [7,8,9]

    dataIRIs = [min_cop_iri,mean_cop_iri,max_cop_iri]
    values = [minvalues, meanvalues, maxvalues]

    # Create time series from test data                        
    ts = TSClient.create_timeseries(dates, dataIRIs, values)
    with ts_client.connect() as conn:
        # Initialise time series in Blazegraph and PostgreSQL
        ts_client.tsclient.initTimeSeries(dataIRIs, [DATACLASS]*len(dataIRIs), TIME_FORMAT, conn)
        # Add test time series data
        ts_client.tsclient.addTimeSeriesData(ts, conn)

def query_data(data_iri, ts_client):
    with ts_client.connect() as conn:
        ts = ts_client.tsclient.getTimeSeries(data_iri, conn)
    dates = [d.toString() for d in ts.getTimes()]
    values = [v for v in ts.getValues(data_iri[0])]
    print(ts)
    print(dates)
    print(values)

# Initialize Config
agent_config = config_derivation_agent(env_file='./agent.env.example')

# Initialize Agent
agent = COPCalculationAgent(
    # Settings read from environment variables (.env file, docker-compose)
    register_agent=agent_config.REGISTER_AGENT,
    agent_iri=agent_config.ONTOAGENT_SERVICE_IRI, 
    time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
    derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
    agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
    # Settings read from Stack Clients
    kg_url=QUERY_ENDPOINT,
    kg_update_url=UPDATE_ENDPOINT,      
    # Miscellaneous settings
    logger_name='dev',
    max_thread_monitor_async_derivations=1
)
  
# Initialise TS client
ts_client = TSClient(kg_client=agent.sparql_client)

query_data(["http://statistics.data.gov.uk/id/statistical-geography/Test_mean_008",],ts_client)

#instantiate_data(ts_client=ts_client)

