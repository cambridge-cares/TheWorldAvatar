from data_classes.ts_data_classes import *
url = 'http://localhost:5001/ForecastingAgent'
iri = 'https://www.theworldavatar.com/resource/agents/Service__ForecastingAgent/Test'
ep = 'http://localhost:9999/blazegraph/namespace/country/sparql'

ts_iri = 'https://www.theworldavatar.com/kg/country/Number_773521f9-528e-40fc-9bcb-4218e49f40e8'
name = '1'

KG = KgAccessInfo(endpoint=ep)
from kg_access.forecast_client import ForcastAgentClient
#agent = MackayDataAgent()
#agent.instantiate_timeseries()
#agent.add_timeseries()
agent = ForcastAgentClient(url,iri,KG)
#agent.delete_insert_forecast_meta(ts_iri, name)