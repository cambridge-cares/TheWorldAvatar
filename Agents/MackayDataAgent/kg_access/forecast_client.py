#This serves as a wrapper to call forcast Agent
from pyderivationagent.data_model.iris import ONTODERIVATION_DERIVATIONWITHTIMESERIES
from pyderivationagent.kg_operations import PyDerivationClient
from data_types.ts_data_classes import *
from pyderivationagent.kg_operations import PySparqlClient
DELETE_STR = "DELETE WHERE"
INSERT_STR = "INSERT DATA"
FORECAST_META_BASE_IRI = "https://www.theworldavatar.com/test/"
META_UPDATE_QUERY = 'prefix: <'+ FORECAST_META_BASE_IRI+''' >
prefix owl:    <http://www.w3.org/2002/07/owl#>
prefix xsd:    <http://www.w3.org/2001/XMLSchema#>
prefix rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs:   <http://www.w3.org/2000/01/rdf-schema#>
prefix om:     <http://www.ontology-of-units-of-measure.org/resource/om-2/>
prefix time:   <http://www.w3.org/2006/time#>
prefix ts:     <https://www.theworldavatar.com/kg/ontotimeseries/>
prefix deriv:	<https://www.theworldavatar.com/kg/derivation/>
prefix disp:	<https://www.theworldavatar.com/kg/ontodispersion/>
prefix dh:	    <https://www.theworldavatar.com/kg/ontoheatnetwork/>
prefix ems:    <https://www.theworldavatar.com/kg/ontoems/>
{action} {{
<{ts_iri}> rdf:type owl:Thing.
:ForecastingModel_{name} rdf:type ts:ForecastingModel.
  :ForecastingModel_{name}  rdfs:label {model} .
  :ForecastingModel_{name}  ts:scaleData "false"^^xsd:boolean .
:OptimisationInterval_{name} rdf:type time:Interval .
 :OptimisationInterval_{name}   time:hasBeginning :OptimisationStartInstant_{name}.
 :OptimisationInterval_{name}   time:hasEnd :OptimisationEndInstant_{name} .
:OptimisationStartInstant_{name} rdf:type time:Instant .
 :OptimisationStartInstant_{name}   time:inTimePosition :TimePosition_{name}_1 .
:OptimisationEndInstant_{name} rdf:type time:Instant .
  :OptimisationEndInstant_{name}  time:inTimePosition :TimePosition_{name}_2 .
:TimePosition_{name}_1 rdf:type time:TimePosition .
  :TimePosition_{name}_1  time:hasTRS <http://dbpedia.org/resource/Unix_time> .
  :TimePosition_{name}_1  time:numericPosition {start}.
:TimePosition_{name}_2 rdf:type time:TimePosition .
 :TimePosition_{name}_2   time:hasTRS <http://dbpedia.org/resource/Unix_time> .
 :TimePosition_{name}_2   time:numericPosition {end} .

:Frequency_{name} rdf:type ts:Frequency .
:Frequency_{name}    time:numericDuration {frequency} .
 :Frequency_{name}   time:unitType {unit} .
  :Frequency_{name}  ts:resampleData "false"^^xsd:boolean .

:Duration_{name} rdf:type time:Duration .
:Duration_{name}    time:numericDuration {duration}.
 :Duration_{name}   time:unitType {unit} .

}}
'''

# Wrapper for using a forecast Agent
# Requires: a running copy of ForecastAgent with a URL, a remote/local KG access info for R/W agent meta information, and the IRI of this forecast agent in the KG
class ForcastAgentClient:
    def __init__(self, agent_url:str, agent_iri:str, kg_info:KgAccessInfo):
        self.deriv_client = PyDerivationClient(agent_url, kg_info.endpoint,kg_info.endpoint,kg_user=kg_info.user,kg_password=kg_info.password)
        self.forcastagent_iri = agent_iri
        self.sparql_client = PySparqlClient(kg_info.endpoint,kg_info.endpoint,kg_user=kg_info.user,kg_password=kg_info.password)

    def run(self, input_iri_list) -> str:
        derivation = self.deriv_client.createSyncDerivationForNewInfo(self.forcastagent_iri, input_iri_list, ONTODERIVATION_DERIVATIONWITHTIMESERIES)
        derivation_iri = derivation.getIri()
        # Update existing forecast derivation
        self.deriv_client.unifiedUpdateDerivation(derivation_iri)
        return derivation_iri

    def update_forcast_meta(self, fmeta:ForecastMeta) -> list:
        self._delete_forecast_meta(fmeta.iri, fmeta.name)
        self._insert_forecast_meta(fmeta)
        input_list = [ fmeta.iri,
                       FORECAST_META_BASE_IRI+"ForecastingModel_{}".format(fmeta.name),
                       FORECAST_META_BASE_IRI+"Frequency_{}".format(fmeta.name),
                       FORECAST_META_BASE_IRI+"OptimisationInterval{}".format(fmeta.name),
                       FORECAST_META_BASE_IRI+"Duration{}".format(fmeta.name)
                       ]
        return input_list

    def _delete_forecast_meta(self, src_iri, name):
        delete_str = META_UPDATE_QUERY.format(action=DELETE_STR, name=name, ts_iri=src_iri, model = '?m', start='?s',end='?e',frequency='?f', unit='?u',duration="?d")
        r = self.sparql_client.performUpdate(delete_str)




    def _insert_forecast_meta(self, fmeta:ForecastMeta):
        update_str = META_UPDATE_QUERY.format(action=INSERT_STR,name=fmeta.name,ts_iri=fmeta.iri,model=fmeta.model,start=fmeta.start,end=fmeta.end,frequency = fmeta.frequency, unit=fmeta.unit_frequency,duration=fmeta.duration)
        r = self.sparql_client.performUpdate(update_str)
