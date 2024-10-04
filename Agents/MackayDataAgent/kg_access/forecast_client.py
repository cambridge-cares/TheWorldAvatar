'''
This module contains functions to call forecast agent client and insert/delete meta data into KG as required by forecast agent
'''
import logging

from pyderivationagent.data_model.iris import ONTODERIVATION_DERIVATIONWITHTIMESERIES
from pyderivationagent.kg_operations import PyDerivationClient
from data_classes.ts_data_classes import *
from pyderivationagent.kg_operations import PySparqlClient

DELETE_STR = "DELETE WHERE"
SELECT_STR=  "SELECT * WHERE"
INSERT_STR = "INSERT DATA"
GLOBAL_BASE = "https://www.theworldavatar.com/kg/"
META_PREFIX = 'PREFIX : <{}> ' + '''
PREFIX owl:    <http://www.w3.org/2002/07/owl#>
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
'''

META_UPDATE_QUERY = '''
{action} {{
<{ts_iri}> rdf:type owl:Thing.
:ForecastingModel_{name} rdf:type ts:ForecastingModel.
:ForecastingModel_{name}  rdfs:label {model}.
:ForecastingModel_{name}  ts:scaleData "false"^^xsd:boolean.
:OptimisationInterval_{name} rdf:type time:Interval.
 :OptimisationInterval_{name}   time:hasBeginning :OptimisationStartInstant_{name}.
 :OptimisationInterval_{name}   time:hasEnd :OptimisationEndInstant_{name}.
:OptimisationStartInstant_{name} rdf:type time:Instant.
 :OptimisationStartInstant_{name}   time:inTimePosition :TimePosition_{name}_1.
:OptimisationEndInstant_{name} rdf:type time:Instant.
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
    def __init__(self, agent_url: str, agent_iri: str, kg_info: KgAccessInfo, base_iri):
        self.deriv_client = PyDerivationClient(agent_url, kg_info.endpoint, kg_info.endpoint, kg_user=kg_info.user,
                                               kg_password=kg_info.password)
        self.forcastagent_iri = agent_iri
        self.sparql_client = PySparqlClient(kg_info.endpoint, kg_info.endpoint, kg_user=kg_info.user,
                                            kg_password=kg_info.password)
        self.base_iri = base_iri # namespace

    def call_predict(self, forecast_meta: ForecastMeta):
        predict_input = self.update_forcast_meta(
            forecast_meta)  # Meta definition of forecast instance needs to be inserted before run forecast agent
        logging.info('Forecast meta successfully inserted to KG')
        self.create_forecast(predict_input)
        logging.info('Forecast successfully created for: {}'.format(forecast_meta.name))

    def create_forecast(self, input_iri_list) -> str:
        derivation = self.deriv_client.createSyncDerivationForNewInfo(self.forcastagent_iri, input_iri_list,
                                                                      ONTODERIVATION_DERIVATIONWITHTIMESERIES)
        derivation_iri = derivation.getIri()
        # Update existing forecast derivation
        return derivation_iri

    def check_if_TS_update(self, forecast_iri):
        # check if TS is updated
        derivation_iri = self.deriv_client.getDerivationsOf([forecast_iri])
        derivation_iri = derivation_iri[forecast_iri]
        self.deriv_client.derivation_client.updateMixedAsyncDerivation(derivation_iri)
        status = self.deriv_client.derivation_client.getStatusType(derivation_iri)
        if status == 'https://www.theworldavatar.com/kg/ontoderivation/derived/Requested':
            return True# Original TS needs update
        return False


    def get_forecast_iri(self, src_iri):
        q = '''
        SELECT * where {{
        <{}> <https://www.theworldavatar.com/kg/ontotimeseries/hasForecast>	?forecast
        }}
        '''
        r = self.sparql_client.performQuery(q.format(src_iri))
        if len(r) > 0:
            return r[0]['forecast']
        else:
            return False

    def update_forcast_meta(self, fmeta: ForecastMeta) -> list:
        self._delete_forecast_meta(fmeta.iri, fmeta.name)
        self._insert_forecast_meta(fmeta)
        logging.info('Forecast meta successfully inserted')
        input_list = [fmeta.iri,
                      self.base_iri + "ForecastingModel_{}".format(fmeta.name),
                      self.base_iri  + "Frequency_{}".format(fmeta.name),
                      self.base_iri  + "OptimisationInterval_{}".format(fmeta.name),
                      self.base_iri  + "Duration_{}".format(fmeta.name)
                      ]
        return input_list

    def _delete_forecast_meta(self, src_iri, name):
        delete_str = META_PREFIX.format(self.base_iri) + META_UPDATE_QUERY.format(action=DELETE_STR, name=name, ts_iri=src_iri, model='?m',
                                                            start='?s', end='?e', frequency='?f', unit='?u',
                                                            duration="?d")
        if self.get_forecast_meta(src_iri,name):
            r = self.sparql_client.performUpdate(delete_str)
            logging.info('Old Forecast meta successfully deleted')


    def _insert_forecast_meta(self, fmeta: ForecastMeta):
        update_str = META_PREFIX.format(self.base_iri) + META_UPDATE_QUERY.format(action=INSERT_STR, name=fmeta.name, ts_iri=fmeta.iri,
                                                            model="\"" + fmeta.model + "\"", start=fmeta.start,
                                                            end=fmeta.end, frequency=fmeta.frequency,
                                                            unit=fmeta.unit_frequency_kg, duration=fmeta.duration)
        r = self.sparql_client.performUpdate(update_str)

    def get_forecast_meta(self,src_iri,name):
        search_str = META_PREFIX.format(self.base_iri) + META_UPDATE_QUERY.format(action=SELECT_STR, name=name, ts_iri=src_iri, model='?model',
                                                            start='?start', end='?end', frequency='?frequency', unit='?unit_frequency',
                                                            duration="?duration")
        r = self.sparql_client.performQuery(search_str)
        if len(r) == 1:
            return r[0]
        elif len(r)>1:
            raise Exception('Duplicate forecast meta info records in KG of same iri {}'.format(src_iri))
        else:
            return None