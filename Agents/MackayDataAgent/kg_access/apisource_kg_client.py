from pyderivationagent.kg_operations import PySparqlClient
from data_classes.ts_data_classes import KgAccessInfo
import logging
from data_classes.iris import *

DELETE_STR = "DELETE WHERE"
SELECT_STR=  "SELECT * WHERE"
INSERT_STR = "INSERT DATA"

META_PREFIX = '''
prefix rml4ts: <http://www.theworldavatar.com/ontology/rml4ts/rml4ts.owl#>
prefix td: <https://www.w3.org/2019/wot/td#>
prefix htv: <http://www.w3.org/2011/http#>
prefix hctl: <https://www.w3.org/2019/wot/hypermedia#>
prefix rml: <http://semweb.mmlab.be/ns/rml#>
prefix time: <https://www.w3.org/2006/time#>
'''

SOURCE_QUERY = '''
{action} {{
<{map_iri}> rml:logicalSource ?lsource.
?lsource rml:source ?source.
?source td:hasForm ?form.
?lsource rml:referenceFormulation ?type.
?lsource rml4ts:value_iterator ?value_iter.
?lsource rml4ts:time_iterator ?time_iter.
OPTIONAL {{ ?lsource rml4ts:hasAlgebra ?algebra. }}
}}
'''

REQ_DYNAMIC_QUERY = '''
{action} {{
<{source_iri}> td:hasForm ?form.
?form rml4ts:hasDynamicTarget ?dynamic_source.
?form hctl:forContentType ?contenttype.
?form htv:methodName ?method.
}}
'''

REQ_QUERY = '''
{action} {{
<{source_iri}> td:hasForm ?form.
?form hctl:hasTarget ?url.
?form hctl:forContentType ?contenttype.
?form htv:methodName ?method.
}}
'''

TARGET_QUERY = '''
{action} {{
?target rml4ts:has_ts_mapping <{map_iri}>.
}}


'''


INTERVAL_QUERY = '''
{action} {{
<{map_iri}> time:hasDuration ?duration .
?duration time:numericDuration ?interval ;
}}
'''


CTYPE_2_FORMAT = {'text/csv':'csv', 'application/json':'json','application/vnd.openxmlformats-officedocument.spreadsheetml.sheet':'xlsx'}


class APISourceSparqlClient:
    def __init__(self, kg_info: KgAccessInfo):
        self.sparql_client = PySparqlClient(kg_info.endpoint, kg_info.endpoint, kg_user=kg_info.user,
                                            kg_password=kg_info.password)
        self.base_iri = GLOBAL_BASE + kg_info.endpoint.split('/')[-2] # namespace



    def get_target_iri_from_map_iri(self, map_iri):
        q = META_PREFIX.format(self.base_iri) + TARGET_QUERY.format(action=SELECT_STR, map_iri = map_iri)
        r = self.sparql_client.performQuery(q)
        if len(r) > 0:
            target_iri = r[0]['target']
            return target_iri
        else:
            logging.info('map iri '+map_iri+' do not have a associated ontology instance' )
            return None

    def get_api_info(self, map_iri) -> dict: # target number IRI -> API request parameters
        # target IRI -> mapping IRI
        # mapping IRI -> source af IRI
        # API source IRI -> request parameters
        q = META_PREFIX.format(self.base_iri) + SOURCE_QUERY.format(action=SELECT_STR, map_iri = map_iri)
        print(q)
        r = self.sparql_client.performQuery(q)
        print(r)
        if len(r) > 0:
            source_iri = r[0]['source']
            url_info = self.get_source_url(source_iri)
            print(url_info)
            if not url_info:
                return {}
            else:
                url_info['value_iter'] = r[0]['value_iter']
                url_info['time_iter'] = r[0]['time_iter']
                if 'algebra' in r[0]:
                    algebra_name = r[0]['algebra'].split('#')[-1].lower()
                    url_info['calculation'] = 'algebra_'+ algebra_name
                return url_info
        else:
            logging.info('map iri '+map_iri+' is not properly defined in KG' )
            return {}

    def get_update_interval(self, map_iri):
        q_d =  META_PREFIX.format(self.base_iri) + INTERVAL_QUERY.format(action=SELECT_STR, map_iri = map_iri)
        r = self.sparql_client.performQuery(q_d)
        if len(r) > 0:
            interval = r[0]['interval']
            print(interval)
            return interval
        return None

    def get_source_url(self, source_iri) -> dict: # API resource form IRI -> URL request parameters
        q_d =  META_PREFIX.format(self.base_iri) + REQ_DYNAMIC_QUERY.format(action=SELECT_STR, source_iri = source_iri)
        print(q_d)
        r = self.sparql_client.performQuery(q_d)
        outdict = {"dynamic_generated":False}
        if len(r) > 0:
            source_iri = r[0]['dynamic_source']
            outdict['method'] = r[0]['method']
            outdict['contenttype'] = r[0]['contenttype']
            outdict["dynamic_generated"] = True
        q =     META_PREFIX.format(self.base_iri) + REQ_QUERY.format(action=SELECT_STR, source_iri = source_iri)
        print(q)
        r = self.sparql_client.performQuery(q)
        print(r)
        if len(r) > 0:
            outdict['url'] = r[0]['url']
            if outdict["dynamic_generated"]:
                outdict["method_dynamic"] = r[0]['method']
            else:
                outdict["method"] = r[0]['method']
                outdict['contenttype'] = r[0]['contenttype']
            outdict['format'] = CTYPE_2_FORMAT[outdict['contenttype']]
            return  outdict
        else:
            logging.info('source iri '+source_iri+' do not have an asscociated API mappings' )
            return {}