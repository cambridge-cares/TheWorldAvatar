from pyderivationagent.kg_operations import PySparqlClient
from data_classes.ts_data_classes import KgAccessInfo
import logging
from data_classes.iris import *

# This module provides KG-query related utility functions for API_meta-data triples

DELETE_STR = "DELETE WHERE"
SELECT_STR = "SELECT * WHERE"
INSERT_STR = "INSERT DATA"

META_PREFIX = '''
prefix rml4ts: <https://www.theworldavatar.com/kg/rml4ts/>
prefix td: <https://www.w3.org/2019/wot/td#>
prefix htv: <http://www.w3.org/2011/http#>
prefix hctl: <https://www.w3.org/2019/wot/hypermedia#>
prefix rml: <http://semweb.mmlab.be/ns/rml#>
prefix time: <http://www.w3.org/2006/time#>
prefix ontode: <https://www.theworldavatar.com/kg/ontoderivation/>
'''

SOURCE_QUERY = '''
{action} {{
<{map_iri}> rml:logicalSource ?lsource.
?lsource rml:source ?source.
?source td:hasForm ?form.
?lsource rml:referenceFormulation ?type.
?lsource rml4ts:value_iterator ?value_iter.
?lsource rml4ts:time_iterator ?time_iter.
OPTIONAL {{ ?lsource rml4ts:hasMapFunction ?mfunc. }}
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
TARGET_REVERSE_QUERY = '''
{action} {{
<{target_iri}> rml4ts:has_ts_mapping ?map.
}}
'''

INTERVAL_QUERY = '''
{action} {{
<{map_iri}> time:hasDuration ?duration .
?duration time:numericDuration ?interval ;
}}
'''

TS_QUERY = '''
{action} {{
<{target_iri}> ontode:belongsTo ?de.
?de time:hasTime ?tp.
?tp time:inTimePosition ?ntp.
?ntp time:numericPosition ?timestamp.
}}
'''

CTYPE_2_FORMAT = {'text/csv': 'csv', 'application/json': 'json',
                  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet': 'xlsx'}


class APISourceSparqlClient:
    def __init__(self, kg_info: KgAccessInfo):
        self.sparql_client = PySparqlClient(kg_info.endpoint, kg_info.endpoint, kg_user=kg_info.user,
                                            kg_password=kg_info.password)
        self.base_iri = GLOBAL_BASE + kg_info.endpoint.split('/')[-2]  # namespace

    def get_target_iri_from_map_iri(self, map_iri):
        q = META_PREFIX.format(self.base_iri) + TARGET_QUERY.format(action=SELECT_STR, map_iri=map_iri)
        r = self.sparql_client.performQuery(q)
        if len(r) > 0:
            target_iri = r[0]['target']
            return target_iri
        else:
            logging.info('map iri ' + map_iri + ' do not have a associated ontology instance')
            return None

    def get_map_iri_from_target_iri(self, target_iri):
        q = META_PREFIX.format(self.base_iri) + TARGET_REVERSE_QUERY.format(action=SELECT_STR, target_iri=target_iri)
        r = self.sparql_client.performQuery(q)
        if len(r) > 0:
            map_iri = r[0]['map']
            return map_iri
        else:
            logging.info('target_iri  ' + target_iri + ' do not have a associated API map instance')
            return None

    def get_api_info(self, map_iri) -> dict:  # target number IRI -> API request parameters
        # target IRI -> mapping IRI
        # mapping IRI -> source af IRI
        # API source IRI -> request parameters
        q = META_PREFIX.format(self.base_iri) + SOURCE_QUERY.format(action=SELECT_STR, map_iri=map_iri)
        r = self.sparql_client.performQuery(q)
        if len(r) > 0:
            source_iri = r[0]['source']
            url_info = self.get_source_url(source_iri)
            if not url_info:
                return {}
            else:
                url_info['value_iter'] = r[0]['value_iter']
                url_info['time_iter'] = r[0]['time_iter']
                if 'mfunc' in r[0]:
                    mfunc_name = r[0]['mfunc'].split('/')[-1].lower()
                    url_info['calculation'] = 'map_function_' + mfunc_name
                return url_info
        else:
            logging.info('map iri ' + map_iri + ' is not properly defined in KG')
            return {}

    def get_update_interval(self, map_iri):
        q_d = META_PREFIX.format(self.base_iri) + INTERVAL_QUERY.format(action=SELECT_STR, map_iri=map_iri)
        r = self.sparql_client.performQuery(q_d)
        if len(r) > 0:
            interval = r[0]['interval']
            return interval
        return None

    def get_timestep(self, ts_iri):
        q_ts = META_PREFIX.format(self.base_iri) + TS_QUERY.format(action=SELECT_STR, target_iri=ts_iri)
        r = self.sparql_client.performQuery(q_ts)
        if len(r) > 0:
            ts = r[0]['timestamp']
            return ts
        return None

    def get_source_url(self, source_iri) -> dict:  # API resource form IRI -> URL request parameters
        q_d = META_PREFIX.format(self.base_iri) + REQ_DYNAMIC_QUERY.format(action=SELECT_STR, source_iri=source_iri)
        r = self.sparql_client.performQuery(q_d)
        outdict = {"dynamic_generated": False}
        if len(r) > 0:
            source_iri = r[0]['dynamic_source']
            outdict['method'] = r[0]['method']
            outdict['contenttype'] = r[0]['contenttype']
            outdict["dynamic_generated"] = True
        q = META_PREFIX.format(self.base_iri) + REQ_QUERY.format(action=SELECT_STR, source_iri=source_iri)
        r = self.sparql_client.performQuery(q)
        if len(r) > 0:
            outdict['url'] = r[0]['url']
            if outdict["dynamic_generated"]:
                outdict["method_dynamic"] = r[0]['method']
            else:
                outdict["method"] = r[0]['method']
                outdict['contenttype'] = r[0]['contenttype']
            outdict['format'] = CTYPE_2_FORMAT[outdict['contenttype']]
            return outdict
        else:
            logging.info('source iri ' + source_iri + ' do not have an asscociated API mappings')
            return {}
