import json
import logging
import requests
import time
import traceback

import ontomatch.httpCaller.httpcaller as httpcaller
import ontomatch.evaluate
import ontomatch.instancematching
import ontomatch.knowledge.enrichment
import ontomatch.knowledge.geocoding
import ontomatch.knowledge.geoNames
import ontomatch.matchManager
import ontomatch.scoring
import ontomatch.utils.blackboard
import ontomatch.utils.util

class Agent():

    def start(self, config_handle:str, http:bool=False):

        # read the config params from blackboard
        config_json = ontomatch.utils.util.call_agent_blackboard_for_reading(config_handle, http)
        params = ontomatch.utils.util.convert_json_to_dict(config_json)

        try:
            src_addr = params['dataset']['src']
            tgt_addr =  params['dataset']['tgt']
            add_knowledge =  params['pre_processing']['add_knowledge']

            if src_addr.endswith('pkl'):
                src_graph_handle = src_addr
            else:
                _, src_graph_handle = self.call_agent_knowledge_enrichment(src_addr, add_knowledge, http)
            if tgt_addr.endswith('pkl'):
                tgt_graph_handle = tgt_addr
            else:
                _, tgt_graph_handle = self.call_agent_knowledge_enrichment(tgt_addr, add_knowledge, http)

            matching_name = params['matching']['name']
            if matching_name is None:
                logging.debug('for testing only: instance matching is not performed')
            elif matching_name == 'matchManager.matchManager':
                self.__call_match_manager(config_handle, src_graph_handle, tgt_graph_handle, http)
            elif matching_name == 'instancematching.InstanceMatcherWithAutoCalibration':
                self.__call_matching_with_auto_calibration(config_handle, src_graph_handle, tgt_graph_handle, http)
            elif matching_name == 'instancematching.InstanceMatcherWithScoringWeights':
                self.__call_matching_with_scoring_weights(config_handle, src_graph_handle, tgt_graph_handle, http)
            elif matching_name == 'instancematching.InstanceMatcherClassifier':
                self.__call_matching_with_Classifier(config_handle, src_graph_handle, tgt_graph_handle, http)
            else:
                raise RuntimeError('unknown matcher', matching_name)

        except:
            logging.fatal('finished with exception')
            full_traceback = traceback.format_exc()
            print(full_traceback)
            logging.fatal(full_traceback)
            raise

    def call_agent_knowledge_enrichment(self, addr:str, add_knowledge:bool, http:bool=False) -> tuple[str, str]:
        logging.info('calling ontomatch.knowledge.enrichment.Agent, addr=%s, add_knowledge=%s, http=%s',
                addr, add_knowledge, http)
        if http:
            #TODO: check error handling
            try:
                params = dict(addr=addr, add_knowledge=add_knowledge)
                data = httpcaller .caller().callAgent("enrichment", params)
                return data["enriched"], data["handle"]
            except Exception as e:
                raise Exception

        else:
            enriched, handle = ontomatch.knowledge.enrichment.Agent().start(addr, add_knowledge, http=False)
        logging.info('called ontomatch.knowledge.enrichment.Agent, enriched=%s, handle=%s', enriched, handle)
        return enriched, handle

    def __call_matching_with_auto_calibration(self, config_handle:str, src_graph_handle:str, tgt_graph_handle:str, http:bool=False):
        logging.info('calling InstanceMatcherWithAutoCalibration, http=%s', http)
        if http:
            params = dict(config_handle=config_handle, src_graph_handle=src_graph_handle,
                          tgt_graph_handle=tgt_graph_handle)
            self.__call_matching_agent_http("autocalibration", params)
        else:
            matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()
            matcher.start(config_handle, src_graph_handle, tgt_graph_handle, http=False)
        logging.info('called InstanceMatcherWithAutoCalibration')

    def __call_matching_with_Classifier(self, config_handle:str, src_graph_handle:str, tgt_graph_handle:str, http:bool=False):
        logging.info('calling InstanceMatcherClassifier, http=%s', http)
        if http:
            params = dict(config_handle=config_handle, src_graph_handle=src_graph_handle,
                          tgt_graph_handle=tgt_graph_handle)
            self.__call_matching_agent_http("classifier", params)
        else:
            matcher = ontomatch.instancematching.InstanceMatcherClassifier()
            matcher.start(config_handle, src_graph_handle, tgt_graph_handle, http=False)
        logging.info('called InstanceMatcherClassifier')

    def __call_matching_with_scoring_weights(self, config_handle:str, src_graph_handle:str, tgt_graph_handle:str, http:bool=False):
        logging.info('calling InstanceMatcherWithScoringWeights, http=%s', http)
        if http:
            params = dict(config_handle=config_handle, src_graph_handle=src_graph_handle,
                          tgt_graph_handle=tgt_graph_handle)
            self.__call_matching_agent_http("scoring_weight", params)
        else:
            matcher = ontomatch.instancematching.InstanceMatcherWithScoringWeights()
            matcher.start(config_handle, src_graph_handle, tgt_graph_handle, http=False)
        logging.info('called InstanceMatcherWithScoringWeights, http=%s', http)

    def __call_match_manager(self, config_handle:str, src_graph_handle:str, tgt_graph_handle:str, http:bool=False):
        logging.info('calling matchManager, http=%s', http)
        if http:
            params = dict(config_handle=config_handle, src_graph_handle=src_graph_handle, tgt_graph_handle=tgt_graph_handle)
            self.__call_matching_agent_http("default", params)
        else:
            matcher = ontomatch.matchManager.matchManager()
            matcher.start(config_handle, src_graph_handle, tgt_graph_handle, http=False)
        logging.info('called matchManager, http=%s', http)

    def __call_matching_agent_http(self, choice, params):
        try:
            params['choice'] = choice
            data = httpcaller.caller().callAgent("matchmanager", params)
        except Exception as e:
            raise Exception


def start(config_dev=None):

    starttime = time.time()
    params, config_file = ontomatch.utils.util.init(config_dev)
    http = params['pre_processing'].get('http')

    if http:
        # write the config params to the blackboard
        params_str = json.dumps(params)
        rv = requests.post('http://localhost:5000/api/blackboard', params={
            'addr':config_file, 'serialized_object':params_str
        })

        rd = rv.json()
        handle = rd["result"]["handle"]
        rv = requests.post('http://localhost:5000/api/coordinator', params ={
            "config":handle
        })
        re = rv.json()
        logging.info('request finished, %s', re)
    else:
        # write the config params to the blackboard
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(config_file, params_str, http=False)
        Agent().start(config_handle, http=False)

    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)

if __name__ == '__main__':
    start()
