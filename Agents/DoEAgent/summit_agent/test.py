# # from poly_model import PolyModel
# from summit_doe import *

# from summit.utils.dataset import DataSet
# from summit.domain import ContinuousVariable, Domain
# from summit.strategies import TSEMO, SOBO

from jpsSingletons import jpsBaseLibGW
from resources.parameter import *
from resources.doeagent_properties import *
# from kgUtils import *

import pandas as pd
import json

# from summit.utils.dataset import DataSet
from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF
# from rdflib.plugins.stores import sparqlstore
import uuid

def main():

    endpoint = SPARQL_QUERY_ENDPOINT
    strategy_instance = trimIRI("https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1")
    domain_instance = trimIRI("https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1")
    systemResponse_instances = ['https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1', 'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2']
    historicalData_instance = trimIRI("https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1")
    # systemResponse_instances = ['https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1']
    # response = getHistoricalDataOfDesignVariable(endpoint, variable_instance, historicalData_instance, checkIfPositionalIDExists(endpoint, variable_instance))
    # {"agent_input": {"strategy": "https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1", "domain": "https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1", "systemResponse": ['https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1', 'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2'], "historicalData": "https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1"}}
    
    strategy_dict = getDoEStrategy(endpoint, strategy_instance)
    designVariable_dict, systemResponse_dict, previous_results = constructHistoricalDataTable(endpoint, domain_instance, systemResponse_instances, historicalData_instance)
    historicalData_dict = {"historicalData": previous_results.drop(columns="rxnexp").astype(float)}
    
    doe_info = {**strategy_dict, **designVariable_dict, **systemResponse_dict, **historicalData_dict}
    print(doe_info)
    # doe = { \
    #             "TSEMO": {"nSpectralPoints": 30, "nGenerations": 20, "populationSize": 20}, \
    #             "continuousVariables": [{"name": "ContinuousVariable_1", "lower": 1, "upper": 10}, 
    #             {"name": "ContinuousVariable_2", "lower": 0.02, "upper": 0.2},
    #             {"name": "ContinuousVariable_3", "lower": 5, "upper": 15},
    #             {"name": "ContinuousVariable_4", "lower": 30, "upper": 70}], \
    #             "systemResponses": [{"name": "SystemResponse_1", "direction": "maximise"}, 
    #             {"name": "SystemResponse_2", "direction": "minimise"}], \
    #             "historicalData": previous_results, \
    #             "numOfExp": 1}
    # next_exp = proposeNewExperiment(doe_info)
    # print(next_exp)

    # endpoint = "http://theworldavatar.com/blazegraph/namespace/textontorxn/sparql"
    # variable_instance = trimIRI("https://theworldavatar.com/kb/ontodoe/DoE_1/ContinuousVariable_1")
    # strategy_instance = trimIRI("https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1")
    # historicalData_instance = trimIRI("https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1")
    # domain_instance = trimIRI("https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1")
    # systemResponse_instances = ['https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1', 'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2']
    # # systemResponse_instances = ['https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1']
    # # response = getHistoricalDataOfDesignVariable(endpoint, variable_instance, historicalData_instance, checkIfPositionalIDExists(endpoint, variable_instance))
    # response = getDesignVariables(endpoint, domain_instance)
    # print(getSystemResponses(endpoint, systemResponse_instances))
    # print(response)
    # # print(response)
    # # print(len(response))
    # list_of_designVariable_dict, list_of_systemResponse_dict, previous_results = constructHistoricalDataTable(endpoint, domain_instance, systemResponse_instances, historicalData_instance)
    # previous_results = previous_results.drop(columns="rxnexp").astype(float)
    # # previous_results = pd.to_numeric(previous_results, errors='coerce')
    # print(list_of_designVariable_dict)
    # print(list_of_systemResponse_dict)
    # print(previous_results)
    # # for r in response:
    # #     print(r['id']) if 'id' in r else None
    # print(getDoEStrategy(endpoint, strategy_instance))
    # doe = { \
    #             "TSEMO": {"nSpectralPoints": 30, "nGenerations": 20, "populationSize": 20}, \
    #             "continuousVariables": [{"name": "ContinuousVariable_1", "lower": 1, "upper": 10}, 
    #             {"name": "ContinuousVariable_2", "lower": 0.02, "upper": 0.2},
    #             {"name": "ContinuousVariable_3", "lower": 5, "upper": 15},
    #             {"name": "ContinuousVariable_4", "lower": 30, "upper": 70}], \
    #             "systemResponses": [{"name": "SystemResponse_1", "direction": "maximise"}, 
    #             {"name": "SystemResponse_2", "direction": "minimise"}], \
    #             "historicalData": previous_results, \
    #             "numOfExp": 1}
    # next_exp = proposeNewExperiment(doe)
    # print(next_exp)
    
    # domain = Domain()
    # domain += ContinuousVariable(name='ContinuousVariable_1', description='molar equiv of 2', bounds=[1, 10])
    # domain += ContinuousVariable(name='ContinuousVariable_2', description='molar equiv of 3', bounds=[0.02, 0.2])
    # domain += ContinuousVariable(name='ContinuousVariable_3', description='residence time', bounds=[5, 15])
    # domain += ContinuousVariable(name='ContinuousVariable_4', description='reaction temperature', bounds=[30, 70])
    
    # domain += ContinuousVariable(name='SystemResponse_1', description='yield of reaction', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)
    # domain += ContinuousVariable(name='SystemResponse_2', description='run material cost', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=False)
    
    # columns = [v.name for v in domain.variables]
    # strategy = TSEMO(domain, n_spectral_points=30, generations=20, pop_size=20)
    # # print(type(previous_results['ContinuousVariable_1'][0]))
    # previous_results = DataSet.from_df(previous_results)
    # # print(previous_results)
    # result = strategy.suggest_experiments(1,prev_res=previous_results)
    # print(result)
    # # result.to_csv('test.csv')
    # print(previous_results)

    # # class DoEObject(dict):
    # #     def __getattribute__(self, name: str):
    # #         return super().__getattribute__(name)
    # #     def __setattr__(self, name: str, value) -> None:
    # #         return super().__setattr__(name, value)
    
    
    # # print(startone.a)
    
    # # domain = Domain()
    # # domain += ContinuousVariable(name='var0', description='molar equiv of 2', bounds=[0, 100])
    # # domain += ContinuousVariable(name='var1', description='molar equiv of 3', bounds=[0, 100])
    # # # domain += ContinuousVariable(name='ContinuousVariable_3', description='residence time', bounds=[5, 15])
    # # domain += ContinuousVariable(name='var2', description='reaction temperature', bounds=[0, 100000000])
    
    # # domain += ContinuousVariable(name='Yield-1', description='yield of reaction', bounds=[-1000000000000000000000, 100000000000000000000], is_objective=True, maximize=True)
    
    # # df = pd.read_excel("./summit_agent/resources/ReactionData.xls", sheet_name="TSEMO Data")
    # # data = DataSet.from_df(df)
    # # print(data)
    # # strategy = TSEMO(domain, n_spectral_points=30, generations=20, pop_size=20)
    # # experimentsdf = strategy.suggest_experiments(1, data)
    # # print(experimentsdf)

    
def main2():
    string = """{"('ContinuousVariable_1', 'DATA')":{"0":8.2284274166},"('ContinuousVariable_2', 'DATA')":{"0":0.1575472341},"('ContinuousVariable_3', 'DATA')":{"0":5.0033808762},"('ContinuousVariable_4', 'DATA')":{"0":53.4850225055},"('SystemResponse_1', 'DATA')":{"0":-1.0055809259},"('SystemResponse_2', 'DATA')":{"0":6.194594869},"('strategy', 'METADATA')":{"0":"TSEMO"},"('SystemResponse_1_variance', 'METADATA')":{"0":1.489259273},"('SystemResponse_1_noise', 'METADATA')":{"0":0.1919815179},"('rmse_train_spectral', 'METADATA')":{"0":0.1499131621},"('SystemResponse_1_ContinuousVariable_1_lengthscale', 'METADATA')":{"0":0.3086220071},"('SystemResponse_1_ContinuousVariable_2_lengthscale', 'METADATA')":{"0":0.326631102},"('SystemResponse_1_ContinuousVariable_3_lengthscale', 'METADATA')":{"0":0.3350415558},"('SystemResponse_1_ContinuousVariable_4_lengthscale', 'METADATA')":{"0":0.3296703199},"('SystemResponse_2_variance', 'METADATA')":{"0":1.2906870139},"('SystemResponse_2_noise', 'METADATA')":{"0":0.0890160606},"('SystemResponse_2_ContinuousVariable_1_lengthscale', 'METADATA')":{"0":0.4259952595},"('SystemResponse_2_ContinuousVariable_2_lengthscale', 'METADATA')":{"0":0.3024215685},"('SystemResponse_2_ContinuousVariable_3_lengthscale', 'METADATA')":{"0":0.3312573088},"('SystemResponse_2_ContinuousVariable_4_lengthscale', 'METADATA')":{"0":0.3534948822},"('iterations', 'METADATA')":{"0":1}}"""
    # string = """{"('ContinuousVariable_1', 'DATA')":{"0":1.8163072134},
    # "('ContinuousVariable_2', 'DATA')":{"0":0.0224809419},
    # "('ContinuousVariable_3', 'DATA')":{"0":6.7605145143},
    # "('ContinuousVariable_4', 'DATA')":{"0":44.9694354228},
    # "('SystemResponse_1', 'DATA')":{"0":29.5262381291},
    # "('SystemResponse_2', 'DATA')":{"0":5.550231053},
    # "('strategy', 'METADATA')":{"0":"TSEMO"},"('SystemResponse_1_variance', 'METADATA')":{"0":1.489259273},"('SystemResponse_1_noise', 'METADATA')":{"0":0.1919815179},"('rmse_train_spectral', 'METADATA')":{"0":0.1402844027},"('SystemResponse_1_ContinuousVariable_1_lengthscale', 'METADATA')":{"0":0.3086220071},"('SystemResponse_1_ContinuousVariable_2_lengthscale', 'METADATA')":{"0":0.326631102},"('SystemResponse_1_ContinuousVariable_3_lengthscale', 'METADATA')":{"0":0.3350415558},"('SystemResponse_1_ContinuousVariable_4_lengthscale', 'METADATA')":{"0":0.3296703199},"('SystemResponse_2_variance', 'METADATA')":{"0":1.2906870139},"('SystemResponse_2_noise', 'METADATA')":{"0":0.0890160606},"('SystemResponse_2_ContinuousVariable_1_lengthscale', 'METADATA')":{"0":0.4259952595},"('SystemResponse_2_ContinuousVariable_2_lengthscale', 'METADATA')":{"0":0.3024215685},"('SystemResponse_2_ContinuousVariable_3_lengthscale', 'METADATA')":{"0":0.3312573088},"('SystemResponse_2_ContinuousVariable_4_lengthscale', 'METADATA')":{"0":0.3534948822},"('iterations', 'METADATA')":{"0":1}}"""
    # [[1.8163072133660791 0.02248094185228038 6.760514514267891 44.96943542280268 29.526238129088064 5.550231053016629]]
    j = json.loads(string)
    for k in j:
        # print(k)
        if 'SystemResponse_1' in k:
            print(k)

def main3():
    dict_ = {'index': [0], 'columns': [('ContinuousVariable_1', 'DATA'), ('ContinuousVariable_2', 'DATA'), ('ContinuousVariable_3', 'DATA'), ('ContinuousVariable_4', 'DATA'), ('SystemResponse_1', 'DATA'), ('SystemResponse_2', 'DATA'), ('strategy', 'METADATA'), ('SystemResponse_1_variance', 'METADATA'), ('SystemResponse_1_noise', 'METADATA'), ('rmse_train_spectral', 'METADATA'), ('SystemResponse_1_ContinuousVariable_1_lengthscale', 'METADATA'), ('SystemResponse_1_ContinuousVariable_2_lengthscale', 'METADATA'), ('SystemResponse_1_ContinuousVariable_3_lengthscale', 'METADATA'), ('SystemResponse_1_ContinuousVariable_4_lengthscale', 'METADATA'), ('SystemResponse_2_variance', 'METADATA'), ('SystemResponse_2_noise', 'METADATA'), ('SystemResponse_2_ContinuousVariable_1_lengthscale', 'METADATA'), ('SystemResponse_2_ContinuousVariable_2_lengthscale', 'METADATA'), ('SystemResponse_2_ContinuousVariable_3_lengthscale', 'METADATA'), ('SystemResponse_2_ContinuousVariable_4_lengthscale', 'METADATA'), ('iterations', 'METADATA')], 'data': [[7.174510974170408, 0.08173921437073844, 12.629386033004979, 67.18350143026052, -33.15469598062445, 6.652716018377521, 'TSEMO', 1.4892592730306422, 0.19198151793249038, 0.0826615936170382, 0.3086220071343901, 0.3266311020243481, 0.33504155583514605, 0.3296703198595183, 1.2906870139083357, 0.08901606060249682, 0.4259952594737004, 0.30242156854680446, 0.33125730881278703, 0.3534948822230497, 1]]}
    df = DataSet.from_dict(dict_)
    print(type(df['ContinuousVariable_1'][0]))

def main4():
    g = createReactionVariation('https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1')
    print(g.serialize(format='turtle'))


def createReactionVariation(rxnexp: str, reaction_conditions, performance_indicators) -> Graph:
    """rxnexp refers to the known OntoRxn:ReactionExperiment
    """
    g = Graph()
    rxnexp = URIRef(rxnexp)
    rxnvar = URIRef(NAMESPACE_KB_ONTORXN + getShortName(ONTORXN_REACTIONVARIATION) + '_' + str(uuid.uuid4()))
    
    g.add((rxnvar, RDF.type, URIRef(ONTORXN_REACTIONVARIATION)))
    g.add((rxnvar, URIRef(ONTORXN_ISVARIATIONOF), rxnexp))
    g.add((rxnexp, URIRef(ONTORXN_HASVARIATION), rxnvar))

    # for con in reaction_conditions:
    #     g = 


    # exp1:RxnExp_1
	# 	OntoRxn:hasVariation :RxnExp_1;
	# .

	# :RxnExp_1
	# 	rdf:type OntoRxn:ReactionVariation;
	# 	OntoRxn:isVariationOf exp1:RxnExp_1;
	# 	OntoRxn:hasResTime :ResidenceTime_1;
	# 	OntoRxn:hasRxnTemperature :RxnTemperature_1;
	# 	OntoRxn:hasStoichiometryRatio :StoiRatio_2;
	# 	OntoRxn:hasStoichiometryRatio :StoiRatio_3;
	# 	OntoRxn:hasPerformanceIndicator :Yield_1;
	# 	OntoRxn:hasPerformanceIndicator :RunMaterialCost_1;
	# .

    return g

class ReactionVariation:
    def __init__(self, rxnIRI, rxn_conditions: list, perf_indicators: list, query_endpoint, update_endpoint):
        self.rIRI = URIRef(rxnIRI)
        self.rxnvar = ""
        self.conditions = rxn_conditions
        self.indicators = perf_indicators
        self.g = Graph()
        self.query = query_endpoint
        self.update = update_endpoint
        # self.store = sparqlstore.SPARQLUpdateStore()
        # self.up_str = 'INSERT DATA { '
    
    # def uploadOntoRxnInstanceToKG(self):
    #     # self.store.open((self.query, self.update))
    #     self.prepareSPARQLUpdate()
    #     response = performUpdate(self.update, self.up_str)
    #     print(response)
    #     # self.store.update(self.up_str)
    #     # self.store.close()
    #     # for s, p, o in self.g:
    #     #     print(f'<{type(s)}> <{type(p)}> <{type(o)}>')
    #     #     print(f'<{s}> <{p}> <{o}>')
    #     #     # self.store.add((s, p, o))
    
    # def prepareSPARQLUpdate(self):
    #     for s, p, o in self.g:
    #         self.up_str = self.up_str + f'<{s}> <{p}> <{o}> . '
    #     self.up_str = self.up_str + ' }'
    
    def createOntoRxnInstance(self):
        self.rxnvar = URIRef(NAMESPACE_KB_ONTORXN + getShortName(ONTORXN_REACTIONVARIATION) + '_' + str(uuid.uuid4()))
        
        self.g.add((self.rxnvar, RDF.type, URIRef(ONTORXN_REACTIONVARIATION)))
        self.g.add((self.rxnvar, URIRef(ONTORXN_ISVARIATIONOF), self.rIRI))
        self.g.add((self.rIRI, URIRef(ONTORXN_HASVARIATION), self.rxnvar))

        for con in self.conditions:
            self.addReactionCondition(**con)
        for ind in self.indicators:
            self.addPerformanceIndicator(**ind)
        
        filePath = f'{str(uuid.uuid4())}.xml'
        self.g.serialize(filePath, format='xml')
        return filePath

    def addReactionCondition(self, clz, num, id=None, **kwargs):
        """
        rxn: the complete IRI of the reaction experiment (or variation) that the reaction condition is to be added to
        clz: the rdf:type (complete IRI) of the reaction condition to be added
        """
        
        condition_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(clz) + '_' + str(uuid.uuid4()))
        measure_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(OM_MEASURE) + '_' + str(uuid.uuid4()))

        for pred in getObjectRelationship(self.query, self.rIRI, clz):
            self.g.add((self.rxnvar, URIRef(pred), condition_iri))
        
        self.g.add((condition_iri, RDF.type, URIRef(clz)))
        self.g.add((condition_iri, URIRef(OM_HASPHENOMENON), self.rxnvar))
        self.g.add((condition_iri, URIRef(OM_HASVALUE), measure_iri))
        # Only add positionalID if it exists
        if id is not None:
            self.g.add((condition_iri, URIRef(ONTODOE_POSITIONALID), Literal(int(id))))
        # Also add indicatesMultiplicityOf/indicatesUsageOf if it's a OntoRxn:StoichiometryRatio/OntoRxn:ReactionScale
        if (clz == ONTORXN_STOICHIOMETRYRATIO) or (clz == ONTORXN_REACTIONSCALE):
            res = getIndicatesInputChemical(self.query, str(self.rIRI), clz, int(id))
            self.g.add((condition_iri, URIRef(res['indicates']), URIRef(res['inputChem'])))

        self.g.add((measure_iri, RDF.type, URIRef(OM_MEASURE)))
        if id is not None:
            self.g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(getQuantityUnit(self.query, str(self.rIRI), clz, int(id)))))
        else:
            self.g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(getQuantityUnit(self.query, str(self.rIRI), clz, None))))
        self.g.add((measure_iri, URIRef(OM_HASNUMERICALVALUE), Literal(float(num))))

    def addPerformanceIndicator(self, clz, id=None, **kwargs):
        # Create performance indicator instance
        indicator_iri = URIRef(NAMESPACE_KB_ONTORXN + getShortName(clz) + '_' + str(uuid.uuid4()))
        # Add performance indicator to reaction experiment (or variation)
        self.g.add((self.rxnvar, URIRef(ONTORXN_HASPERFORMANCEINDICATOR), indicator_iri))
        self.g.add((indicator_iri, URIRef(OM_HASPHENOMENON), self.rxnvar))
        # Add rdf:type to created performance indicator instance
        self.g.add((indicator_iri, RDF.type, URIRef(ONTORXN_PERFORMANCEINDICATOR)))
        self.g.add((indicator_iri, RDF.type, URIRef(clz)))
        if id is not None:
            self.g.add((indicator_iri, URIRef(ONTODOE_POSITIONALID), Literal(int(id))))

def getShortName(iri):
    iri = trimIRI(iri)
    if '#' in iri:
        return iri[iri.rfind('#')+1:]
    else:
        return iri[iri.rfind('/')+1:]

def trimIRI(iri):
    if iri.startswith("<"):
        iri = iri[1:]
    if iri.endswith(">"):
        iri = iri[:-1]
    return iri

def main5():
    rxn_conditions = [{"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#StoichiometryRatio', "id": 2, "num": 3}, \
    {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#StoichiometryRatio', "id": 2, "num": 0.05}, \
    {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#ReactionTemperature', "num": 60}, \
    {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#ResidenceTime', "num": 5}, ]
    perf_indicators = [{"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#Yield'}, \
    {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#RunMaterialCost', "id": 7}, \
    {"clz": 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#SpaceTimeYield', "id": 8, "num": 0.5}]
    endpoint = "http://theworldavatar.com/blazegraph/namespace/textontorxn/sparql"
    new_exp_ = ReactionVariation('https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1', rxn_conditions, perf_indicators, endpoint, endpoint)
    filePath = new_exp_.createOntoRxnInstance()
    uploadOntology(filePath)
# doe = { \
    #             "TSEMO": {"nSpectralPoints": 30, "nGenerations": 20, "populationSize": 20}, \
    #             "continuousVariables": [{"name": "ContinuousVariable_1", "lower": 1, "upper": 10}, 
    #             {"name": "ContinuousVariable_2", "lower": 0.02, "upper": 0.2},
    #             {"name": "ContinuousVariable_3", "lower": 5, "upper": 15},
    #             {"name": "ContinuousVariable_4", "lower": 30, "upper": 70}], \
    #             "systemResponses": [{"name": "SystemResponse_1", "direction": "maximise"}, 
    #             {"name": "SystemResponse_2", "direction": "minimise"}], \
    #             "historicalData": previous_results, \
    #             "numOfExp": 1}
def main6():
    g = Graph()
    g.parse("/home/jb2197/code/TheWorldAvatar/Agents/DoEAgent/summit_agent/11c6c7b0-14da-48af-bf3a-a8474042b693.xml")
    # g.serialize(format='turtle')
    up_str = 'INSERT DATA { '
    for s, p, o in g:
        up_str = up_str + f'<{s}> <{p}> <{o}> . \n'
    up_str = up_str + ' }'
    print(up_str)

def main7():
    print(trimIRI(['https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1']))

def main8():
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")
    storeClient = jpsBaseLib_view.RemoteStoreClient(SPARQL_QUERY_ENDPOINT)
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient)
    # test_derivationClient = derivationClient.addTimeInstance('https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1')

    
    derivationSparql = jpsBaseLib_view.DerivationSparql
    boo = jpsBaseLib_view.DerivationSparql.isFinished(storeClient, 'http://www.theworldavatar.com/kb/ontoderivation/derivation_doe/Derivation_doe')
    print(boo)
    # test = derivationSparql.addTimeInstance(storeClient, 'https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1')
    
    # KGRouter = jpsBaseLib_view.StoreRouter
    # KGClient = KGRouter.getStoreClient(SPARQL_UPDATE_ENDPOINT, True, False)
    # getStoreClient(targetResourceID, True, False)
    # derivationClient = jpsBaseLib_view.DerivationClient(storeClient)
    # derivationSparql = jpsBaseLib_view.DerivationSparql
    # test = derivationSparql.addTimeInstance(storeClient, DOEAGENT_ONTOAGENT_SERVICE)
    # print(test)

def main9():
    iri = ['http://www.theworldavatar.com/kb/ontoderivation/derivation_doe/Derivation_doe>', '<http://www.theworldavatar.com/kb/ontoderivation/derivation_doe/Derivation_doe', '<http://www.theworldavatar.com/kb/ontoderivation/derivation_doe/Derivation_doe>']
    iri = trimIRI(iri)
    print(iri)

def main10():
    iri = ['http://www.theworldavatar.com/kb/ontoderivation/derivation_doe/Derivation_doe>', '<http://www.theworldavatar.com/kb/ontoderivation/derivation_doe#Derivation_doe', '<http://www.theworldavatar.com/kb/ontoderivation/derivation_doe/Derivation_doe/>']
    for i in iri:
        print(getNameSpace(i))

def trimIRI(iri):
    if isinstance(iri, list):
        for i in range(len(iri)):
            iri[i] = trimIRI(iri[i])
    else:
        if iri.startswith("<"):
            iri = iri[1:]
        if iri.endswith(">"):
            iri = iri[:-1]
    return iri

def getNameSpace(iri):
    iri = trimIRI(iri)
    if '#' in iri:
        return iri[:iri.rfind('#')+1]
    else:
        return iri[:iri.rfind('/')+1]

if __name__ == "__main__":
    main10()
