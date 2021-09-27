# from poly_model import PolyModel
# from summit_doe import DoEModel

# from summit.utils.dataset import DataSet
# from summit.domain import ContinuousVariable, Domain
# from summit.strategies import TSEMO, SOBO

# # model = DoEModel()
# # result = model.suggest()
# domain = Domain()
# domain += ContinuousVariable(name='temperature', description='reaction temperature in celsius', bounds=[50, 100])
# domain += ContinuousVariable(name='flowrate_a', description='flow of reactant a in mL/min', bounds=[0.1, 0.5])
# domain += ContinuousVariable(name='flowrate_b', description='flow of reactant b in mL/min', bounds=[0.1, 0.5])
# domain += ContinuousVariable(name="yld", description='yield of reaction', bounds=[0,100], is_objective=True)
# columns = [v.name for v in domain.variables]
# # values = {("temperature", "DATA"): [60, 50],("flowrate_a", "DATA"): [0.5, 0.3],("flowrate_b", "DATA"): [0.5, 0.2],("yld", "DATA"): [50, 30]}
# # values = {("temperature", "DATA"): 45,("flowrate_a", "DATA"): 0.1,("flowrate_b", "DATA"): 0.2,("yld", "DATA"): 16,("de", "DATA"): 60}
# values = {("temperature", "DATA"): [1.5, 10.0],("flowrate_a", "DATA"): [0.5, 3.0],("flowrate_b", "DATA"): [0.1, 4.0],("yld", "DATA"): [30.0, 100.0]}
# previous_results = DataSet(values, columns=columns)
# strategy = TSEMO(domain)
# result = strategy.suggest_experiments(1,previous_results)
# print(result)
# print(values)
# print(previous_results)
# # values = {("tau", "DATA"): [1.5, 10.0],("equiv_pldn", "DATA"): [0.5, 3.0],("conc_dfnb", "DATA"): [0.1, 4.0],("temperature", "DATA"): [30.0, 100.0],("strategy", "METADATA"): ["test", "test"]}
# # ds = DataSet(values)
# # print(ds)

from kgUtils import *

response1 = performQuery("ontokin", "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#> \
                                     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>   SELECT ?mechanismIRI \
                                     WHERE   { ?mechanismIRI rdf:type ontokin:ReactionMechanism .} LIMIT 10")
print(response1)
# "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#> \
#                                     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>   SELECT ?mechanismIRI \
#                                     WHERE   { ?mechanismIRI rdf:type ontokin:ReactionMechanism .} LIMIT 10"