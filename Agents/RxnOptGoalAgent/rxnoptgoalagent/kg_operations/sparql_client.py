# import chemistry_and_robots.kg_operations.dict_and_list as dal
from rxnoptgoaliteragent.kg_operations.sparql_client import RxnOptGoalIterSparqlClient

from rxnoptgoalagent.data_model import *

class RxnOptGoalSparqlClient(RxnOptGoalIterSparqlClient):
    def get_all_rxn_opt_plans(self):
        query = f"""SELECT ?rxn_opt WHERE {{ ?rxn_opt a <{ONTOGOAL_RXNOPTPLAN}> . }}"""
        response = self.performQuery(query)
        return [list(r.values())[0] for r in response]
