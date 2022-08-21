from rxnoptgoaliteragent.data_model import *
from chemistry_and_robots.kg_operations import *

class RxnOptGoalIterSparqlClient(ChemistryAndRobotsSparqlClient):
    # TODO: implement
    def get_goal_set_instance(self, goal_set_iri) -> GoalSet:
        pass

    # TODO: implement
    def generate_doe_instance_from_goal(self, goal_set: GoalSet, rxn_exp_beliefs: List[ReactionExperiment]) -> DesignOfExperiment:
        list_design_variables = []
        list_system_responses = []
        doe_instance = DesignOfExperiment(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(goal_set.instance_iri),
            # TODO add support for Strategy defined by user
            usesStrategy=TSEMO(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(goal_set.instance_iri),
            ),
            hasDomain=Domain(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(goal_set.instance_iri),
                hasDesignVariable=list_design_variables,
            ),
            hasSystemResponse=list_system_responses,
            utilisesHistoricalData=rxn_exp_beliefs,
        )
        pass

    # TODO: implement
    def detect_postpro_derivation_result(
        self, postpro_derivation_iri: str, interested_performance_indicators: list
    ) -> ReactionExperiment:
        postpro_derivation_iri = trimIRI(postpro_derivation_iri)
        if self.check_if_triple_exist(None, ONTODERIVATION_BELONGSTO, postpro_derivation_iri):            
            query = f"""
                SELECT ?rxn_exp
                WHERE {{
                    VALUES ?rxn_rdfType {{ {ONTOREACTION_REACTIONEXPERIMENT, ONTOREACTION_REACTIONVARIATION} }}
                    <{postpro_derivation_iri}> <{ONTODERIVATION_ISDERIVEDFROM}> ?rxn_exp. 
                    ?rxn_exp a ?rxn_rdfType.
                }}"""
            response = self.performQuery(query)
            if len(response) != 1:
                raise Exception(f"""Exactly one OntoReaction:ReactionExperiment or OntoReaction:ReactionVariation 
                is expected as input of HPLCPostProDerivation {postpro_derivation_iri}, but found: {response}""")
        pass
