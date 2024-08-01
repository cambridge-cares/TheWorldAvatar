from locate_then_ask.graph2sparql import Graph2Sparql


class OCCGraph2Sparql(Graph2Sparql):
    def __init__(self):
        super().__init__(
            predicates_to_entities_linked_by_rdfslabel=[
                "occ:hasSpeciesModel/occ:hasSpecies"
            ]
        )
