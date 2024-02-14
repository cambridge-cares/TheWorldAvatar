from decimal import Decimal
from typing import Dict

from locate_then_ask.kg_client import KgClient
from locate_then_ask.model import OmMeasure
from locate_then_ask.singapore.model import OPltPlot


class SgEntityStore:
    def __init__(self, bg_endpoint: str, ontop_endpoint: str):
        self.kg_client = KgClient(bg_endpoint)
        self.ontop_endpoint = ontop_endpoint
        self.iri2entity: Dict[str, OPltPlot] = dict()

    def get(self, entity_iri: str):
        if entity_iri not in self.iri2entity:
            self.iri2entity[entity_iri] = OPltPlot(
                land_use_type=self.retrieve_landUseType(entity_iri),
                gross_plot_ratio=self.retrieve_omMeasure_byPredicate(
                    entity_iri, predicate="^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue"
                ),
                is_awaiting_detailed_gpr_eval=self.retrieve_isAwaitingDetailedGPREval(
                    entity_iri
                ),
                plot_area=self.retrieve_omMeasure_byPredicate(
                    entity_iri, predicate="oplt:hasPlotArea/om:hasValue"
                ),
                gross_floor_area=self.retrieve_omMeasure_byPredicate(
                    entity_iri, predicate="oplt:hasMaximumPermittedGPR/om:hasValue"
                ),
            )
        return self.iri2entity[entity_iri]

    def retrieve_landUseType(self, entity_iri: str):
        template = """PREFIX ozng: <https://www.theworldavatar.com/kg/ontozoning/>

SELECT DISTINCT ?LandUseTypeType WHERE {{
    SERVICE <{ontop}> {{
        <{IRI}> ozng:hasLandUseType ?LandUseType .
    }}
    ?LandUseType a ?LandUseTypeType .
}}"""
        query = template.format(ontop=self.ontop_endpoint, IRI=entity_iri)
        bindings = self.kg_client.query(query)["results"]["bindings"]

        if len(bindings) == 0:
            return None
        if len(bindings) > 1:
            print("Found more than 1 land use type: ", entity_iri)
        return bindings[0]["LandUseTypeType"]["value"]

    def retrieve_isAwaitingDetailedGPREval(self, entity_iri: str):
        template = """PREFIX  oplnrgl: <https://www.theworldavatar.com/kg/ontoplanningregulation/>

SELECT DISTINCT ?IsAwaitingDetailedGPREval WHERE {{
    SERVICE <{ontop}> {{
        <{IRI}> oplnrgl:isAwaitingDetailedGPREvaluation ?IsAwaitingDetailedGPREval
    }}
}}
"""
        query = template.format(ontop=self.ontop_endpoint, IRI=entity_iri)
        bindings = self.kg_client.query(query)["results"]["bindings"]

        if len(bindings) == 0:
            return None
        val = bindings[0]["IsAwaitingDetailedGPREval"]["value"]
        if val == "true":
            return True
        elif val == "false":
            return False
        else:
            raise Exception("Unexpected is_awaiting_detailed_gpr_eval:", bindings)

    def retrieve_omMeasure_byPredicate(self, entity_iri: str, predicate: str):
        query_template = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX oplt: <https://www.theworldavatar.com/kg/ontoplot/>
PREFIX oplnrgl: <https://www.theworldavatar.com/kg/ontoplanningregulation/>

SELECT DISTINCT * WHERE {{
    SERVICE <{ontop}> {{
        <{IRI}> {pred} [
            om:hasNumericalValue ?NumericalValue ;
            om:hasUnit ?Unit
        ]
    }}
}}
LIMIT 1"""
        query = query_template.format(
            ontop=self.ontop_endpoint, IRI=entity_iri, pred=predicate
        )

        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        if len(response_bindings) == 0:
            return None
        return OmMeasure(
            numerical_value=Decimal(response_bindings[0]["NumericalValue"]["value"]),
            unit_iri=response_bindings[0]["Unit"]["value"],
        )
