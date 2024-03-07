import logging
from typing import List, Tuple

from model.constraint import CompoundNumericalConstraint
from .constants import SpeciesPropertyAttrKey
from .kg_client import get_ontospecies_kg_client


logger = logging.getLogger(__name__)


class SpeciesFinder:
    def __init__(self):
        self.kg_client = get_ontospecies_kg_client()

    def find(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[
            Tuple[SpeciesPropertyAttrKey, CompoundNumericalConstraint]
        ] = [],
    ) -> List[str]:
        patterns = []

        for chemical_class in chemical_classes:
            patterns.append(
                '?Species (a|!a)+ [ a os:ChemicalClass ; rdfs:label "{label}" ] .'.format(
                    label=chemical_class
                )
            )

        for use in uses:
            patterns.append(
                '?Species os:hasUse/rdfs:label "{label}" .'.format(label=use)
            )

        for key, compound_constraint in properties:
            patterns.append(
                "?Species os:has{key}/os:value ?{key}Value .".format(key=key.value)
            )
            atomic_constraints = [
                "?{key}Value {operator} {operand}".format(
                    key=key.value, operator=x.operator.value, operand=x.operand
                )
                for x in compound_constraint.constraints
            ]
            if compound_constraint.logical_operator:
                delimiter = compound_constraint.logical_operator.value
            else:
                delimiter = "&&"
            exprn = delimiter.join(atomic_constraints)
            patterns.append("FILTER ( {exprn} )".format(exprn=exprn))

        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Species WHERE {{
{patterns}
}}""".format(
            patterns="\n".join(patterns)
        )

        logger.info("SPARQL query: " + query)

        return [
            x["Species"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]
