import logging
from typing import Annotated, Dict, List, Tuple

from fastapi import Depends

from model.qa import QAData
from model.constraint import CompoundNumericalConstraint
from services.kg_client import KgClient
from ..kg_client import get_ontospecies_kg_client
from ..constants import (
    SpeciesAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesIdentifierAttrKey,
    SpeciesPropertyAttrKey,
    SpeciesUseAttrKey,
)
from .link_entity import SpeciesLinker, get_species_linker
from .align import OntoSpeciesLiteralAligner, get_ontospecies_literal_aligner

logger = logging.getLogger(__name__)


class OntoSpeciesAgent:
    def __init__(
        self,
        kg_client: KgClient,
        species_linker: SpeciesLinker,
        ontospecies_literal_aligner: OntoSpeciesLiteralAligner,
    ):
        self.kg_client = kg_client
        self.species_linker = species_linker
        self.aligner = ontospecies_literal_aligner

    def lookup_iri_attribute(
        self, species_iri: str, attr_key: SpeciesAttrKey
    ) -> List[Dict[str, str]]:
        if isinstance(attr_key, SpeciesUseAttrKey):
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Use WHERE {{
    OPTIONAL {{
        <{IRI}> os:hasUse/rdfs:label ?Use .
    }}
}}"""
        elif isinstance(attr_key, SpeciesChemicalClassAttrKey):
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?ChemicalClass WHERE {{
    OPTIONAL {{
        <{IRI}> (a|!a)+ [ a os:ChemicalClass ; rdfs:label ?ChemicalClass ] .
    }}
}}"""
        elif isinstance(attr_key, SpeciesIdentifierAttrKey):
            template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?{key} WHERE {{
    OPTIONAL {{
        <{IRI}> os:has{key}/os:value ?{key} .
    }}
}}"""
        else:
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Value ?Unit ?ReferenceStateValue ?ReferenceStateUnit WHERE {{
    OPTIONAL {{
        <{IRI}> os:has{key} ?{key} .
        ?{key} os:value ?Value ; os:unit/rdfs:label ?Unit .
        OPTIONAL {{
            ?{key} os:hasReferenceState [ os:value ?ReferenceStateValue ; os:unit/rdfs:label ?ReferenceStateUnit ] .
        }}
    }}
}}"""
        query = template.format(key=attr_key.value, IRI=species_iri)
        return [
            {k: v["value"] for k, v in x.items()}
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def lookup_chemicalSpecies_attributes(
        self, species: str, attr_keys: List[SpeciesAttrKey]
    ):
        species_iris = self.species_linker.link(species)

        vars = ["IRI"] + [key.value for key in attr_keys]
        bindings: List[dict] = []
        for iri in species_iris:
            datum = dict(IRI=iri)
            for key in attr_keys:
                datum[key.value] = self.lookup_iri_attribute(iri, key)
            bindings.append(datum)

        return QAData(vars=vars, bindings=bindings)

    def lookup_iri_label(self, species_iri: str):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        
SELECT ?Label WHERE {{
    <{IRI}> rdfs:label ?Label .
}}""".format(
            IRI=species_iri
        )
        return self.kg_client.query(query)["results"]["bindings"][0]["Label"]["value"]

    def find_chemicalSpecies(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[
            Tuple[SpeciesPropertyAttrKey, CompoundNumericalConstraint]
        ] = [],
    ):
        vars = []
        patterns = []

        if chemical_classes:
            logger.info("Aligning chemical class labels...")
            aligned_chemical_classes = self.aligner.align_chemical_classes(
                chemical_classes
            )
            logger.info("Aligned chemical classes: " + str(aligned_chemical_classes))

            for i, chemclass_options in enumerate(aligned_chemical_classes):
                varnode = "?ChemicalClassLabel" + str(i)
                vars.append(varnode)
                values = ['"{val}"'.format(val=val) for val in chemclass_options]
                patterns.extend(
                    [
                        "VALUES {varnode} {{ {values} }}".format(
                            varnode=varnode, values=" ".join(values)
                        ),
                        "?IRI (a|!a)+ [ a os:ChemicalClass ; rdfs:label {varnode} ] .".format(
                            varnode=varnode
                        ),
                    ]
                )

        if uses:
            logger.info("Aligning use labels...")
            aligned_uses = self.aligner.align_uses(uses)
            logger.info("Aligned uses: " + str(aligned_uses))

            for i, use_options in enumerate(aligned_uses):
                varnode = "?UseLabel" + str(i)
                vars.append(varnode)
                values = ['"{val}"'.format(val=val) for val in use_options]
                patterns.extend(
                    [
                        "VALUES {varnode} {{ {values} }}".format(
                            varnode=varnode, values=" ".join(values)
                        ),
                        "?IRI os:hasUse/rdfs:label {varnode} .".format(varnode=varnode),
                    ]
                )

        for key, compound_constraint in properties:
            patterns.extend(
                [
                    "?IRI os:has{key} ?{key} .".format(key=key.value),
                    "?{key} os:value ?{key}Value .".format(key=key.value),
                ]
            )
            vars.append("?" + key.value)
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

SELECT DISTINCT ?IRI (MIN(?Label) AS ?label) {vars} WHERE {{
?IRI rdfs:label ?Label .
{patterns}
}}
GROUP BY ?IRI""".format(
            vars=" ".join(
                ["(SAMPLE({var}) AS {var}Sample)".format(var=var) for var in vars]
            ),
            patterns="\n".join(patterns),
        )

        logger.info("SPARQL query: " + query)
        response = self.kg_client.query(query)

        # Combine use and chemical class values into a single field each
        vars = response["head"]["vars"]
        chemclass_vars = [x for x in vars if x.startswith("ChemicalClass")]
        use_vars = [x for x in vars if x.startswith("Use")]
        property_vars = [
            x
            for x in vars
            if any(x.startswith(k.value) for k in SpeciesPropertyAttrKey)
        ]
        property_keys = [
            next(k for k in SpeciesPropertyAttrKey if x.startswith(k.value))
            for x in property_vars
        ]
        other_vars = [
            x
            for x in vars
            if x not in chemclass_vars and x not in use_vars and x not in property_vars
        ]

        vars = list(other_vars)
        if chemclass_vars:
            vars.append("ChemicalClass")
        if use_vars:
            vars.append("Use")
        vars.extend([x.value for x in property_keys])

        bindings = []
        for binding in response["results"]["bindings"]:
            binding = {k: v["value"] for k, v in binding.items()}
            datum = {k: v for k, v in binding.items() if k in other_vars}
            if chemclass_vars:
                datum["ChemicalClass"] = ", ".join(
                    [v for k, v in binding.items() if k in chemclass_vars]
                )
            if use_vars:
                datum["Use"] = ", ".join(
                    [v for k, v in binding.items() if k in use_vars]
                )
            for pkey, pvar in zip(property_keys, property_vars):
                # query for value, unit, ref value, ref unit
                query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Value ?Unit ?RefStateValue ?RefStateUnit WHERE {{
    <{IRI}> os:value ?Value ; os:unit/rdfs:label ?Unit .
    OPTIONAL {{
        <{IRI}> os:hasReferenceState [ os:value ?RefStateValue ; os:unit/rdfs:label ?RefStateUnit ]
    }}
}}
LIMIT 1""".format(
                    IRI=binding[pvar]
                )
                _bindings = self.kg_client.query(query)["results"]["bindings"]
                if not _bindings:
                    continue
                _binding = {k: v["value"] for k, v in _bindings[0].items()}
                datum[pkey.value] = dict(
                    value=_binding["Value"],
                    unit=_binding["Unit"],
                    ref_state_value=_binding["RefStateValue"],
                    ref_state_unit=_binding["RefStateUnit"],
                )

            bindings.append(datum)

        return QAData(vars=vars, bindings=bindings)


def get_ontospecies_agent(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    species_linker: Annotated[SpeciesLinker, Depends(get_species_linker)],
    ontospecies_literal_aligner: Annotated[
        OntoSpeciesLiteralAligner, Depends(get_ontospecies_literal_aligner)
    ],
):
    return OntoSpeciesAgent(
        kg_client=kg_client,
        species_linker=species_linker,
        ontospecies_literal_aligner=ontospecies_literal_aligner,
    )
