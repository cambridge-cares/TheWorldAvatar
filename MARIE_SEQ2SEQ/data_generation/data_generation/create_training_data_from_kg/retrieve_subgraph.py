import random
from typing import Dict, List, Optional

from SPARQLWrapper import SPARQLWrapper, JSON, POST

from data_generation.constants import (
    IDENTIFIER_NAMES,
    PROPERTY_NAMES,
)


class SubgraphRetriever:
    PREFIXES = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"""
    IDENTIFIER_NAMES_FOR_HEAD = [
        "InChI",
        "IUPACName",
        "MolecularFormula",
        "SMILES",
    ]

    def __init__(
        self,
        kg_endpoint: str = "http://theworldavatar.com/blazegraph/namespace/copy_ontospecies_pubchem/sparql",
    ):
        sparql_client = SPARQLWrapper(kg_endpoint)
        sparql_client.setReturnFormat(JSON)
        sparql_client.setMethod(POST)
        self.sparql_client = sparql_client

    def get_subgraph(
        self,
        tail_nums: Optional[Dict[str, int]] = None,
        bindings: Optional[Dict[str, str]] = None,
    ):
        results = []

        try_num = 0
        try_limit = 5 if not bindings else 1
        while len(results) == 0 and try_num < try_limit:
            if tail_nums is None:
                tail_nums = self.get_rand_tail_nums()
            sparql_query = self.get_subgraph_query(tail_nums, bindings)

            self.sparql_client.setQuery(sparql_query)
            results = self.sparql_client.queryAndConvert()["results"]["bindings"]
            try_num += 1

        if len(results) == 0:
            return None

        return self.convert_kg_results_to_subgraph(results[0])

    def get_rand_tail_nums(self):
        property_num = random.randint(0, 3)
        identifier_num = random.randint(property_num, 3) - property_num

        id_prop_num = identifier_num + property_num
        if id_prop_num == 0:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        elif id_prop_num == 1:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0), (1, 1)])
        elif id_prop_num == 2:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0)])
        else:
            use_num, chemicalclass_num = 0, 0

        return dict(
            property_num=property_num,
            identifier_num=identifier_num,
            use_num=use_num,
            chemicalclass_num=chemicalclass_num,
        )

    def get_where_species(self, identifier_name: str):
        return f"""
    ?SpeciesIRI a os:Species ; os:has{identifier_name} ?IdentifierIRI .
    ?IdentifierIRI os:value ?IdentifierValue ."""

    def get_where_property(self, i: int, has_property_binding: Optional[str] = None):
        has_property_values = (
            [has_property_binding]
            if has_property_binding is not None
            else [f"os:has{p}" for p in PROPERTY_NAMES]
        )
        return f"""
    VALUES (?hasProperty{i}) {{
        {" ".join([f"({val})" for val in has_property_values])}
    }}
    ?SpeciesIRI ?hasProperty{i} ?PropertyIRI{i} .
    ?PropertyIRI{i} os:value ?PropertyValue{i} ."""

    def get_where_property_filter(self, property_num: int):
        if property_num < 2:
            return ""

        if property_num == 2:
            return """
    FILTER ( ?hasProperty1 != ?hasProperty2 )"""

        if property_num == 3:
            return """
    FILTER ( ?hasProperty1 != ?hasProperty2 )
    FILTER ( ?hasProperty1 != ?hasProperty3 )
    FILTER ( ?hasProperty2 != ?hasProperty3 )"""

        raise ValueError(
            f"`property_num` is expected to be in the range of [0, 3] inclusive, but got {property_num}."
        )

    def get_where_identifier(
        self, i: int, has_identifier_binding: Optional[str] = None
    ):
        has_identifier_values = (
            [has_identifier_binding]
            if has_identifier_binding is not None
            else [f"os:has{i}" for i in IDENTIFIER_NAMES]
        )
        return f"""
    VALUES (?hasIdentifier{i}) {{
        {" ".join([f"({val})" for val in has_identifier_values])}
    }}
    ?SpeciesIRI ?hasIdentifier{i} ?IdentifierIRI{i} .
    ?IdentifierIRI{i} os:value ?IdentifierValue{i} ."""

    def get_where_identifier_filter(self, identifier_num: int):
        if identifier_num < 2:
            return ""

        if identifier_num == 2:
            return """
    FILTER ( ?hasIdentifier1 != ?hasIdentifier2 )"""

        if identifier_num == 3:
            return """
    FILTER ( ?hasIdentifier1 != ?hasIdentifier2 )
    FILTER ( ?hasIdentifier1 != ?hasIdentifier3 )
    FILTER ( ?hasIdentifier2 != ?hasIdentifier3 )"""

        raise ValueError(
            f"`identifier_num` is expected to be in the range of [0, 3] inclusive, but got {identifier_num}."
        )

    def get_where_use(self, use_value_binding: Optional[str] = None):
        value_binding_str = (
            f"""
    VALUES (?UseValue) {{
        (\"{use_value_binding}\")
    }}"""
            if use_value_binding is not None
            else ""
        )

        return (
            value_binding_str
            + """?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI rdfs:label ?UseValue ."""
        )

    def get_where_chemicalclass(
        self, chemicalclass_value_binding: Optional[str] = None
    ):
        value_binding_str = (
            f"""
    VALUES (?ChemicalClassValue) {{
        (\"{chemicalclass_value_binding}\")
    }}"""
            if chemicalclass_value_binding is not None
            else ""
        )

        return (
            value_binding_str
            + """?SpeciesIRI os:hasChemicalClass* ?x .
    ?x ?y ?z .
    ?z rdfs:subClassOf* ?ChemicalClassIRI .
    ?ChemicalClassIRI rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue ."""
        )

    def get_subgraph_query(
        self, tail_nums: Dict[str, int], bindings: Optional[dict] = None
    ):
        identifier_name = random.choice(self.IDENTIFIER_NAMES_FOR_HEAD)

        select_variables = ["?SpeciesIRI", "?IdentifierIRI", "?IdentifierValue"]
        where_clause_blocks = [self.get_where_species(identifier_name)]

        for i in range(1, tail_nums["property_num"] + 1):
            select_variables.extend(
                [
                    f"?hasProperty{i}",
                    f"?PropertyIRI{i}",
                    f"?PropertyValue{i}",
                ]
            )
            where_clause_blocks.append(
                self.get_where_property(
                    i, has_property_binding=bindings.get(f"hasProperty{i}")
                )
            )
        where_clause_blocks.append(
            self.get_where_property_filter(tail_nums["property_num"])
        )

        for i in range(1, tail_nums["identifier_num"] + 1):
            select_variables.extend(
                [f"?hasIdentifier{i}", f"?IdentifierIRI{i}", f"?IdentifierValue{i}"]
            )
            where_clause_blocks.append(
                self.get_where_identifier(
                    i, has_identifier_binding=bindings.get(f"hasIdentifier{i}")
                )
            )
        where_clause_blocks.append(
            self.get_where_identifier_filter(tail_nums["identifier_num"])
        )

        if tail_nums["use_num"] > 0:
            select_variables.extend(["?UseIRI", "?UseValue"])
            where_clause_blocks.append(
                self.get_where_use(use_value_binding=bindings.get("UseValue"))
            )

        if tail_nums["chemicalclass_num"] > 0:
            select_variables.extend(["?ChemicalClassIRI", "?ChemicalClassValue"])
            where_clause_blocks.append(
                self.get_where_chemicalclass(
                    chemicalclass_value_binding=bindings.get("ChemicalClassValue")
                )
            )

        return f"""{self.PREFIXES}

SELECT {" ".join(select_variables)} 
WHERE {{{"".join(where_clause_blocks)}
}}
LIMIT 1"""

    def convert_kg_results_to_subgraph(self, valueset: Dict[str, Dict[str, str]]):
        values = {k: v["value"] for k, v in valueset.items()}

        head = dict(
            SpeciesIRI=values["SpeciesIRI"],
            IdentifierIRI=values["IdentifierIRI"],
            IdentifierValue=values["IdentifierValue"],
        )

        tails = []

        for i in range(1, 4):
            if f"PropertyIRI{i}" not in values:
                break
            tails.append(
                dict(
                    type="property",
                    PropertyName=values[f"hasProperty{i}"].split("#", maxsplit=1)[-1][
                        len("has") :
                    ],
                    PropertyIRI=values[f"PropertyIRI{i}"],
                    PropertyValue=values[f"PropertyValue{i}"],
                )
            )

        for i in range(1, 4):
            if f"IdentifierIRI{i}" not in values:
                break
            tails.append(
                dict(
                    type="identifier",
                    IdentifierName=values[f"hasIdentifier{i}"].split("#", maxsplit=1)[
                        -1
                    ][len("has") :],
                    IdentifierIRI=values[f"IdentifierIRI{i}"],
                    IdentifierValue=values[f"IdentifierValue{i}"],
                )
            )

        if "UseIRI" in values:
            tails.append(
                dict(type="use", UseIRI=values["UseIRI"], UseValue=values["UseValue"])
            )

        if "ChemicalClassIRI" in values:
            tails.append(
                dict(
                    type="chemicalclass",
                    ChemicalClassIRI=values["ChemicalClassIRI"],
                    ChemicalClassValue=values["ChemicalClassValue"],
                )
            )

        return dict(head=head, tails=tails)
