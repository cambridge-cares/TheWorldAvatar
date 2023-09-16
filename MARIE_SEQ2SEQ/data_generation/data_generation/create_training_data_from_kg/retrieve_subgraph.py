from collections import defaultdict
import itertools
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
            print(sparql_query)

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

    def get_rand_tail_nums_non_identifier(self):
        property_num = random.randint(0, 3)
        use_num = random.randint(property_num, 3) - property_num
        chemicalclass_num = (
            random.randint(property_num + use_num, 3) - property_num - use_num
        )
        return dict(
            property_num=property_num,
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

    def get_where_filter(self, variable_prefix: str, variable_num: int):
        filter_clauses = []
        for i, j in itertools.combinations(range(1, variable_num + 1), r=2):
            filter_clauses.append(
                f"FILTER ( ?{variable_prefix}{i} != ?{variable_prefix}{j} )"
            )
        filter_clauses_str = f"""
    """.join(filter_clauses)
        if filter_clauses_str:
            return f"""
    {filter_clauses_str}"""
        return ""

    def get_where_property_filter(self, property_num: int):
        return self.get_where_filter(
            variable_prefix="hasProperty", variable_num=property_num
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
        return self.get_where_filter(
            variable_prefix="hasIdentifier", variable_num=identifier_num
        )

    def get_where_use(self, i: int, use_value_binding: Optional[str] = None):
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
            + f"""
    ?SpeciesIRI os:hasUse ?UseIRI{i} .
    ?UseIRI{i} rdfs:label ?UseValue{i} ."""
        )

    def get_where_use_filter(self, use_num: int):
        return self.get_where_filter(variable_prefix="UseValue", variable_num=use_num)

    def get_where_chemicalclass(
        self, i: int, chemicalclass_value_binding: Optional[str] = None
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
            + f"""
    ?SpeciesIRI os:hasChemicalClass* ?x{i} .
    ?x{i} ?y{i} ?z{i} .
    ?z rdfs:subClassOf* ?ChemicalClassIRI{i} .
    ?ChemicalClassIRI{i} rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue{i} ."""
        )

    def get_where_chemicalclass_filter(self, chemicalclass_num: int):
        return self.get_where_filter(
            variable_prefix="ChemicalClassValue", variable_num=chemicalclass_num
        )

    def get_subgraph_query(
        self, tail_nums: Dict[str, int], bindings: Optional[dict] = None
    ):
        if bindings is None:
            bindings = dict()
        identifier_name = random.choice(self.IDENTIFIER_NAMES_FOR_HEAD)

        select_variables = ["?SpeciesIRI ?IdentifierIRI ?IdentifierValue"]
        where_clause_blocks = [self.get_where_species(identifier_name)]
        if any(x > 0 for x in tail_nums.values()):
            where_clause_blocks.append("\n")

        property_num = tail_nums.get("property_num", 0)
        for i in range(1, property_num + 1):
            select_variables.append(
                f"?hasProperty{i} ?PropertyIRI{i} ?PropertyValue{i}"
            )
            where_clause_blocks.append(
                self.get_where_property(
                    i, has_property_binding=bindings.get(f"hasProperty{i}")
                )
            )
        where_clause_blocks.append(self.get_where_property_filter(property_num))
        if property_num > 0:
            where_clause_blocks.append("\n")

        identifier_num = tail_nums.get("identifier_num", 0)
        for i in range(1, identifier_num + 1):
            select_variables.append(
                f"?hasIdentifier{i} ?IdentifierIRI{i} ?IdentifierValue{i}"
            )
            where_clause_blocks.append(
                self.get_where_identifier(
                    i, has_identifier_binding=bindings.get(f"hasIdentifier{i}")
                )
            )
        where_clause_blocks.append(self.get_where_identifier_filter(identifier_num))
        if identifier_num > 0:
            where_clause_blocks.append("\n")

        use_num = tail_nums.get("use_num", 0)
        for i in range(1, use_num + 1):
            select_variables.append(f"?UseIRI{i} ?UseValue{i}")
            where_clause_blocks.append(
                self.get_where_use(i, use_value_binding=bindings.get(f"UseValue{i}"))
            )
        where_clause_blocks.append(self.get_where_use_filter(use_num))
        if use_num > 0:
            where_clause_blocks.append("\n")

        chemicalclass_num = tail_nums.get("chemicalclass_num", 0)
        for i in range(1, chemicalclass_num + 1):
            select_variables.append(f"?ChemicalClassIRI{i} ?ChemicalClassValue{i}")
            where_clause_blocks.append(
                self.get_where_chemicalclass(
                    i,
                    chemicalclass_value_binding=bindings.get(f"ChemicalClassValue{i}"),
                )
            )
        where_clause_blocks.append(
            self.get_where_chemicalclass_filter(chemicalclass_num)
        )
        if chemicalclass_num > 0:
            where_clause_blocks.append("\n")

        return f"""{self.PREFIXES}

SELECT {" ".join(select_variables)} 
WHERE {{{"".join(where_clause_blocks).rstrip()}
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

        for i in range(1, 4):
            if f"UseIRI{i}" not in values:
                break
            tails.append(
                dict(type="use", UseIRI=values[f"UseIRI{i}"], UseValue=values[f"UseValue{i}"])
            )

        for i in range(1, 4):
            if f"ChemicalClassIRI{i}" not in values:
                break
            tails.append(
                dict(
                    type="chemicalclass",
                    ChemicalClassIRI=values[f"ChemicalClassIRI{i}"],
                    ChemicalClassValue=values[f"ChemicalClassValue{i}"],
                )
            )

        return dict(head=head, tails=tails)
