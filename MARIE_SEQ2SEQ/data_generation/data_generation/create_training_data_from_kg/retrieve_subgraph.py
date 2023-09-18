from abc import ABC, abstractmethod
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
        property_helper = QueryConstructionHelperProperty(
            tail_num=property_num, value_bindings=bindings
        )
        select_variables.append(property_helper.get_select_variables())
        where_clause_blocks.append(property_helper.get_where_clauses())

        identifier_num = tail_nums.get("identifier_num", 0)
        identifier_helper = QueryConstructionHelperIdentifier(
            tail_num=identifier_num, value_bindings=bindings
        )
        select_variables.append(identifier_helper.get_select_variables())
        where_clause_blocks.append(identifier_helper.get_where_clauses())

        use_num = tail_nums.get("use_num", 0)
        use_helper = QueryConstructionHelperUse(
            tail_num=use_num, value_bindings=bindings
        )
        select_variables.append(use_helper.get_select_variables())
        where_clause_blocks.append(use_helper.get_where_clauses())

        chemicalclass_num = tail_nums.get("chemicalclass_num", 0)
        chemicalclass_helper = QueryConstructionHelperChemicalClass(tail_num=chemicalclass_num, value_bindings=bindings)
        select_variables.append(chemicalclass_helper.get_select_variables())
        where_clause_blocks.append(chemicalclass_helper.get_where_clauses())

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
                dict(
                    type="use",
                    UseIRI=values[f"UseIRI{i}"],
                    UseValue=values[f"UseValue{i}"],
                )
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


class QueryConstructionHelperTail(ABC):
    def __init__(
        self,
        tail_num: int,
        var_prefix_for_mutual_exclusion: str,
        value_bindings: Dict[str, str] = dict(),
    ):
        self.tail_num = tail_num
        self.var_prefix_for_mutual_exclusion = var_prefix_for_mutual_exclusion
        self.value_bindings = value_bindings

    def get_select_variables(self):
        return " ".join(
            [
                self.get_select_variable_per_tail(tail_id)
                for tail_id in range(1, self.tail_num + 1)
            ]
        )

    @abstractmethod
    def get_select_variable_per_tail(self, tail_id: int) -> str:
        pass

    def get_where_clauses(self) -> str:
        clauses = (
            "".join(
                [
                    self.get_where_clauses_per_tail(tail_id)
                    for tail_id in range(1, self.tail_num + 1)
                ]
            )
            + self.get_filter_pairwise_notequal()
        )
        if self.tail_num > 0:
            clauses += "\n"
        return clauses

    @abstractmethod
    def get_where_clauses_per_tail(
        self, tail_id: int, value_binding: Optional[str]
    ) -> str:
        pass

    def get_filter_pairwise_notequal(self) -> str:
        variable_prefix = self.var_prefix_for_mutual_exclusion
        variable_num = self.tail_num

        filter_clauses = []
        for i, j in itertools.combinations(range(1, variable_num + 1), r=2):
            filter_clauses.append(
                f"FILTER ( ?{variable_prefix}{i} != ?{variable_prefix}{j} )"
            )
        filter_clauses_str = f"""
    """.join(
            filter_clauses
        )
        if filter_clauses_str:
            return f"""
    {filter_clauses_str}"""
        return ""


class QueryConstructionHelperProperty(QueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="hasProperty",
            value_bindings=value_bindings,
        )

    def get_select_variable_per_tail(self, tail_id: int):
        return f"?hasProperty{tail_id} ?PropertyIRI{tail_id} ?PropertyValue{tail_id}"

    def get_where_clauses_per_tail(self, tail_id: int):
        has_property_binding = self.value_bindings.get(f"hasProperty{tail_id}")
        has_property_values = (
            [has_property_binding]
            if has_property_binding is not None
            else [f"os:has{p}" for p in PROPERTY_NAMES]
        )
        return f"""
    VALUES (?hasProperty{tail_id}) {{
        {" ".join([f"({val})" for val in has_property_values])}
    }}
    ?SpeciesIRI ?hasProperty{tail_id} ?PropertyIRI{tail_id} .
    ?PropertyIRI{tail_id} os:value ?PropertyValue{tail_id} ."""


class QueryConstructionHelperIdentifier(QueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="hasIdentifier",
            value_bindings=value_bindings,
        )

    def get_select_variable_per_tail(self, tail_id: int):
        return (
            f"?hasIdentifier{tail_id} ?IdentifierIRI{tail_id} ?IdentifierValue{tail_id}"
        )

    def get_where_clauses_per_tail(self, tail_id: int):
        has_identifier_binding = self.value_bindings.get(f"hasIdentifier{tail_id}")
        has_identifier_values = (
            [has_identifier_binding]
            if has_identifier_binding is not None
            else [f"os:has{i}" for i in IDENTIFIER_NAMES]
        )
        return f"""
    VALUES (?hasIdentifier{tail_id}) {{
        {" ".join([f"({val})" for val in has_identifier_values])}
    }}
    ?SpeciesIRI ?hasIdentifier{tail_id} ?IdentifierIRI{tail_id} .
    ?IdentifierIRI{tail_id} os:value ?IdentifierValue{tail_id} ."""


class QueryConstructionHelperUse(QueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="UseValue",
            value_bindings=value_bindings,
        )

    def get_select_variable_per_tail(self, tail_id: int):
        return f"?UseIRI{tail_id} ?UseValue{tail_id}"

    def get_where_clauses_per_tail(self, tail_id: int):
        use_value_binding = self.value_bindings.get(f"UseValue{tail_id}")
        value_binding_str = (
            f"""
    VALUES (?UseValue{tail_id}) {{
        (\"{use_value_binding}\")
    }}"""
            if use_value_binding is not None
            else ""
        )

        return (
            value_binding_str
            + f"""
    ?SpeciesIRI os:hasUse ?UseIRI{tail_id} .
    ?UseIRI{tail_id} rdfs:label ?UseValue{tail_id} ."""
        )

class QueryConstructionHelperChemicalClass(QueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="ChemicalClassValue",
            value_bindings=value_bindings,
        )

    def get_select_variable_per_tail(self, tail_id: int):
        return f"?ChemicalClassIRI{tail_id} ?ChemicalClassValue{tail_id}"

    def get_where_clauses_per_tail(self, tail_id: int):
        chemicalclass_value_binding = self.value_bindings.get(f"ChemicalClassValue{tail_id}")
        value_binding_str = (
            f"""
    VALUES (?ChemicalClassValue{tail_id}) {{
        (\"{chemicalclass_value_binding}\")
    }}"""
            if chemicalclass_value_binding is not None
            else ""
        )

        return (
            value_binding_str
            + f"""
    ?SpeciesIRI os:hasChemicalClass* ?x{tail_id} .
    ?x{tail_id} ?y{tail_id} ?z{tail_id} .
    ?z{tail_id} rdfs:subClassOf* ?ChemicalClassIRI{tail_id} .
    ?ChemicalClassIRI{tail_id} rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue{tail_id} ."""
        )