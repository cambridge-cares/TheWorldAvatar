from abc import ABC, abstractmethod
import itertools
import random
from typing import Dict, List, Optional

from data_generation.constants import IDENTIFIER_NAMES, PROPERTY_NAMES


class RetrieveQueryConstructor:
    PREFIXES = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"""
    IDENTIFIER_NAMES_FOR_HEAD = [
        "InChI",
        "IUPACName",
        "MolecularFormula",
        "SMILES",
    ]

    def construct(self, tail_nums: Dict[str, int], bindings: Optional[dict] = None):
        if bindings is None:
            bindings = dict()

        head_helper = RetrieveQueryConstructionHelperHead(
            identifier_name=random.choice(self.IDENTIFIER_NAMES_FOR_HEAD)
        )
        property_helper = RetrieveQueryConstructionHelperProperty(
            tail_num=tail_nums.get("property_num", 0), value_bindings=bindings
        )
        identifier_helper = RetrieveQueryConstructionHelperIdentifier(
            tail_num=tail_nums.get("identifier_num", 0), value_bindings=bindings
        )
        use_helper = RetrieveQueryConstructionHelperUse(
            tail_num=tail_nums.get("use_num", 0), value_bindings=bindings
        )
        chemicalclass_helper = RetrieveQueryConstructionHelperChemicalClass(
            tail_num=tail_nums.get("chemicalclass_num", 0), value_bindings=bindings
        )

        helpers: List[RetrieveQueryConstructionHelper] = [
            head_helper,
            property_helper,
            identifier_helper,
            use_helper,
            chemicalclass_helper,
        ]

        select_variables = [helper.get_select_variables() for helper in helpers]
        where_clause = [helper.get_where_clauses() for helper in helpers]

        return f"""{self.PREFIXES}

SELECT {" ".join(select_variables)} 
WHERE {{{"".join(where_clause).rstrip()}
}}
LIMIT 1"""


class RetrieveQueryConstructionHelper(ABC):
    @abstractmethod
    def get_select_variables(self) -> str:
        pass

    @abstractmethod
    def get_where_clauses(self) -> str:
        pass


class RetrieveQueryConstructionHelperHead(RetrieveQueryConstructionHelper):
    def __init__(self, identifier_name: str):
        self.identifier_name = identifier_name

    def get_select_variables(self):
        return "?SpeciesIRI ?IdentifierIRI ?IdentifierValue"

    def get_where_clauses(self) -> str:
        return f"""
    ?SpeciesIRI a os:Species ; os:has{self.identifier_name} ?IdentifierIRI .
    ?IdentifierIRI os:value ?IdentifierValue .
"""


class RetrieveQueryConstructionHelperTail(RetrieveQueryConstructionHelper, ABC):
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
                self.get_select_variables_per_tail(tail_id)
                for tail_id in range(1, self.tail_num + 1)
            ]
        )

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

    @abstractmethod
    def get_select_variables_per_tail(self, tail_id: int) -> str:
        pass

    @abstractmethod
    def get_where_clauses_per_tail(
        self, tail_id: int, value_binding: Optional[str]
    ) -> str:
        pass


class RetrieveQueryConstructionHelperProperty(RetrieveQueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="hasProperty",
            value_bindings=value_bindings,
        )

    def get_select_variables_per_tail(self, tail_id: int):
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


class RetrieveQueryConstructionHelperIdentifier(RetrieveQueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="hasIdentifier",
            value_bindings=value_bindings,
        )

    def get_select_variables_per_tail(self, tail_id: int):
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


class RetrieveQueryConstructionHelperUse(RetrieveQueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="UseValue",
            value_bindings=value_bindings,
        )

    def get_select_variables_per_tail(self, tail_id: int):
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


class RetrieveQueryConstructionHelperChemicalClass(RetrieveQueryConstructionHelperTail):
    def __init__(self, tail_num: int, value_bindings: Dict[str, str] = dict()):
        super().__init__(
            tail_num,
            var_prefix_for_mutual_exclusion="ChemicalClassValue",
            value_bindings=value_bindings,
        )

    def get_select_variables_per_tail(self, tail_id: int):
        return f"?ChemicalClassIRI{tail_id} ?ChemicalClassValue{tail_id}"

    def get_where_clauses_per_tail(self, tail_id: int):
        chemicalclass_value_binding = self.value_bindings.get(
            f"ChemicalClassValue{tail_id}"
        )
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
