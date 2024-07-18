from abc import ABC, abstractmethod
from typing import Dict, Optional, Union, List
import random

from data_generation.constants import IDENTIFIER_LABELS, PROPERTY_LABELS
from .constants import IMPERATIVE, INTERROGATIVE, KEYWORD, VERBALIZATION_TYPE
from .exceptions import UnexpectedVerbalizationType
from .utils import tails_to_tail_nums


class ExampleMakerHead2Tail:
    def make_example(self, subgraphs: List[Optional[Dict[str, Union[dict, List[dict]]]]]):
        if len(subgraphs) == 0:
            return None

        if len(subgraphs) == 1:
            return self.make_example_1h(subgraphs[0])

        if len(subgraphs) == 2:
            return self.make_example_2h(subgraph1=subgraphs[0], subgraph2=subgraphs[1])

        raise ValueError(
            "Currently only construction of queries with 1 or 2 heads is supported."
        )

    def make_example_1h(self, subgraph: Optional[Dict[str, Union[dict, List[dict]]]]):
        if subgraph is None:
            return None
        
        head_helper = ExampleH2TQueryConstructorHelperHead(
            identifier_values=[subgraph["head"]["IdentifierValue"]]
        )
        tails: List[dict] = list(subgraph["tails"])
        random.shuffle(tails)
        tail_helpers: List[ExampleH2TQueryConstructorHelperTail] = [
            self.resolve_tail_helper(tail) for tail in tails
        ]
        all_helpers: List[ExampleH2TQueryConstructorHelper] = [
            head_helper
        ] + tail_helpers

        select_variables = [helper.get_select_variables() for helper in all_helpers]
        select_variables_compact = [
            helper.get_select_variables_compact() for helper in all_helpers
        ]
        where_clauses = [helper.get_where_clauses() for helper in all_helpers]
        where_clauses_compact = [
            helper.get_where_clauses_compact() for helper in all_helpers
        ]
        ask_items = [helper.get_ask_item() for helper in tail_helpers]

        sparql_query = f"""SELECT DISTINCT {" ".join([x for x in select_variables if x])}
WHERE {{{"".join(where_clauses).rstrip()}
}}"""
        sparql_query_compact = f"""SELECT {" ".join([x for x in select_variables_compact if x])}
WHERE {{{"".join(where_clauses_compact)}
}}"""

        verbalization_type = random.choice(["interrogative", "imperative", "keyword"])
        return dict(
            verbalization=self.make_verbalization(ask_items, verbalization_type=verbalization_type),
            verbalization_type = verbalization_type,
            bindings=dict(species=subgraph["head"]["IdentifierValue"]),
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
        )

    def make_example_2h(
        self,
        subgraph1: Optional[Dict[str, Union[dict, List[dict]]]],
        subgraph2: Optional[Dict[str, Union[dict, List[dict]]]],
    ):
        if subgraph1 is None or subgraph2 is None:
            return None
        
        if not self.are_subgraphs_congruent(subgraph1, subgraph2):
            raise ValueError(
                f"Two subgraphs provided have different set of tails.\n\nSubgraph 1:\n{subgraph1}\n\nSubgraph 2:\n{subgraph2}"
            )

        head_helper = ExampleH2TQueryConstructorHelperHead(
            identifier_values=[
                subgraph1["head"]["IdentifierValue"],
                subgraph2["head"]["IdentifierValue"],
            ]
        )
        tails: List[dict] = list(subgraph1["tails"])
        random.shuffle(tails)
        tail_helpers: List[ExampleH2TQueryConstructorHelperTail] = [
            self.resolve_tail_helper(tail) for tail in tails
        ]
        all_helpers: List[ExampleH2TQueryConstructorHelper] = [
            head_helper
        ] + tail_helpers

        select_variables = [helper.get_select_variables() for helper in all_helpers]
        select_variables_compact = [
            helper.get_select_variables_compact() for helper in all_helpers
        ]
        where_clauses = [helper.get_where_clauses() for helper in all_helpers]
        where_clauses_compact = [
            helper.get_where_clauses_compact() for helper in all_helpers
        ]
        ask_items = [helper.get_ask_item() for helper in tail_helpers]

        sparql_query = f"""SELECT DISTINCT {" ".join([x for x in select_variables if x])}
WHERE {{{"".join(where_clauses).rstrip()}
}}"""
        sparql_query_compact = f"""SELECT {" ".join([x for x in select_variables_compact if x])}
WHERE {{{"".join(where_clauses_compact)}
}}"""
        verbalization_type = random.choice(VERBALIZATION_TYPE)
        return dict(
            verbalization=self.make_verbalization(ask_items, species_num=2, verbalization_type=verbalization_type),
            verbalization_type=verbalization_type,
            bindings=dict(
                species1=subgraph1["head"]["IdentifierValue"],
                species2=subgraph2["head"]["IdentifierValue"],
            ),
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
        )

    def resolve_tail_helper(self, tail: Dict[str, str]):
        if tail["type"] == "property":
            tail_helper = ExampleH2TQueryConstructorHelperTailProperty(
                property_name=tail["PropertyName"]
            )
        elif tail["type"] == "identifier":
            tail_helper = ExampleH2TQueryConstructorHelperTailIdentifier(
                identifier_name=tail["IdentifierName"]
            )
        elif tail["type"] == "use":
            tail_helper = ExampleH2TQueryConstructorHelperTailUse()
        elif tail["type"] == "chemicalclass":
            tail_helper = ExampleH2TQueryConstructorHelperTailChemicalClass()
        else:
            raise ValueError("Unexpected tail type: " + tail["type"])
        return tail_helper

    def make_verbalization(self, ask_items: List[str], species_num: int = 1, verbalization_type: str = "interrogative"):
        tokens = []
        for i, ask_item in enumerate(ask_items):
            if i == 0:
                pass
            elif i < len(ask_items) - 1:
                tokens.append(", ")
            else:
                tokens.append(" and ")
            tokens.append(ask_item)

        if species_num == 1:
            tokens.append(" of {species}")
        elif species_num == 2:
            tokens.append(" of {species1} and {species2}")

        kw_search = "".join(tokens)
        
        if verbalization_type == INTERROGATIVE:
            if len(ask_items) < 2:
                return f"What is the {kw_search}?"
            else:
                return f"What are the {kw_search}?"
        elif verbalization_type == IMPERATIVE:
            return f"Tell me about the {kw_search}."
        elif verbalization_type == KEYWORD:
            return kw_search
        else:
            raise UnexpectedVerbalizationType(verbalization_type)

    def are_subgraphs_congruent(
        self,
        subgraph1: Dict[str, Union[dict, List[dict]]],
        subgraph2: Dict[str, Union[dict, List[dict]]],
    ):
        return tails_to_tail_nums(subgraph1["tails"]) == tails_to_tail_nums(subgraph2["tails"])


class ExampleH2TQueryConstructorHelper(ABC):
    @abstractmethod
    def get_select_variables(self) -> str:
        pass

    @abstractmethod
    def get_select_variables_compact(self) -> str:
        pass

    @abstractmethod
    def get_where_clauses(self) -> str:
        pass

    @abstractmethod
    def get_where_clauses_compact(self) -> str:
        pass


class ExampleH2TQueryConstructorHelperHead(ExampleH2TQueryConstructorHelper):
    def __init__(self, identifier_values: str):
        self.identifier_values = identifier_values

    def get_select_variables(self):
        return "?label"

    def get_select_variables_compact(self):
        return ""

    def get_where_clauses(self):
        return f"""
    VALUES ( ?species ) {{ {" ".join([f'( "{val}" )' for val in self.identifier_values])} }}
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .
"""

    def get_where_clauses_compact(self):
        return f"""
    VALUES ( ?species ) {{ {" ".join([f'( "{val}" )' for val in self.identifier_values])} }}
    ?SpeciesIRI ?hasIdentifier ?species ."""


class AskItemGetter(ABC):
    @abstractmethod
    def get_ask_item(self) -> str:
        pass


class ExampleH2TQueryConstructorHelperTail(
    ExampleH2TQueryConstructorHelper, AskItemGetter
):
    pass


class ExampleH2TQueryConstructorHelperTailProperty(
    ExampleH2TQueryConstructorHelperTail
):
    def __init__(self, property_name: str):
        self.property_name = property_name

    def get_select_variables(self):
        PropertyName = self.property_name
        return f"?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue"

    def get_select_variables_compact(self):
        PropertyName = self.property_name
        return f"?{PropertyName}Value"

    def get_where_clauses(self):
        PropertyName = self.property_name
        return f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL {{
        ?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
        ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
        ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .
    }}
"""

    def get_where_clauses_compact(self):
        PropertyName = self.property_name
        return f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}Value ."""

    def get_ask_item(self):
        return random.choice(PROPERTY_LABELS[self.property_name])


class ExampleH2TQueryConstructorHelperTailIdentifier(
    ExampleH2TQueryConstructorHelperTail
):
    def __init__(self, identifier_name: str):
        self.identifier_name = identifier_name

    def get_select_variables(self):
        IdentifierName = self.identifier_name
        return f"?{IdentifierName}Value"

    def get_select_variables_compact(self):
        IdentifierName = self.identifier_name
        return f"?{IdentifierName}Value"

    def get_where_clauses(self):
        IdentifierName = self.identifier_name
        return f"""
    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}IRI .
    ?{IdentifierName}IRI os:value ?{IdentifierName}Value .
"""

    def get_where_clauses_compact(self):
        IdentifierName = self.identifier_name
        return f"""
    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}Value ."""

    def get_ask_item(self):
        return random.choice(IDENTIFIER_LABELS[self.identifier_name])


class ExampleH2TQueryConstructorHelperTailUse(ExampleH2TQueryConstructorHelperTail):
    def get_select_variables(self):
        return "?UseValue"

    def get_select_variables_compact(self):
        return "?UseValue"

    def get_where_clauses(self):
        return """
    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI rdfs:label ?UseValue .
"""

    def get_where_clauses_compact(self):
        return f"""
    ?SpeciesIRI os:hasUse ?UseValue ."""

    def get_ask_item(self):
        return "use"


class ExampleH2TQueryConstructorHelperTailChemicalClass(
    ExampleH2TQueryConstructorHelperTail
):
    def get_select_variables(self):
        return "?ChemicalClassValue"

    def get_select_variables_compact(self):
        return "?ChemicalClassValue"

    def get_where_clauses(self):
        return f"""
    ?SpeciesIRI os:hasChemicalClass* ?x .
    ?x ?y ?z .
    ?z rdfs:subClassOf* ?ChemicalClassIRI .
    ?ChemicalClassIRI rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue .
"""

    def get_where_clauses_compact(self):
        return f"""
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue ."""

    def get_ask_item(self):
        return "chemical class"
