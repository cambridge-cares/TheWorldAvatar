from abc import ABC, abstractmethod
from typing import Dict, Optional, Union, List
import random

from data_generation.utils import add_space_and_lower


class ExampleMakerHead2Tail:
    def make_example(self, subgraph: Optional[Dict[str, Union[dict, List[dict]]]]):
        if subgraph is None:
            return None

        head_helper = ExampleQueryConstructorHelperHead(
            identifier_value=subgraph["head"]["IdentifierValue"]
        )
        tails: List[dict] = list(subgraph["tails"])
        random.shuffle(tails)
        tail_helpers: List[ExampleQueryConstructorHelperTail] = [
            self.resolve_tail_helper(tail) for tail in tails
        ]
        all_helpers: List[ExampleQueryConstructorHelper] = [head_helper] + tail_helpers

        select_variables = [helper.get_select_variables() for helper in all_helpers]
        select_variables_compact = [
            helper.get_select_variables_compact() for helper in all_helpers
        ]
        where_clause_blocks = [
            helper.get_where_clause_blocks() for helper in all_helpers
        ]
        where_clause_compact_blocks = [
            helper.get_where_clause_compact_blocks() for helper in all_helpers
        ]
        ask_items = [helper.get_ask_item() for helper in tail_helpers]

        sparql_query = f"""SELECT DISTINCT {" ".join(select_variables)} 
WHERE {{{"".join(where_clause_blocks).rstrip()}
}}"""
        sparql_query_compact = f"""SELECT DISTINCT {" ".join(select_variables_compact)} 
WHERE {{{"".join(where_clause_compact_blocks)}
}}"""

        return dict(
            canonical_question=self.make_canonical_question(ask_items),
            bindings=dict(species=subgraph["head"]["IdentifierValue"]),
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
        )

    def resolve_tail_helper(self, tail: Dict[str, str]):
        if tail["type"] == "property":
            tail_helper = ExampleQueryConstructorHelperProperty(
                property_name=tail["PropertyName"]
            )
        elif tail["type"] == "identifier":
            tail_helper = ExampleQueryConstructorHelperIdentifier(
                identifier_name=tail["IdentifierName"]
            )
        elif tail["type"] == "use":
            tail_helper = ExampleQueryConstructorHelperUse()
        elif tail["type"] == "chemicalclass":
            tail_helper = ExampleQueryConstructorHelperChemicalClass()
        else:
            raise ValueError("Unexpected tail type: " + tail["type"])
        return tail_helper

    def make_canonical_question(self, ask_items: List[str]):
        tokens = ["What "]
        if len(ask_items) < 2:
            tokens.append("is")
        else:
            tokens.append("are")
        for i, ask_item in enumerate(ask_items):
            if i == 0:
                pass
            elif i < len(ask_items) - 1:
                tokens.append(",")
            else:
                tokens.append(" and")
            tokens.append(" the ")
            tokens.append(ask_item)
        tokens.append(" of {species}?")
        return "".join(tokens)


class ExampleQueryConstructorHelper(ABC):
    @abstractmethod
    def get_select_variables(self) -> str:
        pass

    @abstractmethod
    def get_select_variables_compact(self) -> str:
        pass

    @abstractmethod
    def get_where_clause_blocks(self) -> str:
        pass

    @abstractmethod
    def get_where_clause_compact_blocks(self) -> str:
        pass


class ExampleQueryConstructorHelperHead(ExampleQueryConstructorHelper):
    def __init__(self, identifier_value: str):
        self.identifier_value = identifier_value

    def get_select_variables(self):
        return "?label"

    def get_select_variables_compact(self):
        return ""

    def get_where_clause_blocks(self):
        species = self.identifier_value
        return f"""
    VALUES ( ?species ) {{ ( "{species}" ) }}
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .
"""

    def get_where_clause_compact_blocks(self):
        species = self.identifier_value
        return f"""
    VALUES ( ?species ) {{ ( "{species}" ) }}
    ?SpeciesIRI ?hasIdentifier ?species ."""


class AskItemGetter(ABC):
    @abstractmethod
    def get_ask_item(self) -> str:
        pass


class ExampleQueryConstructorHelperTail(ExampleQueryConstructorHelper, AskItemGetter):
    pass


class ExampleQueryConstructorHelperProperty(ExampleQueryConstructorHelperTail):
    def __init__(self, property_name: str):
        self.property_name = property_name

    def get_select_variables(self):
        PropertyName = self.property_name
        return f"?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue"

    def get_select_variables_compact(self):
        PropertyName = self.property_name
        return f"?{PropertyName}Value"

    def get_where_clause_blocks(self):
        PropertyName = self.property_name
        return f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{
        ?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
        ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
        ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .
    }}
"""

    def get_where_clause_compact_blocks(self):
        PropertyName = self.property_name
        return f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}Value ."""

    def get_ask_item(self):
        return add_space_and_lower(self.property_name)


class ExampleQueryConstructorHelperIdentifier(ExampleQueryConstructorHelperTail):
    IDENTIFIER_LABELS = {
        "ChebiID": ["ChEBI ID", "Chemical Entities of Biological Interest ID"],
        "CID": ["CID", "PubChem CID", "PubChem ID", "PubChem Compound ID"],
        "EmpiricalFormula": ["empirical formula"],
        "InChI": [
            "InChI",
            "International Chemical Identifier",
            "IUPAC International Chemical Identifier",
        ],
        "InChIKey": ["InChIKey", "hashed InChI"],
        "IUPACName": ["IUPAC name"],
        "MolecularFormula": ["molecular formula"],
        "SMILES": [
            "SMILES",
            "SMILES string",
        ],
    }

    def __init__(self, identifier_name: str):
        self.identifier_name = identifier_name

    def get_select_variables(self):
        IdentifierName = self.identifier_name
        return f"?{IdentifierName}Value"

    def get_select_variables_compact(self):
        IdentifierName = self.identifier_name
        return f"?{IdentifierName}Value"

    def get_where_clause_blocks(self):
        IdentifierName = self.identifier_name
        return f"""
    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}IRI .
    ?{IdentifierName}IRI os:value ?{IdentifierName}Value .
"""

    def get_where_clause_compact_blocks(self):
        IdentifierName = self.identifier_name
        return f"""
    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}Value ."""

    def get_ask_item(self):
        return random.choice(self.IDENTIFIER_LABELS[self.identifier_name])


class ExampleQueryConstructorHelperUse(ExampleQueryConstructorHelperTail):
    def get_select_variables(self):
        return "?UseValue"

    def get_select_variables_compact(self):
        return "?UseValue"

    def get_where_clause_blocks(self):
        return f"""
    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI rdfs:label ?UseValue .
"""

    def get_where_clause_compact_blocks(self):
        return f"""
    ?SpeciesIRI os:hasUse ?UseValue ."""

    def get_ask_item(self):
        return "use"


class ExampleQueryConstructorHelperChemicalClass(ExampleQueryConstructorHelperTail):
    def get_select_variables(self):
        return "?ChemicalClassValue"

    def get_select_variables_compact(self):
        return "?ChemicalClassValue"

    def get_where_clause_blocks(self):
        return f"""
    ?SpeciesIRI os:hasChemicalClass* ?x .
    ?x ?y ?z .
    ?z rdfs:subClassOf* ?ChemicalClassIRI .
    ?ChemicalClassIRI rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue .
"""

    def get_where_clause_compact_blocks(self):
        return f"""
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue ."""

    def get_ask_item(self):
        return "chemical class"
