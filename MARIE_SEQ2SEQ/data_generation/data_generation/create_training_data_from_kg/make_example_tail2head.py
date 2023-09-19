from abc import ABC, abstractmethod
import pprint
import random
from typing import Dict, List, Optional, Union

from data_generation.constants import PROPERTY_LABELS
from .utils_numerical import FloatConversionError, get_value_around, get_value_higher, get_value_lower


class ExampleMakerTail2Head:
    def make_example(self, subgraph: Optional[Dict[str, Union[dict, List[dict]]]]):
        if subgraph is None:
            return None

        head_helper = ExampleT2HQueryConstructorHelperHead()

        tails: List[dict] = [tail for tail in subgraph["tails"] if tail["type"] != "identifier"]
        if len(tails) == 0:
            return None
        
        random.shuffle(tails)
        tail_helper_resolver = TailHelperResolver()
        tail_helpers: List[ExampleT2HQueryConstructorHelperTail] = [
            tail_helper_resolver.resolve(tail) for tail in tails 
        ]

        all_helpers: List[ExampleT2HQueryConstructorHelper] = [
            head_helper
        ] + tail_helpers
        
        select_variables = [helper.get_select_variables() for helper in all_helpers]
        select_variables_compact = [
            helper.get_select_variables_compact() for helper in all_helpers
        ]
        try:
            where_clauses = [helper.get_where_clauses() for helper in all_helpers]
            where_clauses_compact = [
                helper.get_where_clauses_compact() for helper in all_helpers
            ]
        except FloatConversionError as e:
            print("A float conversion error is encountered while making an example for the following subgraph.")
            pprint.pprint(subgraph)
            raise e


        ask_items = [helper.get_ask_item() for helper in tail_helpers]
        bindings = {
            k: v for helper in tail_helpers for k, v in helper.get_binding().items()
        }

        sparql_query = f"""SELECT DISTINCT {" ".join(select_variables)}
WHERE {{{"".join(where_clauses).rstrip()}
}}"""
        sparql_query_compact = f"""SELECT DISTINCT {" ".join(select_variables_compact)}
WHERE {{{"".join(where_clauses_compact)}
}}"""

        return dict(
            canonical_question=self.make_canonical_question(ask_items),
            bindings=bindings,
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
        )

    def make_canonical_question(self, ask_items: List[str]):
        tokens = ["What are the chemical species whose "]
        for i, ask_item in enumerate(ask_items):
            tokens.append(ask_item)
            if i < len(ask_items) - 2:
                tokens.append(", ")
            elif i == len(ask_items) - 2:
                tokens.append(" and ")
        tokens.append("?")
        return "".join(tokens)


class TailHelperResolver:
    def __init__(self):
        self.use_num = 0
        self.chemicalclass_num = 0

    def resolve(self, tail: Dict[str, str]):
        if tail["type"] == "property":
            tail_helper = ExampleT2HQueryConstructorHelperTailProperty(
                property_name=tail["PropertyName"], property_value=tail["PropertyValue"]
            )
        elif tail["type"] == "use":
            self.use_num += 1
            tail_helper = ExampleT2HQueryConstructorHelperTailUse(
                tail_id=self.use_num, use_value=tail["UseValue"]
            )
        elif tail["type"] == "chemicalclass":
            self.chemicalclass_num += 1
            tail_helper = ExampleT2HQueryConstructorHelperTailChemicalClass(
                tail_id=self.chemicalclass_num,
                chemicalclass_value=tail["ChemicalClassValue"],
            )
        else:
            raise ValueError("Unexpected tail type: " + tail["type"])
        return tail_helper


class ExampleT2HQueryConstructorHelper(ABC):
    def get_select_variables(self):
        return ""

    def get_select_variables_compact(self):
        return ""

    @abstractmethod
    def get_where_clauses(self) -> str:
        pass

    @abstractmethod
    def get_where_clauses_compact(self) -> str:
        pass


class ExampleT2HQueryConstructorHelperHead(ExampleT2HQueryConstructorHelper):
    def get_select_variables(self):
        return "?label ?IUPACNameValue"

    def get_select_variables_compact(self):
        return "?IUPACNameValue"

    def get_where_clauses(self):
        return f"""
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .
"""

    def get_where_clauses_compact(self):
        return f"""
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue ."""


class AskItemAndBindingGetter(ABC):
    @abstractmethod
    def get_ask_item(self) -> str:
        pass

    def get_binding(self):
        return dict()


class ExampleT2HQueryConstructorHelperTail(
    ExampleT2HQueryConstructorHelper, AskItemAndBindingGetter
):
    pass


class ExampleT2HQueryConstructorHelperTailProperty(
    ExampleT2HQueryConstructorHelperTail
):
    def __init__(self, property_name: str, property_value: str):
        self.property_name = property_name
        self.property_value = property_value
        self.numerical_clause = random.choice(
            ["higher", "lower", "inside", "outside", "around"]
        )

        self.filter_clause = None
        self.ask_item = None

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
    }}{self.get_filter_clause()}
"""

    def get_where_clauses_compact(self):
        PropertyName = self.property_name
        return f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .{self.get_filter_clause()}"""

    def get_ask_item(self):
        if self.ask_item is None:
            self.populate_property_filter_and_ask_item()
        return self.ask_item

    def get_filter_clause(self):
        if self.filter_clause is None:
            self.populate_property_filter_and_ask_item()
        return self.filter_clause

    def populate_property_filter_and_ask_item(self):
        PropertyName = self.property_name
        PropertyValue = self.property_value
        numerical_clause = self.numerical_clause
        property_verbalised = random.choice(PROPERTY_LABELS[PropertyName])

        if numerical_clause == "higher":
            value = get_value_lower(PropertyValue)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value > {value} )"""
            ask_item = f"{property_verbalised} is higher than {value}"
        elif numerical_clause == "lower":
            value = get_value_higher(PropertyValue)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value < {value} )"""
            ask_item = f"{property_verbalised} is lower than {value}"
        elif numerical_clause == "outside":
            if random.getrandbits(1):
                high = get_value_lower(PropertyValue)
                low = get_value_lower(high)
            else:
                low = get_value_higher(PropertyValue)
                high = get_value_higher(low)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value < {low} || ?{PropertyName}Value > {high} )"""
            ask_item = f"{property_verbalised} is outside the range between {low} and {high}"
        elif numerical_clause == "inside":
            low = get_value_lower(PropertyValue)
            high = get_value_higher(PropertyValue)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value > {low} && ?{PropertyName}Value < {high} )"""
            ask_item = f"{property_verbalised} is between {low} and {high}"
        elif numerical_clause == "around":
            value = get_value_around(PropertyValue)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value > {value}*0.9 && ?{PropertyName}Value < {value}*1.1 )"""
            ask_item = f"{property_verbalised} is around {value}"
        else:
            raise ValueError(
                f"Unexpected argument for `numerical_clause`: {numerical_clause}."
            )

        self.filter_clause = filter_clause
        self.ask_item = ask_item


class ExampleT2HQueryConstructorHelperTailUse(ExampleT2HQueryConstructorHelperTail):
    def __init__(self, tail_id: int, use_value: str):
        self.tail_id = tail_id
        self.use_value = use_value

    def get_where_clauses(self):
        i = self.tail_id
        UseValue = self.use_value
        return f"""
    ?SpeciesIRI os:hasUse ?UseIRI{i} .
    ?UseIRI{i} rdfs:label \"{UseValue}\" .
"""

    def get_where_clauses_compact(self):
        UseValue = self.use_value
        return f"""
    ?SpeciesIRI os:hasUse \"{UseValue}\" ."""

    def get_ask_item(self):
        i = self.tail_id
        return f"use is as {{UseValue{i}}}"

    def get_binding(self):
        i = self.tail_id
        UseValue = self.use_value
        return {f"UseValue{i}": UseValue}


class ExampleT2HQueryConstructorHelperTailChemicalClass(
    ExampleT2HQueryConstructorHelperTail
):
    def __init__(self, tail_id: int, chemicalclass_value: str):
        self.tail_id = tail_id
        self.chemicalclass_value = chemicalclass_value

    def get_where_clauses(self):
        i = self.tail_id
        ChemicalClassValue = self.chemicalclass_value
        return f"""
    ?SpeciesIRI os:hasChemicalClass* ?x{i} .
	?x{i} ?y{i} ?z{i} .
	?z{i} rdfs:subClassOf* ?ChemicalClassIRI{i} .
	?ChemicalClassIRI{i} rdf:type os:ChemicalClass ; rdfs:label \"{ChemicalClassValue}\" .
"""

    def get_where_clauses_compact(self):
        ChemicalClassValue = self.chemicalclass_value
        return f"""
    ?SpeciesIRI os:hasChemicalClass \"{ChemicalClassValue}\" ."""

    def get_ask_item(self):
        i = self.tail_id
        return f"chemical class is {{ChemicalClassValue{i}}}"

    def get_binding(self):
        i = self.tail_id
        ChemicalClassValue = self.chemicalclass_value
        return {f"ChemicalClassValue{i}": ChemicalClassValue}
