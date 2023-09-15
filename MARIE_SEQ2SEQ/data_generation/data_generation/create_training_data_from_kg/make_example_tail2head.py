import random
from typing import Dict, List, Literal, Optional, Union

from data_generation.utils import add_space_and_lower


class ExampleMakerTail2Head:
    def make_example(self, subgraph: Optional[Dict[str, Union[dict, List[dict]]]]):
        if subgraph is None:
            return None
        
        select_variables = ["?label ?IUPACNameValue"]
        select_variables_compact = ["?IUPACNameValue"]

        where_clause_blocks = [self.make_where_species()]
        where_clause_compact_blocks = [self.make_where_species_compact()]
        ask_items: List[str] = []
        bindings : Dict[str, str] = dict()

        tails: List[dict] = list(subgraph["tails"])
        random.shuffle(tails)
        property_num = 0
        use_num = 0
        chemicalclass_num = 0
        for tail in tails:
            if tail["type"] == "property":
                property_num += 1
                PropertyName = tail["PropertyName"]
                PropertyValue = tail["PropertyValue"]
                numerical_clause = random.choice(["higher", "lower", "inside", "outside", "around"])

                select_variables.append(
                    f"?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue"
                )
                select_variables_compact.append(f"?{PropertyName}Value")
                where_clause_blocks.append(self.make_where_property(PropertyName))
                where_clause_compact_blocks.append(
                    self.make_where_property_compact(PropertyName)
                )
                filter_clause, ask_item = self.make_where_property_filter_and_ask_item(PropertyName, PropertyValue, numerical_clause)
                where_clause_blocks.append(filter_clause)
                where_clause_compact_blocks.append(filter_clause)
                ask_items.append(ask_item)
            elif tail["type"] == "use":
                use_num += 1
                UseValue = tail["UseValue"]

                where_clause_blocks.append(self.make_where_use(use_num, UseValue))
                where_clause_compact_blocks.append(self.make_where_use_compact(UseValue))
                ask_items.append(f"use is as {{UseValue{use_num}}}")
                bindings[f"UseValue{use_num}"] = UseValue
            elif tail["type"] == "chemicalclass":
                chemicalclass_num += 1
                ChemicalClassValue = tail["ChemicalClassValue"]

                where_clause_blocks.append(self.make_where_chemicalclass(chemicalclass_num, ChemicalClassValue))
                where_clause_compact_blocks.append(
                    self.make_where_chemicalclass_compact(ChemicalClassValue)
                )
                ask_items.append(f"chemical class is {{ChemicalClassValue{chemicalclass_num}}}")
                bindings[f"ChemicalClassValue{chemicalclass_num}"] = ChemicalClassValue
            else:
                raise ValueError("Unexpected tail type: " + tail["type"])
        
        sparql_query = f"""SELECT DISTINCT {" ".join(select_variables)}
WHERE {{{"".join(where_clause_blocks)}
}}"""
        sparql_query_compact = f"""SELECT DISTINCT {" ".join(select_variables_compact)}
WHERE {{{"".join(where_clause_compact_blocks)}
}}"""

        return dict(
            canonical_question=self.make_canonical_question(ask_items),
            bindings=bindings,
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
        )

    def make_where_species(self):
        return f"""
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue ."""

    def make_where_species_compact(self):
        return f"""
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue ."""

    def get_value_lower(self, value_str: str, eps=1e-6):
        value = float(value_str) * 0.9
        if abs(value) < eps:
            value = (value > 0) * 0.5
        if random.getrandbits(1):
            value = round(value)
        return value
    
    def get_value_higher(self, value_str: str, eps=1e-6):
        value = float(value_str) * 1.1
        if abs(value) < eps:
            value = (value > 0) * 0.5
        if random.getrandbits(1):
            value = round(value)
        return value

    def make_where_property(self, PropertyName: str):
        return f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL {{
        ?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
        ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
        ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .
    }}"""
    
    def make_where_property_compact(self, PropertyName: str):
        return f"""
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value ."""

    def make_where_property_filter_and_ask_item(self, PropertyName: str, PropertyValue: str, numerical_clause: str):
        property_verbalised = add_space_and_lower(PropertyName)
        if numerical_clause == "higher":
            value = self.get_value_lower(PropertyValue)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value > {value} )"""
            ask_item = f"{property_verbalised} is higher than {value}"
        elif numerical_clause == "lower":
            value = self.get_value_higher(PropertyValue)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value < {value} )"""
            ask_item = f"{property_verbalised} is lower than {value}"
        elif numerical_clause == "outside":
            if random.getrandbits(1):
                high = self.get_value_lower(PropertyValue)
                low = self.get_value_lower(high)
            else:
                low = self.get_value_higher(PropertyValue)
                high = self.get_value_higher(low)
            filter_clause = f"""
    FILTER ( ?{PropertyName}Value < {low} && ?{PropertyName}Value > {high} )"""
            ask_item = f"a {property_verbalised} "
        elif numerical_clause == "inside":
            low = self.get_value_lower(PropertyValue)
            high = self.get_value_higher(PropertyValue)
            filter_clause = f"""
        FILTER ( ?{PropertyName}Value > {low} && ?{PropertyName}Value < {high} )"""
            ask_item = f"{property_verbalised} is between {low} and {high}"
        elif numerical_clause == "around":
            value = float(PropertyValue)
            if random.getrandbits(1):
                value = round(value)
            filter_clause = f"""
        FILTER ( ?{PropertyName}Value > {value}*0.9 && ?{PropertyName}Value < {value}*1.1 )"""
            ask_item = f"{property_verbalised} is around {value}"
        else:
            raise ValueError(f"Unexpected argument for `numerical_clause`: {numerical_clause}.")

        return filter_clause, ask_item
    
    def make_where_use(self, i: int, UseValue: str):
        return f"""
    ?SpeciesIRI os:hasUse ?UseIRI{i} .
    ?UseIRI{i} rdfs:label \"{UseValue}\" ."""
    
    def make_where_use_compact(self, UseValue: str):
        return f"""
    ?SpeciesIRI os:hasUse \"{UseValue}\" ."""

    def make_where_chemicalclass(self, i: int, ChemicalClassValue: str):
        return f"""
    ?SpeciesIRI os:hasChemicalClass* ?x{i} .
	?x{i} ?y{i} ?z{i} .
	?z{i} rdfs:subClassOf* ?ChemicalClassIRI{i} .
	?ChemicalClassIRI{i} rdf:type os:ChemicalClass ; rdfs:label \"{ChemicalClassValue}\" ."""

    def make_where_chemicalclass_compact(self, ChemicalClassValue: str):
        return f"""
    ?SpeciesIRI os:hasChemicalClass \"{ChemicalClassValue}\" ."""

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