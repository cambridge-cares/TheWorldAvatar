from typing import List

from core.utils import advance_ptr_thru_space, advance_ptr_to_kw, advance_ptr_to_space
from core.data_processing.utils import replace_list_element


RESULT_CLAUSE_KWS = ["SELECT"]

SPECIES_FROM_IDENTIFIER_PATTERN_COMPACT_PREFIX = "?SpeciesIRI ?hasIdentifier ?species"
PROPERTY_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasProperty"
IDENTIFIER_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasIdentifier"
CHEMCLASS_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue"
USE_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasUse ?UseValue"
ALL_PROPERTIES_PATTERN_COMPACT_PREFIX = (
    "?SpeciesIRI ?hasPropertyName ?PropertyNameValue"
)
ALL_IDENTIFIERS_PATTERN_COMPACT_PREFIX = (
    "?SpeciesIRI ?hasIdentifierName ?IdentifierNameValue"
)

SPECIES_FROM_QUALIFIERS_PATTERN_COMPACT_PREFIX = (
    "?SpeciesIRI os:hasIUPACName ?IUPACNameValue"
)

SPECIES_FROM_IDENTIFIER_PATTERNS_VERBOSE = [
    "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
    "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
    "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
    "?Identifier rdfs:subClassOf os:Identifier .",
]
PROPERTY_PATTERNS_VERBOSE = lambda property_name: [
    f"?SpeciesIRI os:has{property_name} ?{property_name}IRI .",
    f"?{property_name}IRI os:value ?{property_name}Value ; os:unit ?{property_name}UnitIRI ; os:hasProvenance ?{property_name}ProvenanceIRI .",
    f"?{property_name}UnitIRI rdfs:label ?{property_name}UnitValue .",
    (
        f"OPTIONAL{{?{property_name}IRI os:hasReferenceState ?{property_name}ReferenceStateIRI .\n"
        f"?{property_name}ReferenceStateIRI os:value ?{property_name}ReferenceStateValue ; os:unit ?{property_name}ReferenceStateUnitIRI .\n"
        f"?{property_name}ReferenceStateUnitIRI rdfs:label ?{property_name}ReferenceStateUnitValue .}}"
    ),
]
IDENTIFIER_PATTERNS_VERBOSE = lambda identifier_name: [
    f"?SpeciesIRI os:has{identifier_name} ?{identifier_name}IRI .",
    f"?{identifier_name}IRI os:value ?{identifier_name}Value .",
]
CHEMCLASS_PATTERNS_VERBOSE = [
    "?SpeciesIRI os:hasChemicalClass* ?x .",
    "?x ?y ?z .",
    "?z rdfs:subClassOf* ?ChemicalClassIRI .",
    "?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .",
]
USE_PATTERNS_VERBOSE = [
    "?SpeciesIRI os:hasUse ?UseIRI .",
    "?UseIRI rdfs:label ?UseValue .",
]
ALL_PROPERTIES_PATTERNS_VERBOSE = [
    "?SpeciesIRI ?hasPropertyName ?PropertyNameIRI .",
    "?PropertyNameIRI  rdf:type ?PropertyName .",
    "?PropertyName rdfs:subClassOf os:Property .",
    "?PropertyNameIRI os:value ?PropertyNameValue ; os:unit ?PropertyNameUnitIRI ; os:hasProvenance ?PropertyNameProvenanceIRI .",
    "?PropertyNameUnitIRI rdfs:label ?PropertyNameUnitValue .",
    (
        "OPTIONAL{?PropertyNameIRI os:hasReferenceState ?PropertyNameReferenceStateIRI .\n"
        "?PropertyNameReferenceStateIRI os:value ?PropertyNameReferenceStateValue ; os:unit ?PropertyNameReferenceStateUnitIRI .\n"
        "?PropertyNameReferenceStateUnitIRI rdfs:label ?PropertyNameReferenceStateUnitValue .}\n"
    ),
    "BIND(strafter(str(?PropertyName),'#') AS ?PropertyLabel)",
]
ALL_IDENTIFIERS_PATTERNS_VERBOSE = [
    "?SpeciesIRI ?hasIdentifierName ?IdentifierNameIRI .",
    "?IdentifierNameIRI  rdf:type ?IdentifierName .",
    "?IdentifierName rdfs:subClassOf os:Identifier .",
    "?IdentifierNameIRI os:value ?IdentifierNameValue .",
    "BIND(strafter(str(?IdentifierName),'#') AS ?IdentifierLabel)",
]

SPECIES_FROM_QUALIFIERS_PATTERNS_VERBOSE = [
    "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
    "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
    "?IUPACNameIRI os:value ?IUPACNameValue .",
]


class AbstractQueryRep:
    """To be deprecated. Please use CompactQueryRep instead."""
    
    def __init__(
        self,
        result_clause: str,
        where_clause: List[str],
    ):
        self.result_clause = result_clause
        self.where_clause = where_clause

    def __eq__(self, other):
        return (
            isinstance(other, AbstractQueryRep)
            and self.result_clause == other.result_clause
            and self.where_clause == other.where_clause
        )

    def __repr__(self):
        return repr(vars(self))

    def to_query_string(self):
        return "{result_clause}\nWHERE {{\n{where_clause}\n}}".format(
            result_clause=self.result_clause, where_clause="\n".join(self.where_clause)
        )

    def compact2verbose(self):
        if not self.result_clause.startswith("SELECT DISTINCT"):
            raise Exception("Unexpected result clause: " + self.result_clause)

        result_clause_tokens = ["SELECT DISTINCT ?label"] + self.result_clause[
            len("SELECT DISTINCT") :
        ].strip().split()

        where_clause = []
        for pattern in self.where_clause:
            if pattern.startswith("FILTER"):
                where_clause.append(pattern)

            # assume it's triple pattern, species -> properties/identifiers/chemclass
            elif pattern.startswith(SPECIES_FROM_IDENTIFIER_PATTERN_COMPACT_PREFIX):
                where_clause.extend(SPECIES_FROM_IDENTIFIER_PATTERNS_VERBOSE)
            elif pattern.startswith(PROPERTY_PATTERN_COMPACT_PREFIX):
                property_name = pattern[
                    len(PROPERTY_PATTERN_COMPACT_PREFIX) : advance_ptr_to_space(
                        pattern, len(PROPERTY_PATTERN_COMPACT_PREFIX)
                    )
                ].strip()

                replace_list_element(
                    result_clause_tokens,
                    f"?{property_name}Value",
                    f"?{property_name}Value ?{property_name}UnitValue ?{property_name}ReferenceStateValue ?{property_name}ReferenceStateUnitValue",
                )

                where_clause.extend(PROPERTY_PATTERNS_VERBOSE(property_name))
            elif pattern.startswith(IDENTIFIER_PATTERN_COMPACT_PREFIX):
                identifier_name = pattern[
                    len(IDENTIFIER_PATTERN_COMPACT_PREFIX) : advance_ptr_to_space(
                        pattern, len(PROPERTY_PATTERN_COMPACT_PREFIX)
                    )
                ].strip()
                where_clause.extend(IDENTIFIER_PATTERNS_VERBOSE(identifier_name))
            elif pattern.startswith(CHEMCLASS_PATTERN_COMPACT_PREFIX):
                where_clause.extend(CHEMCLASS_PATTERNS_VERBOSE)
            elif pattern.startswith(USE_PATTERN_COMPACT_PREFIX):
                where_clause.extend(USE_PATTERNS_VERBOSE)
            elif pattern.startswith(ALL_PROPERTIES_PATTERN_COMPACT_PREFIX):
                replace_list_element(
                    result_clause_tokens,
                    old="?PropertyNameValue",
                    new="?PropertyLabel ?PropertyNameValue ?PropertyNameUnitValue ?PropertyNameReferenceStateValue ?PropertyNameReferenceStateUnitValue",
                )

                where_clause.extend(ALL_PROPERTIES_PATTERNS_VERBOSE)
            elif pattern.startswith(ALL_IDENTIFIERS_PATTERN_COMPACT_PREFIX):
                replace_list_element(
                    result_clause_tokens,
                    old="?IdentifierNameValue",
                    new="?IdentifierLabel ?IdentifierNameValue",
                )

                where_clause.extend(ALL_IDENTIFIERS_PATTERNS_VERBOSE)

            # qualifiers -> species
            elif pattern.startswith(SPECIES_FROM_QUALIFIERS_PATTERN_COMPACT_PREFIX):
                where_clause.extend(SPECIES_FROM_QUALIFIERS_PATTERNS_VERBOSE)
            else:
                raise Exception("Unexpected compact clause: " + pattern)

        return AbstractQueryRep(" ".join(result_clause_tokens), where_clause)

    @classmethod
    def _does_startwith_result_clause_kw(cls, query: str, ptr: int):
        return any(query.startswith(kw, ptr) for kw in RESULT_CLAUSE_KWS)

    @classmethod
    def from_string(cls, query: str):
        ptr = 0
        while not cls._does_startwith_result_clause_kw(query, ptr) and ptr < len(query):
            ptr += 1
        if ptr >= len(query):
            raise ValueError("Result clause is missing from the query: ", query)
        result_clause_idx = ptr

        ptr = advance_ptr_to_kw(query, "WHERE", ptr)
        if ptr >= len(query):
            raise ValueError("WHERE clause is missing from the query: ", query)
        where_clause_idx = ptr

        result_clause = query[result_clause_idx:where_clause_idx].strip()

        ptr += len("WHERE")
        ptr = advance_ptr_thru_space(query, ptr)
        if query[ptr] != "{":
            raise ValueError("Missing open bracket after WHERE keyword: ", query)

        ptr += 1
        # assume that WHERE clause contains only basic triple patterns and FILTER clauses
        where_clause = []
        while True:
            ptr = advance_ptr_thru_space(query, ptr)

            if query[ptr] == "}":
                break
            if ptr >= len(query):
                raise ValueError(
                    "Close curly bracket is missing from the WHERE clause: " + query
                )

            start_idx = ptr
            if query.startswith("FILTER", ptr):
                ptr += len("FILTER")
                ptr = advance_ptr_thru_space(query, ptr)
                if query[ptr] != "(":
                    raise ValueError(
                        "Open bracket is missing from FILTER clause: "
                        + query[start_idx:]
                    )

                open_brac_num = 1
                while open_brac_num > 0:
                    ptr += 1
                    if query[ptr] == "(":
                        open_brac_num += 1
                    elif query[ptr] == ")":
                        open_brac_num -= 1

                ptr += 1
            else:  # assume it's the triple pattern
                ptr = advance_ptr_to_kw(query, ".", ptr)
                if ptr >= len(query):
                    raise ValueError(
                        "Full-stop is missing from triple pattern: " + query[start_idx:]
                    )
                ptr += 1

            end_idx = ptr
            where_clause.append(query[start_idx:end_idx].strip())

        return cls(result_clause, where_clause)
