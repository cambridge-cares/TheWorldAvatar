from typing import List
import torch
import numpy as np
import triton_python_backend_utils as pb_utils

from optimum.onnxruntime import ORTModelForSeq2SeqLM
from transformers import AutoTokenizer


T5_INPUT_PREFIX = "translate to SPARQL: "
T5_INPUT_ENCODINGS = {
    "<": "ls_th",
    ">": "gt_th",
}
T5_INPUT_DECODINGS = {v: k for k, v in T5_INPUT_ENCODINGS.items()}

T5_OUTPUT_ENCODINGS = {
    "{": "op_br",
    "}": "cl_br",
    "?": "var_",
    "<": "ls_th",
    ">": "gr_th",
}
T5_OUTPUT_DECODINGS = {v: k for k, v in T5_OUTPUT_ENCODINGS.items()}


def replace_multi(text: str, mapper: dict):
    for k, v in mapper.items():
        text = text.replace(k, v)
    return text


class TritonPythonModel:
    def initialize(self, args):
        self.tokenizer = AutoTokenizer.from_pretrained("/models/translation/1/model")
        self.model = ORTModelForSeq2SeqLM.from_pretrained("/models/translation/1/model")
        self.max_new_tokens = 256

    def preprocess(self, text):
        text = replace_multi(text, T5_INPUT_ENCODINGS)
        text = T5_INPUT_PREFIX + text
        return text
    
    def postprocess(self, text):
        text = replace_multi(text, T5_OUTPUT_DECODINGS)
        return text

    def translate(self, text):
        text = self.preprocess(text)

        input_ids = self.tokenizer(text, return_tensors="pt").input_ids
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )
        output_text = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)

        output_text = self.postprocess(output_text)
        return output_text

        
    def execute(self, requests):
        responses = []

        for request in requests:
            input_text = pb_utils.get_input_tensor_by_name(
                request, "text"
            ).as_numpy().tolist()[0].decode("UTF-8")
            output_text = self.translate(input_text)

            try:
                verbose_output = AbstractQueryRep.from_string(output_text).compact2verbose().to_query_string()
            except:
                verbose_output = ""
            
            inference_response = pb_utils.InferenceResponse(
                output_tensors=[
                    pb_utils.Tensor("output_compact", np.array([output_text], dtype=object)),
                    pb_utils.Tensor("output_verbose", np.array([verbose_output], dtype=object))
                ]
            )
            responses.append(inference_response)

        return responses


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
    "?UseIRI os:value ?UseValue .",
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
                        "Open bracket is missing from FITLER clause: "
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


def advance_ptr_to_kw(text: str, kw: str, ptr: int=0):
    while ptr < len(text) and not text.startswith(kw, ptr):
        ptr += 1
    return ptr


def advance_ptr_thru_space(text: str, ptr: int=0):
    while ptr < len(text) and text[ptr].isspace():
        ptr += 1
    return ptr


def advance_ptr_to_space(text: str, ptr: int=0):
    while ptr < len(text) and not text[ptr].isspace():
        ptr += 1
    return ptr


def replace_list_element(lst: list, old, new):
    found = False
    for i, val in enumerate(lst):
        if val == old:
            lst[i] = new
            found = True
        if found:
            break