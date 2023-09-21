from typing import List

from core.utils import advance_ptr_thru_space, advance_ptr_to_kw, advance_ptr_to_space


SPECIES_FROMHEAD_PATTERN_COMPACT_PREFIX = "?SpeciesIRI ?hasIdentifier ?species"
PROPERTY_AND_IDENTIFIER_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:has"
CHEMCLASS_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue"
USE_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasUse ?UseValue"
ALL_PROPERTIES_PATTERN_COMPACT_PREFIX = (
    "?SpeciesIRI ?hasPropertyName ?PropertyNameValue"
)
ALL_IDENTIFIERS_PATTERN_COMPACT_PREFIX = (
    "?SpeciesIRI ?hasIdentifierName ?IdentifierNameValue"
)

SPECIES_FROMTAIL_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasIUPACName ?IUPACNameValue"

SPECIES_FROMHEAD_PATTERNS_VERBOSE = """
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .
"""
PROPERTY_PATTERNS_VERBOSE = (
    lambda PropertyName: f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL {{
        ?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
        ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
        ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .
    }}
"""
)
IDENTIFIER_PATTERNS_VERBOSE = (
    lambda IdentifierName: f"""
    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}IRI .
    ?{IdentifierName}IRI os:value ?{IdentifierName}Value .
"""
)

CHEMCLASS_PATTERNS_VERBOSE = (
    lambda i: f"""
    ?SpeciesIRI os:hasChemicalClass* ?x{i} .
    ?x{i} ?y{i} ?z{i} .
    ?z{i} rdfs:subClassOf* ?ChemicalClassIRI{i} .
    ?ChemicalClassIRI{i} rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue{i} .
"""
)
USE_PATTERNS_VERBOSE = (
    lambda i: f"""
    ?SpeciesIRI os:hasUse ?UseIRI{i} .
    ?UseIRI{i} rdfs:label ?UseValue{i} .
"""
)
ALL_PROPERTIES_PATTERNS_VERBOSE = """
    ?SpeciesIRI ?hasPropertyName ?PropertyNameIRI .
    ?PropertyNameIRI rdf:type ?PropertyName .
    ?PropertyName rdfs:subClassOf os:Property .
    ?PropertyNameIRI os:value ?PropertyNameValue ; os:unit ?PropertyNameUnitIRI ; os:hasProvenance ?PropertyNameProvenanceIRI .
    ?PropertyNameUnitIRI rdfs:label ?PropertyNameUnitValue .
    OPTIONAL {
        ?PropertyNameIRI os:hasReferenceState ?PropertyNameReferenceStateIRI .
        ?PropertyNameReferenceStateIRI os:value ?PropertyNameReferenceStateValue ; os:unit ?PropertyNameReferenceStateUnitIRI .
        ?PropertyNameReferenceStateUnitIRI rdfs:label ?PropertyNameReferenceStateUnitValue .
    }

    BIND(strafter(str(?PropertyName),'#') AS ?PropertyLabel)
"""
ALL_IDENTIFIERS_PATTERNS_VERBOSE = """
    ?SpeciesIRI ?hasIdentifierName ?IdentifierNameIRI .
    ?IdentifierNameIRI  rdf:type ?IdentifierName .
    ?IdentifierName rdfs:subClassOf os:Identifier .
    ?IdentifierNameIRI os:value ?IdentifierNameValue .

    BIND(strafter(str(?IdentifierName),'#') AS ?IdentifierLabel)
"""

SPECIES_FROMTAIL_PATTERNS_VERBOSE = """
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .
"""

PROPERTY_NAMES = set(
    [
        "AtomChiralCount",
        "AtomChiralDefCount",
        "AtomChiralUndefCount",
        "BondChiralCount",
        "BondChiralDefCount",
        "BondChiralUndefCount",
        "CanonicalizedCompound",
        "Charge",
        "CompoundComplexity",
        "CovalentUnitCount",
        "ExactMass",
        "HeavyAtomCount",
        "HydrogenBondAcceptorCount",
        "HydrogenBondDonorCount",
        "IsotopeAtomCount",
        "MolecularWeight",
        "MonoIsotopicWeight",
        "RotatableBondCount",
        "SubStructureKeysFingerprint",
        "TautomerCount",
        "XLogP3",
        "AutoignitionTemperature",
        "Caco2Permeability",
        "CollisionCrossSection",
        "Hydrophobicity",
        "IonizationPotential",
        "IsoelectricPoint",
        "LogP",
        "LogS",
        "PolarSurfaceArea",
        "BoilingPoint",
        "Density",
        "DissociationConstants",
        "EnthalpyOfSublimation",
        "FlashPoint",
        "StandardEnthalpyOfFormation",
        "HeatOfCombustion",
        "HeatOfVaporization",
        "HenrysLawConstant",
        "MeltingPoint",
        "OpticalRotation",
        "Solubility",
        "SurfaceTension",
        "VaporDensity",
        "VaporPressure",
        "Viscosity",
    ]
)

IDENTIFIER_NAMES = set(
    [
        "ChebiID",
        "CID",
        "EmpiricalFormula",
        "InChI",
        "InChIKey",
        "IUPACName",
        "MolecularFormula",
        "SMILES",
    ]
)


class CompactQueryRep:
    def __init__(self, select_variables: str, where_clauses: List[str]):
        self.select_variables = select_variables
        self.where_clauses = where_clauses

    def __eq__(self, other):
        return (
            isinstance(other, CompactQueryRep)
            and self.select_variables == other.select_variables
            and self.where_clauses == other.where_clauses
        )

    def __repr__(self):
        return repr(vars(self))

    def to_verbose(self):
        select_variables = str(self.select_variables)
        where_clauses_verbose = []

        # assumes that the triple patterns for head always come first, which might include VALUES bindings
        if self.where_clauses[0].startswith("VALUES"):
            where_clauses_verbose.append("\n    " + self.where_clauses[0])
            head_pattern = self.where_clauses[1]
            tail_patterns = self.where_clauses[2:]
        else:
            head_pattern = self.where_clauses[0]
            tail_patterns = self.where_clauses[1:]

        if head_pattern.startswith(SPECIES_FROMHEAD_PATTERN_COMPACT_PREFIX):
            where_clauses_verbose.append(SPECIES_FROMHEAD_PATTERNS_VERBOSE)
        elif head_pattern.startswith(SPECIES_FROMTAIL_PATTERN_COMPACT_PREFIX):
            where_clauses_verbose.append(SPECIES_FROMTAIL_PATTERNS_VERBOSE)
        else:
            raise Exception("Unexpected pattern for head node: " + head_pattern)

        for pattern in tail_patterns:
            if pattern.startswith("FILTER"):
                where_clauses_verbose.append(f"    {pattern}\n")
            elif pattern.startswith("VALUES"):
                where_clauses_verbose.append("\n    " + pattern)
            elif pattern.startswith(PROPERTY_AND_IDENTIFIER_PATTERN_COMPACT_PREFIX):
                name = pattern[
                    len(
                        PROPERTY_AND_IDENTIFIER_PATTERN_COMPACT_PREFIX
                    ) : advance_ptr_to_space(
                        pattern, len(PROPERTY_AND_IDENTIFIER_PATTERN_COMPACT_PREFIX)
                    )
                ].strip()

                if name in PROPERTY_NAMES:
                    select_variables = select_variables.replace(
                        f"?{name}Value",
                        f"?{name}Value ?{name}UnitValue ?{name}ReferenceStateValue ?{name}ReferenceStateUnitValue",
                        1,
                    )
                    where_clauses_verbose.append(PROPERTY_PATTERNS_VERBOSE(name))
                elif name in IDENTIFIER_NAMES:
                    where_clauses_verbose.append(IDENTIFIER_PATTERNS_VERBOSE(name))
                elif name == "Use":
                    i = pattern[
                        len(USE_PATTERN_COMPACT_PREFIX) : advance_ptr_to_space(
                            pattern, len(USE_PATTERN_COMPACT_PREFIX)
                        )
                    ].strip()
                    where_clauses_verbose.append(USE_PATTERNS_VERBOSE(i))
                elif name == "ChemicalClass":
                    i = pattern[
                        len(CHEMCLASS_PATTERN_COMPACT_PREFIX) : advance_ptr_to_space(
                            pattern, len(CHEMCLASS_PATTERN_COMPACT_PREFIX)
                        )
                    ].strip()
                    where_clauses_verbose.append(CHEMCLASS_PATTERNS_VERBOSE(i))
                else:
                    raise ValueError(f"Unrecognized relation `os:has{name}`.")

            elif pattern.startswith(ALL_PROPERTIES_PATTERN_COMPACT_PREFIX):
                select_variables = select_variables.replace(
                    "?PropertyNameValue",
                    "?PropertyLabel ?PropertyNameValue ?PropertyNameUnitValue ?PropertyNameReferenceStateValue ?PropertyNameReferenceStateUnitValue",
                    1,
                )
                where_clauses_verbose.extend(ALL_PROPERTIES_PATTERNS_VERBOSE)

            elif pattern.startswith(ALL_IDENTIFIERS_PATTERN_COMPACT_PREFIX):
                select_variables = select_variables.replace(
                    "?IdentifierNameValue", "?IdentifierLabel ?IdentifierNameValue", 1
                )
                where_clauses_verbose.extend(ALL_IDENTIFIERS_PATTERNS_VERBOSE)

            else:
                raise Exception("Unexpected compact clause: " + pattern)

        return f"""SELECT DISTINCT ?label {select_variables}
WHERE {{{"".join(where_clauses_verbose).rstrip()}
}}"""

    @classmethod
    def from_string(cls, query: str):
        ptr = advance_ptr_thru_space(query)
        if not query.startswith("SELECT", ptr):
            raise ValueError("SELECT keyword is missing from the query: " + query)

        ptr += len("SELECT")
        ptr = advance_ptr_thru_space(query, ptr)
        select_variables_idx_start = ptr

        ptr = advance_ptr_to_kw(query, "WHERE", ptr)
        if ptr >= len(query):
            raise ValueError("WHERE clause is missing from the query: ", query)

        select_variables = query[select_variables_idx_start:ptr].strip()

        ptr += len("WHERE")
        ptr = advance_ptr_thru_space(query, ptr)
        if query[ptr] != "{":
            raise ValueError("Missing open bracket after WHERE keyword: ", query)

        ptr += 1
        # assume that WHERE clause contains only basic triple patterns, FILTER, and VALUES clauses
        where_clauses = []
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

            elif query.startswith("VALUES", ptr):
                ptr += len("VALUES")
                ptr = advance_ptr_thru_space(query, ptr)
                if query[ptr] != "(":
                    raise ValueError(
                        "Open bracket is missing from VALUES clause: " + query
                    )
                ptr += 1
                ptr = advance_ptr_thru_space(query, ptr)
                ptr = advance_ptr_to_space(query, ptr)
                ptr = advance_ptr_thru_space(query, ptr)
                if query[ptr] != ")":
                    raise ValueError(
                        "Close bracket is missing from VALUES clause: " + query
                    )
                ptr += 1
                ptr = advance_ptr_thru_space(query, ptr)
                if query[ptr] != "{":
                    raise ValueError(
                        "Open curly bracket is missing from VALUES clause: " + query
                    )
                ptr += 1
                while True:
                    ptr = advance_ptr_thru_space(query, ptr)
                    if ptr >= len(query):
                        raise ValueError(
                            "Close curly bracket is missing from VALUES clause: "
                            + query
                        )
                    if query[ptr] == "}":
                        ptr += 1
                        break
                    elif query[ptr] == "(":
                        ptr = advance_ptr_to_kw(query, '"', ptr)
                        if ptr >= len(query):
                            raise ValueError(
                                "Value string is missing from VALUES clause: " + query
                            )
                        ptr += 1
                        ptr = advance_ptr_to_kw(query, '"', ptr)
                        if ptr >= len(query):
                            raise ValueError(
                                "Close quotation mark is missing from value string in VALUES clause: "
                                + query
                            )
                        ptr += 1
                        ptr = advance_ptr_to_kw(query, ")", ptr)
                        if ptr >= len(query):
                            raise ValueError(
                                "Close brack is missing from value string in VALUES clause: "
                                + query
                            )
                        ptr += 1
                    else:
                        raise ValueError("Unexpected VALUES clause: " + query)

            else:  # assume it's the triple pattern
                ptr = advance_ptr_to_kw(query, ".", ptr)
                if ptr >= len(query):
                    raise ValueError(
                        "Full-stop is missing from triple pattern: " + query[start_idx:]
                    )
                ptr += 1

            end_idx = ptr
            where_clauses.append(query[start_idx:end_idx].strip())

        return cls(select_variables, where_clauses)
