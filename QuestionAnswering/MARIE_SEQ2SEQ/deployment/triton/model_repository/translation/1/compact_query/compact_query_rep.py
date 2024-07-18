from typing import List

from .constants import (
    ALL_IDENTIFIERS_PATTERN_COMPACT_PREFIX,
    ALL_IDENTIFIERS_PATTERNS_VERBOSE,
    ALL_PROPERTIES_PATTERN_COMPACT_PREFIX,
    ALL_PROPERTIES_PATTERNS_VERBOSE,
    CHEMCLASS_PATTERNS_VERBOSE,
    IDENTIFIER_NAMES_SET,
    IDENTIFIER_PATTERNS_VERBOSE,
    ONTOSPECIES_PATTERN_COMPACT_PREFIX,
    PROPERTY_NAMES_SET,
    PROPERTY_PATTERNS_VERBOSE,
    SPECIES_FROMHEAD_PATTERN_COMPACT_PREFIX,
    SPECIES_FROMHEAD_PATTERNS_VERBOSE,
    SPECIES_FROMTAIL_PATTERN_COMPACT_PREFIX,
    SPECIES_FROMTAIL_PATTERNS_VERBOSE,
    USE_PATTERNS_VERBOSE,
)
from .parser import CompactQueryParser
from .utils import advance_ptr_to_space


class CompactQueryRep:
    compact_query_parser = CompactQueryParser()

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

    def to_string(self):
        where_clauses = "\n    ".join(self.where_clauses)
        return f"""SELECT {self.select_variables}
WHERE {{
    {where_clauses}
}}"""

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
            raise ValueError(
                "Unexpected pattern for head node: " + head_pattern
            )

        for pattern in tail_patterns:
            if pattern.startswith("FILTER"):
                where_clauses_verbose.append(f"    {pattern}\n")
            elif pattern.startswith("VALUES"):
                where_clauses_verbose.append("\n    " + pattern)
            elif pattern.startswith(
                ONTOSPECIES_PATTERN_COMPACT_PREFIX
            ) and pattern.endswith("."):
                name = pattern[
                    len(ONTOSPECIES_PATTERN_COMPACT_PREFIX) : advance_ptr_to_space(
                        pattern, len(ONTOSPECIES_PATTERN_COMPACT_PREFIX)
                    )
                ].strip()
                _, i = self.split_name_and_suffix(name)

                if name in PROPERTY_NAMES_SET:
                    select_variables = select_variables.replace(
                        f"?{name}Value",
                        f"?{name}Value ?{name}UnitValue ?{name}ReferenceStateValue ?{name}ReferenceStateUnitValue",
                        1,
                    )
                    where_clauses_verbose.append(PROPERTY_PATTERNS_VERBOSE(name))
                elif name in IDENTIFIER_NAMES_SET:
                    where_clauses_verbose.append(IDENTIFIER_PATTERNS_VERBOSE(name))
                elif name == "Use":
                    where_clauses_verbose.append(USE_PATTERNS_VERBOSE(i))
                elif name == "ChemicalClass":
                    where_clauses_verbose.append(CHEMCLASS_PATTERNS_VERBOSE(i))
                else:
                    raise InvalidCompactQueryError(
                        f"Unrecognized relation `os:has{name}`."
                    )

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
                raise InvalidCompactQueryError("Unexpected compact clause: " + pattern)

        return f"""SELECT DISTINCT ?label {select_variables}
WHERE {{{"".join(where_clauses_verbose).rstrip()}
}}"""

    def split_name_and_suffix(self, text: str):
        i = len(text) - 1
        while i >= 0 and text[i].isdigit():
            i -= 1
        return text[: i + 1], text[i + 1 :]

    @classmethod
    def from_string(cls, query: str):
        return cls(*cls.compact_query_parser.parse(query))
