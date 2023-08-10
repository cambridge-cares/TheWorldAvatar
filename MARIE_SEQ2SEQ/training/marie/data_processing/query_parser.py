from typing import List

from marie.utils import advance_idx_thru_space, advance_idx_to_kw, advance_idx_to_space


RESULT_CLAUSE_KWS = ["SELECT"]


class AbstractQueryRep:
    def __init__(
        self,
        result_clause: str,
        graph_patterns: List[str],
        solution_modifiers: List[str],
    ):
        self.result_clause = result_clause
        self.graph_patterns = graph_patterns
        self.solution_modifiers = solution_modifiers

    def __eq__(self, other):
        return (
            isinstance(other, AbstractQueryRep)
            and self.result_clause == other.result_clause
            and self.graph_patterns == other.graph_patterns
            and self.solution_modifiers == other.solution_modifiers
        )

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

        ptr = advance_idx_to_kw(query, "WHERE", ptr)
        if ptr >= len(query):
            raise ValueError("WHERE clause is missing from the query: ", query)
        where_clause_idx = ptr

        result_clause = query[result_clause_idx:where_clause_idx].strip()

        ptr = advance_idx_to_kw(query, "{", ptr)
        if ptr >= len(query):
            raise ValueError("Missing open bracket after WHERE keyword: ", query)

        ptr += 1
        # assume that WHERE clause contains only basic triple patterns and FILTER clauses
        graph_patterns = []
        while not query[ptr] != "}":
            start_idx = ptr
            if query.startswith("FILTER"):
                ptr = advance_idx_thru_space(query, ptr)
                ptr += 1
                if query[ptr] != "(":
                    raise ValueError(
                        "Open curly bracket is missing from FITLER clause: ", query
                    )

                ptr = advance_idx_to_kw(query, ")", ptr)
                if ptr >= len(query):
                    raise ValueError(
                        "Close curly bracket is missing from FILTER clause: ", query
                    )
            else:  # assume it's the triple pattern
                ptr = advance_idx_thru_space(query, ptr)  # advance to head entity
                ptr = advance_idx_to_space(query, ptr)
                ptr = advance_idx_thru_space(query, ptr)  # advance to relation
                ptr = advance_idx_to_space(query, ptr)
                ptr = advance_idx_thru_space(query, ptr)  # advance to tail
                ptr = advance_idx_to_kw(query, ".", ptr)
                if ptr >= len(query):
                    raise ValueError(
                        "Full-stop is missing from triple pattern: ", query
                    )
                ptr += 1

            end_idx = ptr
            graph_patterns.append(query[start_idx:end_idx].strip())

        solution_modifiers = []

        return cls(result_clause, graph_patterns, solution_modifiers)
