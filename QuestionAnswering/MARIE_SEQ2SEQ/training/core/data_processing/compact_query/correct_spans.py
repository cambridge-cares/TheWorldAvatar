from typing import List, Optional

import Levenshtein
from core.data_processing.compact_query.compact_query_rep import CompactQueryRep
from core.data_processing.compact_query.utils import remove_terminal_chars

from core.utils import advance_ptr_to_kw


class SpanCorrector:
    def _correct_values_clause(self, clause: str, nlq: str):
        # VALUES ( ?var ) { ( "a" ) ( "b" ) }
        ptr = advance_ptr_to_kw(clause, '(', len("VALUES"))
        query_var_idx_start = ptr + 1
        ptr = advance_ptr_to_kw(clause, ')', query_var_idx_start)
        query_var = clause[query_var_idx_start: ptr].strip()

        values: List[str] = []
        while ptr < len(clause) - 1:
            ptr = advance_ptr_to_kw(clause, '"', ptr)
            if ptr >= len(clause):
                break
            val_start = ptr + 1
            ptr = advance_ptr_to_kw(clause, '"', val_start)
            val = clause[val_start:ptr]
            values.append(val)
            ptr += 1

        words = [remove_terminal_chars(x) for x in nlq.split()]
        values_corrected = []
        for val in values:
            if val in nlq:
                values_corrected.append(val)
            else:
                word_num = len(val.split())
                if word_num > len(words):
                    return clause
                candidates = [
                    " ".join(words[i : i + word_num])
                    for i in range(0, len(words) - word_num + 1)
                ]
                distances = [Levenshtein.distance(val, x) for x in candidates]
                idx_min = min(range(len(distances)), key=lambda i: distances[i])

                values_corrected.append(candidates[idx_min])

        return f"""VALUES ( {query_var} ) {{ {' '.join([f'( "{x}" )' for x in values_corrected])} }}"""

    def _correct_values_clauses(self, where_clauses: List[str], nlq: str):
        where_clauses = list(where_clauses)

        values_clauses: List[str] = []
        values_clause_idxes: List[int] = []

        for i, clause in enumerate(where_clauses):
            if clause.startswith("VALUES") and clause.endswith("}"):
                values_clauses.append(clause)
                values_clause_idxes.append(i)

        if len(values_clauses) == 0:
            return where_clauses
        
        for clause_idx, values_clause in zip(values_clause_idxes, values_clauses):
            where_clauses[clause_idx] = self._correct_values_clause(values_clause, nlq)

        return where_clauses
    
    def correct(self, query: CompactQueryRep, nlq: str):
        where_clauses = self._correct_values_clauses(
            where_clauses=query.where_clauses, nlq=nlq
        )
        return CompactQueryRep(query.select_variables, where_clauses)