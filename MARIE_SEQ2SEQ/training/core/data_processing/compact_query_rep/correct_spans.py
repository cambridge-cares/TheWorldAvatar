from typing import List, Optional

import Levenshtein
from core.data_processing.compact_query_rep.utils import remove_terminal_chars

from core.utils import advance_ptr_to_kw


class SpanCorrector:
    def correct_values_clause(self, where_clauses: List[str], nlq: str):
        where_clauses = list(where_clauses)

        clause_idx: Optional[int] = None
        for i, clause in enumerate(where_clauses):
            if clause.startswith("VALUES") and clause.endswith("}"):
                clause_idx = i
                break

        if clause_idx is None:
            return self

        clause = where_clauses[clause_idx]

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
                    return self
                candidates = [
                    " ".join(words[i : i + word_num])
                    for i in range(0, len(words) - word_num + 1)
                ]
                distances = [Levenshtein.distance(val, x) for x in candidates]
                idx_min = min(range(len(distances)), key=lambda i: distances[i])

                values_corrected.append(candidates[idx_min])

        where_clauses[
            clause_idx
        ] = f"""VALUES ( {query_var} ) {{ {' '.join([f'( "{x}" )' for x in values_corrected])} }}"""
        return where_clauses