from core.data_processing.exceptions import InvalidCompactQueryError
from core.utils import advance_ptr_thru_space, advance_ptr_to_kw


class CompactQueryParser:
    def parse(self, query: str):
        ptr = advance_ptr_thru_space(query)
        if not query.startswith("SELECT", ptr):
            raise InvalidCompactQueryError("SELECT keyword is missing from the query: " + query)

        ptr += len("SELECT")
        ptr = advance_ptr_thru_space(query, ptr)
        select_variables_idx_start = ptr

        ptr = advance_ptr_to_kw(query, "WHERE", ptr)
        if ptr >= len(query):
            raise InvalidCompactQueryError("WHERE clause is missing from the query: ", query)

        select_variables = query[select_variables_idx_start:ptr].strip()

        ptr += len("WHERE")
        ptr = advance_ptr_thru_space(query, ptr)
        if query[ptr] != "{":
            raise InvalidCompactQueryError("Missing open bracket after WHERE keyword: ", query)

        ptr += 1
        # assume that WHERE clause contains only basic triple patterns, FILTER, and VALUES clauses
        where_clauses = []
        while True:
            ptr = advance_ptr_thru_space(query, ptr)

            if query[ptr] == "}":
                break
            if ptr >= len(query):
                raise InvalidCompactQueryError(
                    "Close curly bracket is missing from the WHERE clause: " + query
                )

            start_idx = ptr
            if query.startswith("FILTER", ptr):
                ptr += len("FILTER")
                ptr = advance_ptr_thru_space(query, ptr)
                if query[ptr] != "(":
                    raise InvalidCompactQueryError(
                        "Open bracket is missing from FILTER clause: "
                        + query[start_idx:]
                    )

                open_brac_num = 1
                ptr += 1
                while open_brac_num > 0 and ptr < len(query):
                    if query[ptr] == "(":
                        open_brac_num += 1
                    elif query[ptr] == ")":
                        open_brac_num -= 1
                    ptr += 1
                if open_brac_num > 0:
                    raise InvalidCompactQueryError("There is an unclosed bracket in the FILTER clause: " + query)

            elif query.startswith("VALUES", ptr):
                ptr += len("VALUES")
                ptr = advance_ptr_thru_space(query, ptr)
                if query[ptr] != "(":
                    raise InvalidCompactQueryError(
                        "Open bracket is missing from VALUES clause: " + query
                    )
                ptr += 1
                ptr = advance_ptr_to_kw(query, ")", ptr) 
                if query[ptr] != ")":
                    raise InvalidCompactQueryError(
                        "Close bracket is missing from VALUES clause: " + query
                    )
                ptr += 1
                ptr = advance_ptr_thru_space(query, ptr)
                if query[ptr] != "{":
                    raise InvalidCompactQueryError(
                        "Open curly bracket is missing from VALUES clause: " + query
                    )
                ptr += 1
                while True:
                    ptr = advance_ptr_thru_space(query, ptr)
                    if ptr >= len(query):
                        raise InvalidCompactQueryError(
                            "Close curly bracket is missing from VALUES clause: "
                            + query
                        )
                    if query[ptr] == "}":
                        ptr += 1
                        break
                    elif query[ptr] == "(":
                        ptr = advance_ptr_to_kw(query, '"', ptr)
                        if ptr >= len(query):
                            raise InvalidCompactQueryError(
                                "Value string is missing from VALUES clause: " + query
                            )
                        ptr += 1
                        ptr = advance_ptr_to_kw(query, '"', ptr)
                        if ptr >= len(query):
                            raise InvalidCompactQueryError(
                                "Close quotation mark is missing from value string in VALUES clause: "
                                + query
                            )
                        ptr += 1
                        ptr = advance_ptr_to_kw(query, ")", ptr)
                        if ptr >= len(query):
                            raise InvalidCompactQueryError(
                                "Close brack is missing from value string in VALUES clause: "
                                + query
                            )
                        ptr += 1
                    else:
                        raise InvalidCompactQueryError("Unexpected VALUES clause: " + query)

            else:  # assume it's the triple pattern
                ptr = advance_ptr_to_kw(query, ".", ptr)
                if ptr >= len(query):
                    raise InvalidCompactQueryError(
                        "Full-stop is missing from triple pattern: " + query[start_idx:]
                    )
                ptr += 1

            end_idx = ptr
            where_clauses.append(query[start_idx:end_idx].strip())

        return select_variables, where_clauses
