from constants.ontospecies_keys import (
    CHEMCLASS_KEY,
    IDENTIFIER_KEYS,
    PROPERTY_KEYS,
    USE_KEY,
)


class SparqlCompact2VerboseConverter:
    def convert(self, sparql_compact: str):
        """sparql_compact: SELECT ?x WHERE {graph_patterns...}"""
        _sparql_compact = sparql_compact.strip()
        select_clause, _sparql_compact = _sparql_compact.split("WHERE", maxsplit=1)
        select_clause = select_clause.strip()

        _sparql_compact = _sparql_compact.strip()
        assert _sparql_compact.startswith("{"), sparql_compact
        assert _sparql_compact.endswith("}"), sparql_compact
        _sparql_compact = _sparql_compact[1:-1]

        graph_patterns = []
        while len(_sparql_compact) > 0:
            """sparql_compact: ?Species os:has{key} [?{key}|{literal}]"""
            _sparql_compact = _sparql_compact.strip()
            assert _sparql_compact.startswith("?Species"), sparql_compact
            _sparql_compact = _sparql_compact[len("?Species") :].strip()

            predicate, _sparql_compact = _sparql_compact.split(maxsplit=1)
            assert predicate.startswith("os:has"), sparql_compact
            key = predicate[len("os:has") :]

            _sparql_compact = _sparql_compact.strip()
            if _sparql_compact.startswith('"'):
                ptr = 1
                while ptr < len(_sparql_compact) and _sparql_compact[ptr] != '"':
                    ptr += 1
                obj = _sparql_compact[: ptr + 1]
                _sparql_compact = _sparql_compact[ptr + 1 :].strip()
                assert _sparql_compact.startswith("."), sparql_compact
                _sparql_compact = _sparql_compact[1:]
            else:
                obj, _sparql_compact = _sparql_compact.split(maxsplit=1)
                if obj.endswith("."):
                    obj = obj[:-1]
                else:
                    _sparql_compact = _sparql_compact.strip()
                    assert _sparql_compact.startswith("."), sparql_compact
                    _sparql_compact = _sparql_compact[1:]

            if key in PROPERTY_KEYS:
                """?Species os:has{PropertyName} ?{PropertyName}"""
                assert obj.startswith("?"), sparql_compact
                assert obj[1:] == key, sparql_compact
                graph_patterns.append(
                    """
    ?Species os:has{PropertyName} ?{PropertyName} .
    ?{PropertyName} os:value ?{PropertyName}Value ; os:unit/rdfs:label ?{PropertyName}UnitLabel .
    OPTIONAL {{
        ?{PropertyName} os:hasReferenceState [ os:value ?{PropertyName}ReferenceStateValue ; os:unit/rdfs:label ?{PropertyName}ReferenceStateUnitLabel ] .
    }}""".format(
                        PropertyName=key
                    )
                )

                _sparql_compact = _sparql_compact.strip()
                if _sparql_compact.startswith("FILTER"):
                    _sparql_compact = _sparql_compact[len("FILTER") :].strip()
                    assert _sparql_compact.startswith("("), sparql_compact

                    _sparql_compact = _sparql_compact[1:].strip()
                    ptr = 0
                    quote_open = False
                    while ptr < len(_sparql_compact) and (
                        _sparql_compact[ptr] != ")" or quote_open
                    ):
                        if _sparql_compact[ptr] == '"':
                            quote_open = not quote_open
                        ptr += 1
                    assert _sparql_compact[ptr] == ")", sparql_compact

                    filter_arg = _sparql_compact[:ptr].strip()
                    _sparql_compact = _sparql_compact[ptr + 1 :]
                    filter_arg = filter_arg.replace(key, key + "Value")
                    graph_patterns.append(
                        "\n    FILTER ( {arg} )".format(arg=filter_arg)
                    )
            elif key in IDENTIFIER_KEYS:
                """?Species os:has{IdentifierName} ?{IdentifierName}"""
                assert obj.startswith("?"), sparql_compact
                assert obj[1:] == key, sparql_compact
                graph_patterns.append(
                    """
    ?Species os:has{IdentifierName} ?{IdentifierName} .
    ?{IdentifierName} os:value ?{IdentifierName}Value .""".format(
                        IdentifierName=key
                    )
                )
            elif key == USE_KEY:
                """?Species os:hasUse {literal}"""
                graph_patterns.append(
                    """
    ?Species os:hasUse/rdfs:label {label} .""".format(
                        key=key, label=obj
                    )
                )
            elif key == CHEMCLASS_KEY:
                """?Species os:hasChemicalClass {literal}"""
                graph_patterns.append(
                    """
    ?Species os:hasChemicalClass*/(rdf:|!rdf:)/rdfs:subClassOf* [ rdf:type os:ChemicalClass ; rdfs:label {label} ] .""".format(
                        label=obj
                    )
                )
            else:
                raise ValueError("Unrecognized key: " + key)

        where_clause = "WHERE {{{group_graph_pattern}\n}}".format(
            group_graph_pattern="".join(graph_patterns)
        )

        return "{select} {where}".format(select=select_clause, where=where_clause)
