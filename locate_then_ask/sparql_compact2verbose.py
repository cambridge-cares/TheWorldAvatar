from constants.ontospecies_keys import (
    ABSTRACT_IDENTIFIER_KEY,
    ABSTRACT_PROPERTY_KEY,
    IDENTIFIER_KEYS,
    PROPERTY_KEYS,
)


class SparqlCompact2VerboseConverter:
    def convert_values_clause(self, sparql_compact: str):
        """VALUES ?Species { {literal} {literal} ... }"""
        sparql_compact = sparql_compact[len("VALUES") :].strip()
        assert sparql_compact.startswith("?Species"), sparql_compact
        sparql_compact = sparql_compact[len("?Species") :].strip()
        assert sparql_compact.startswith("{"), sparql_compact
        sparql_compact = sparql_compact[1:].strip()

        ptr = 0
        while ptr < len(sparql_compact) and sparql_compact[ptr] != '}':
            assert sparql_compact[ptr] == '"', sparql_compact
            _ptr = ptr + 1
            while _ptr < len(sparql_compact) and sparql_compact[_ptr] != '"':
                _ptr += 1
            assert sparql_compact[_ptr] == '"'
            
            ptr = _ptr + 1
            while ptr < len(sparql_compact) and sparql_compact[ptr].isspace():
                ptr += 1

        literals = sparql_compact[:ptr].strip()
        sparql_compact = sparql_compact[ptr + 1:]

        template = """
    VALUES ?SpeciesIdentifierValue {{ {literals} }}
    ?Species rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier [ rdf:type/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ] ."""
        return sparql_compact, template.format(literals=literals)
    
    def extract_species_predicate_obj(self, sparql_compact: str):
        assert sparql_compact.startswith("?Species"), sparql_compact
        sparql_compact = sparql_compact[len("?Species") :].strip()

        predicate, sparql_compact = sparql_compact.split(maxsplit=1)

        sparql_compact = sparql_compact.strip()
        if sparql_compact.startswith('"'):
            ptr = 1
            while ptr < len(sparql_compact) and sparql_compact[ptr] != '"':
                ptr += 1
            obj = sparql_compact[: ptr + 1]
            sparql_compact = sparql_compact[ptr + 1 :].strip()
            assert sparql_compact.startswith("."), sparql_compact
            sparql_compact = sparql_compact[1:]
        else:
            obj, sparql_compact = sparql_compact.split(maxsplit=1)
            if obj.endswith("."):
                obj = obj[:-1]
            else:
                sparql_compact = sparql_compact.strip()
                assert sparql_compact.startswith("."), sparql_compact
                sparql_compact = sparql_compact[1:]

        return sparql_compact, predicate, obj
    
    def convert_concretepredicate_property_locateclause(self, sparql_compact: str, key: str):
        template = """
    ?Species os:has{PropertyName} ?{PropertyName} .
    ?{PropertyName} os:value ?{PropertyName}Value ; os:unit/rdfs:label ?{PropertyName}UnitLabel .
    OPTIONAL {{
        ?{PropertyName} os:hasReferenceState [ os:value ?{PropertyName}ReferenceStateValue ; os:unit/rdfs:label ?{PropertyName}ReferenceStateUnitLabel ] .
    }}"""
        pattern = template.format(PropertyName=key)

        sparql_compact = sparql_compact.strip()
        assert sparql_compact.startswith("FILTER")
        
        sparql_compact = sparql_compact[len("FILTER") :].strip()
        assert sparql_compact.startswith("("), sparql_compact

        sparql_compact = sparql_compact[1:].strip()
        ptr = 0
        quote_open = False
        while ptr < len(sparql_compact) and (
            sparql_compact[ptr] != ")" or quote_open
        ):
            if sparql_compact[ptr] == '"':
                quote_open = not quote_open
            ptr += 1
        assert sparql_compact[ptr] == ")", sparql_compact

        filter_arg = sparql_compact[:ptr].strip()
        sparql_compact = sparql_compact[ptr + 1 :]
        filter_arg = filter_arg.replace(key, key + "Value")
        
        pattern += "\n    FILTER ( {arg} )".format(arg=filter_arg)
        
        return sparql_compact, pattern

    def convert_concretepredicate_identifier_locateclause(self, key: str):
        template = """
    ?Species os:has{IdentifierName}/os:value ?{IdentifierName}Value ."""
        pattern = template.format(IdentifierName=key)
        return pattern

    def convert_use_predicate_clause(self, obj: str):
        """?Species os:hasUse/rdfs:label [{literal}|?UseLabel]"""
        template = """
    ?Species os:hasUse/rdfs:label {label} ."""
        return template.format(label=obj)

    def convert_chemclass_predicate_clause(self, obj: str):
        """?Species os:hasChemicalClass/rdfs:label [{literal}|?UseLabel]"""
        template = """
    ?Species os:hasChemicalClass*/(rdf:|!rdf:)/rdfs:subClassOf* [ rdf:type os:ChemicalClass ; rdfs:label {label} ] ."""
        return template.format(label=obj)

    def convert_concretepredicate_property_askclause(self, key: str):
        template = """
    ?Species os:has{PropertyName} ?{PropertyName} .
    ?{PropertyName} os:value ?{PropertyName}Value ; os:unit/rdfs:label ?{PropertyName}UnitLabel .
    OPTIONAL {{
        ?{PropertyName} os:hasReferenceState [ os:value ?{PropertyName}ReferenceStateValue ; os:unit/rdfs:label ?{PropertyName}ReferenceStateUnitLabel ] .
    }}"""
        return template.format(PropertyName=key)

    def convert_concretepredicate_identifier_askclause(self, key: str):
        template = """
    ?Species os:has{IdentifierName}/os:value ?{IdentifierName}Value ."""
        return template.format(IdentifierName=key)

    def convert_concrete_predicate_clause(self, sparql_compact: str, predicate: str, obj: str):
        """?Species os:has{key}[|/os:value|/rdfs:label] [?{key}|?{key}Value|{literal}] ."""
        if predicate.endswith("/os:value"):
            key = predicate[len("os:has"):-len("/os:value")]
            if key in PROPERTY_KEYS:
                """?Species os:has{PropertyName}/os:value ?{PropertyName}Value"""
                assert predicate == "os:has{PropertyName}/os:value".format(PropertyName=key), sparql_compact
                assert obj == "?{PropertyName}Value".format(PropertyName=key), sparql_compact

                sparql_compact, pattern = self.convert_concretepredicate_property_locateclause(sparql_compact, key=key)
            elif key in IDENTIFIER_KEYS:
                """?Species os:has{IdentifierName}/os:value ?{IdentifierName}Value"""
                assert predicate == "os:has{IdentifierName}/os:value".format(IdentifierName=key), sparql_compact
                assert obj == "?{IdentifierName}Value".format(IdentifierName=key), sparql_compact

                sparql_compact, pattern = self.convert_concretepredicate_identifier_locateclause(key=key)
            else:
                raise ValueError("Unexpeced predicate: " + predicate)
        elif predicate.endswith("/rdfs:label"):
            if predicate == "os:hasUse/rdfs:label":
                pattern = self.convert_use_predicate_clause(obj)
            elif predicate == "os:hasChemicalClass/rdfs:label":
                pattern = self.convert_chemclass_predicate_clause(obj)
            else:
                raise ValueError("Unexpeced predicate: " + predicate)
        else:
            key = predicate[len("os:has"):]
            if key in PROPERTY_KEYS:
                """?Species os:has{PropertyName} ?{PropertyName}"""
                assert obj == "?" + key, sparql_compact

                pattern = self.convert_concretepredicate_property_askclause(key)
            elif key in IDENTIFIER_KEYS:
                """?Species os:has{IdentifierName} ?{IdentifierName}"""
                assert obj == "?" + key, sparql_compact

                pattern = self.convert_concretepredicate_identifier_askclause(key)
            else:
                raise ValueError("Unexpeced predicate: " + predicate)
        
        return sparql_compact, pattern

    def convert_abstract_predicate_clause(self, sparql_compact: str, predicate: str, obj: str):
        """?Species ?has{key}Name ?{key}Name .
           ?{key}Name rdf:type/rdfs:subClassOf os:{key} ."""
        key = predicate[len("?has"):-len("Name")]
        assert obj == "?{key}Name".format(key=key)

        sparql_compact = sparql_compact.strip()
        next_s, next_p, next_o, sparql_compact = sparql_compact.split(maxsplit=3)
        if next_o.endswith("."):
            next_o = next_o[:-1]
        else:
            sparql_compact = sparql_compact.strip()
            assert sparql_compact.startswith("."), sparql_compact
            sparql_compact = sparql_compact[1:]

        assert next_s == obj
        assert next_p == "rdf:type/rdfs:subClassOf"
        assert next_o == "os:" + key

        if key == ABSTRACT_PROPERTY_KEY:
            pattern = """
    ?Species ?hasPropertyName ?PropertyName .
    ?PropertyName rdf:type/rdfs:subClassOf os:Property ; os:value ?PropertyNameValue ; os:unit/rdfs:label ?PropertyNameUnitValue .
    OPTIONAL {
        ?PropertyName os:hasReferenceState [ os:value ?PropertyNameReferenceStateValue ; os:unit/rdfs:label ?PropertyNameReferenceStateUnitValue ] .
    }"""
        elif key == ABSTRACT_IDENTIFIER_KEY:
            pattern = """
    ?Species ?hasIdentifierName ?IdentifierName .
    ?IdentifierName rdf:type/rdfs:subClassOf os:Identifier ; os:value ?IdentifierNameValue ."""
        else:
            raise ValueError("Unrecoginzed predicate: " + predicate)

        return sparql_compact, pattern

    def convert(self, sparql_compact: str):
        """sparql_compact: SELECT ?x WHERE {graph_patterns...}"""
        sparql_compact = sparql_compact.strip()
        select_clause, sparql_compact = sparql_compact.split("WHERE", maxsplit=1)

        select_clause = select_clause.strip()
        select_clause = select_clause.replace("SELECT", "SELECT DISTINCT")

        sparql_compact = sparql_compact.strip()
        assert sparql_compact.startswith("{"), sparql_compact
        assert sparql_compact.endswith("}"), sparql_compact
        sparql_compact = sparql_compact[1:-1]

        graph_patterns = []
        while len(sparql_compact) > 0:
            sparql_compact = sparql_compact.strip()
            if sparql_compact.startswith("VALUES"):
                sparql_compact, verbose_patterns = self.convert_values_clause(sparql_compact)
                graph_patterns.append(verbose_patterns)
                continue

            sparql_compact, predicate, obj = self.extract_species_predicate_obj(sparql_compact)

            if predicate.startswith("os:has"):
                sparql_compact, pattern = self.convert_concrete_predicate_clause(sparql_compact, predicate=predicate, obj=obj)
                graph_patterns.append(pattern)
            elif predicate.startswith("?has") and predicate.endswith("Name"):
                sparql_compact, pattern = self.convert_abstract_predicate_clause(sparql_compact, predicate=predicate, obj=obj)
                graph_patterns.append(pattern)
            else:
                raise ValueError("Unrecognized predicate: " + predicate)

        where_clause = "WHERE {{{group_graph_pattern}\n}}".format(
            group_graph_pattern="".join(graph_patterns)
        )

        return "{select} {where}".format(select=select_clause, where=where_clause)
