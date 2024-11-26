from services.execute_data_req.sparql.process_query import SparqlQueryProcessor


class TestSparqlProcessor:
    def test_injectServiceEndpoint(self):
        # Arrange
        ns2endpoint = {
            "ontop": "http://example.org/ontop/sparql",
            "bg": "http://example.come/blazegraph/sparql",
        }
        processor = SparqlQueryProcessor(ns2endpoint=ns2endpoint)

        sparql = """SELECT ?s ?p ?o
WHERE {
    ?s ?p ?o .
    SERVICE <bg> {
        ?o ?p ?s .
    }
    SERVICE <ontop> {
        ?p ?s ?o .
    }
}"""
        expected = """SELECT ?s ?p ?o
WHERE {
    ?s ?p ?o .
    SERVICE <http://example.come/blazegraph/sparql> {
        ?o ?p ?s .
    }
    SERVICE <http://example.org/ontop/sparql> {
        ?p ?s ?o .
    }
}"""

        # Act
        actual = processor.inject_service_endpoint(sparql)

        # Assert
        assert actual == expected

    def test_injectBindings(self):
        # Arrange
        processor = SparqlQueryProcessor()
        sparql = """SELECT ?s1 ?s2 ?p1 ?p2 ?o1 ?o2
WHERE {
    ?s1 ?p1 ?o1 .
    SERVICE <bg> {
        ?s2 ?p2 ?o2 .
    }
    {
        SELECT ?s1 WHERE {
            ?o1 ?p1 ?s2 .
            ?o2 ?p2 ?s1 .
        }
    }
    ?o1 ?p1 ?s1, ?o2 .
}"""
        entity_bindings = {"s1": ["a", "b", "c"], "p2": ["d", "e"]}
        expected = """SELECT ?s1 ?s2 ?p1 ?p2 ?o1 ?o2
WHERE {
    VALUES ?s1 { <a> <b> <c> } ?s1 ?p1 ?o1 .
    SERVICE <bg> {
        VALUES ?p2 { <d> <e> } ?s2 ?p2 ?o2 .
    }
    {
        SELECT ?s1 WHERE {
            ?o1 ?p1 ?s2 .
            VALUES ?s1 { <a> <b> <c> } VALUES ?p2 { <d> <e> } ?o2 ?p2 ?s1 .
        }
    }
    VALUES ?s1 { <a> <b> <c> } ?o1 ?p1 ?s1, ?o2 .
}"""

        # Act
        actual = processor.inject_bindings(
            sparql=sparql, entity_bindings=entity_bindings, const_bindings=dict()
        )

        # Assert
        assert actual == expected
