package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class QueryHandlerTest {
   @Test
    void testGenPrefixMappingSelectBuilder() {
        SelectBuilder builder = new SelectBuilder();
        QueryHandler.genPrefixMapping(builder);
        String namespaces = builder.buildString();
        List<String> expected = genExpectedSelectBuilderPrefixList();
        expected.forEach(line -> assertTrue(namespaces.contains(line)));
    }

    @Test
    void testGenPrefixMappingInsertQueryBuilder() {
        StringBuilder builder = new StringBuilder();
        QueryHandler.genPrefixMapping(builder);
        String namespaces = builder.toString();
        List<String> expected = genExpectedInsertBuilderPrefixList();
        expected.forEach(line -> assertTrue(namespaces.contains(line)));
    }

    private static List<String> genExpectedSelectBuilderPrefixList() {
        List<String> results = new ArrayList<>();
        results.add("ontotimeseries: <https://www.theworldavatar.com/kg/ontotimeseries/>");
        results.add("rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
        results.add("rdfs: <http://www.w3.org/2000/01/rdf-schema#>");
        results.add("bot:  <https://w3id.org/bot#>");
        results.add("bim:  <http://www.theworldavatar.com/ontology/ontobim/ontoBIM#>");
        return results;
    }

    private static List<String> genExpectedInsertBuilderPrefixList() {
        List<String> results = new ArrayList<>();
        results.add("PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
        results.add("PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>");
        results.add("PREFIX saref:<https://saref.etsi.org/core/>");
        results.add("PREFIX skos:<http://www.w3.org/2004/02/skos/core#>");
        results.add("PREFIX qudt:<http://qudt.org/schema/qudt/>");
        results.add("PREFIX ontoubemmp:<https://www.theworldavatar.com/kg/ontoubemmp/>");
        results.add("PREFIX builtenv:<http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl#>");
        return results;
    }
}