package com.cmclinnovations.stack.clients.utils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Path;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class SparqlRulesFileTest {

    @Test
    void defaultConstructor() throws IOException {
        SparqlRulesFile sparqlRulesFile = new SparqlRulesFile();
        String expected = "rules = []\n";
        checkRules(sparqlRulesFile, expected);
    }

    @Test
    void fileConstructor() throws IOException, URISyntaxException {
        Path rulesFilePath = Path.of(SparqlRulesFileTest.class.getResource("rules.toml").toURI());
        SparqlRulesFile sparqlRulesFile = new SparqlRulesFile(rulesFilePath);
        String expected = "rules = [\"CONSTRUCT{ ?s ?p ?o }\\n    WHERE {?s ?p ?o}\\n    \"]\n";
        checkRules(sparqlRulesFile, expected);
    }

    @Test
    void testAddRules() throws IOException, URISyntaxException {
        Path rulesFilePath = Path.of(SparqlRulesFileTest.class.getResource("rules.toml").toURI());
        SparqlRulesFile sparqlRulesFile = new SparqlRulesFile();
        sparqlRulesFile.addRules(rulesFilePath);
        String expected = "rules = [\"CONSTRUCT{ ?s ?p ?o }\\n    WHERE {?s ?p ?o}\\n    \"]\n";
        checkRules(sparqlRulesFile, expected);
    }

    @Test
    void testAddRules2() throws URISyntaxException, IOException {
        Path rulesFilePath = Path.of(SparqlRulesFileTest.class.getResource("rules.toml").toURI());
        SparqlRulesFile sparqlRulesFile = new SparqlRulesFile(rulesFilePath);
        Path rulesFilePath2 = Path.of(SparqlRulesFileTest.class.getResource("rules2.toml").toURI());
        sparqlRulesFile.addRules(rulesFilePath2);
        String expected = "rules = [\"CONSTRUCT{ ?s ?p ?o }\\n    WHERE {?s ?p ?o}\\n    \", \"CONSTRUCT{ ?a ?b ?c }\\n    WHERE {?a ?b ?c}\\n    \"]\n";
        checkRules(sparqlRulesFile, expected);
    }

    private void checkRules(SparqlRulesFile sparqlRulesFile, String expected) throws IOException {
        try (ByteArrayOutputStream of = new ByteArrayOutputStream();) {
            sparqlRulesFile.write(of);
            Assertions.assertEquals(expected, of.toString());
        }
    }
}
