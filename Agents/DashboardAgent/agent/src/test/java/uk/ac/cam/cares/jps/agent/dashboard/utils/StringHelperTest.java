package uk.ac.cam.cares.jps.agent.dashboard.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class StringHelperTest {
    private static final String SPARQL_VAR_FORMAT_TEST_CASE1 = "concept";
    private static final String SPARQL_VAR_FORMAT_TEST_CASE2 = "node";
    private static final String VAR_FORMAT_TEST_CASE1 = "LOWERCASE";
    private static final String VAR_FORMAT_TEST_CASE2 = "Ensure it is conjoined";
    private static final String VAR_FORMAT_TEST_CASE3 = "nochangerequired";
    private static final String VAR_FORMAT_TEST_CASE4 = "TEST-dash-removal";
    private static final String VAR_FORMAT_TEST_CASE5 = "tESt_under_score_";
    private static final String CAPITAL_TEST_CASE1 = "ThisIsTest";
    private static final String CAPITAL_TEST_CASE2 = "ThisisTest";
    private static final String CAPITAL_TEST_CASE3 = "thisisatest";
    private static final String CAPITAL_TEST_CASE4 = "ThisIsATest";
    private static final String CAPITAL_TEST_CASE5 = "EndOfLINENoSPLIT";
    private static final String CAPITAL_TEST_CASE6 = "TESTMustSplit";
    private static final String SINGLE_QUOTE_CASE = "Hughes' room";

    @Test
    void testFormatSparqlVarName() {
        assertEquals(" ?" + SPARQL_VAR_FORMAT_TEST_CASE1, StringHelper.formatSparqlVarName(SPARQL_VAR_FORMAT_TEST_CASE1));
        assertEquals(" ?" + SPARQL_VAR_FORMAT_TEST_CASE2, StringHelper.formatSparqlVarName(SPARQL_VAR_FORMAT_TEST_CASE2));
    }

    @Test
    void testFormatVariableName() {
        assertEquals("lowercase", StringHelper.formatVariableName(VAR_FORMAT_TEST_CASE1));
        assertEquals("ensureitisconjoined", StringHelper.formatVariableName(VAR_FORMAT_TEST_CASE2));
        assertEquals(VAR_FORMAT_TEST_CASE3, StringHelper.formatVariableName(VAR_FORMAT_TEST_CASE3));
        assertEquals("testdashremoval", StringHelper.formatVariableName(VAR_FORMAT_TEST_CASE4));
        assertEquals("testunderscore", StringHelper.formatVariableName(VAR_FORMAT_TEST_CASE5));
    }

    @Test
    void testAddSpaceBetweenCapitalWords() {
        assertEquals("This Is Test", StringHelper.addSpaceBetweenCapitalWords(CAPITAL_TEST_CASE1));
        assertEquals("Thisis Test", StringHelper.addSpaceBetweenCapitalWords(CAPITAL_TEST_CASE2));
        assertEquals(CAPITAL_TEST_CASE3, StringHelper.addSpaceBetweenCapitalWords(CAPITAL_TEST_CASE3));
        assertEquals("This Is A Test", StringHelper.addSpaceBetweenCapitalWords(CAPITAL_TEST_CASE4));
        assertEquals("End Of LINE No SPLIT", StringHelper.addSpaceBetweenCapitalWords(CAPITAL_TEST_CASE5));
        assertEquals("TEST Must Split", StringHelper.addSpaceBetweenCapitalWords(CAPITAL_TEST_CASE6));
    }

    @Test
    void testAddCharacterEscapingForSingleQuotes() {
        assertEquals(SPARQL_VAR_FORMAT_TEST_CASE1, StringHelper.addCharacterEscapingForSingleQuotes(SPARQL_VAR_FORMAT_TEST_CASE1));
        assertEquals(VAR_FORMAT_TEST_CASE4, StringHelper.addCharacterEscapingForSingleQuotes(VAR_FORMAT_TEST_CASE4));
        assertEquals(CAPITAL_TEST_CASE3, StringHelper.addCharacterEscapingForSingleQuotes(CAPITAL_TEST_CASE3));
        assertEquals("Hughes'' room", StringHelper.addCharacterEscapingForSingleQuotes(SINGLE_QUOTE_CASE));
    }
}