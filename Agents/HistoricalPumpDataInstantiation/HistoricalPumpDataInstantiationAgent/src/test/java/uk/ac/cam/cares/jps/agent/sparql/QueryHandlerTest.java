package uk.ac.cam.cares.jps.agent.sparql;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class QueryHandlerTest {
    private static final String baseNamespace = "http://www.example.org/kg/base";

    @Test
    void testGenInsertPrefixMapping() {
        InsertBuilder builder = new InsertBuilder();
        QueryHandler.genInsertPrefixMapping(builder, baseNamespace);
        String expectedStatement = genExpectedQuery();
        assertEquals(expectedStatement, builder.buildString());
    }

    private static String genExpectedQuery() {
        String expectedStatement = InsertBuilderTest.PREFIX + OntologyConstant.ONTOCAPE_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.ONTOCAPE_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.ONTOHEATNETWORK_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.ONTOHEATNETWORK_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.UBEMMP_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.UBEMMP_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.QUDT_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.QUDT_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.SKOS_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.SKOS_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.OM_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.OM_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.XSD_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.XSD_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.RDFS_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.RDFS_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.RDF_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + OntologyConstant.RDF_URI + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.PREFIX + OntologyConstant.BASE_PREFIX + InsertBuilderTest.COLON
                + InsertBuilderTest.OPEN_ANCHOR + baseNamespace + InsertBuilderTest.CLOSED_ANCHOR + InsertBuilderTest.NEWLINE;
        expectedStatement += InsertBuilderTest.INSERT_DATA_EMPTY;
        return expectedStatement;
    }
}
