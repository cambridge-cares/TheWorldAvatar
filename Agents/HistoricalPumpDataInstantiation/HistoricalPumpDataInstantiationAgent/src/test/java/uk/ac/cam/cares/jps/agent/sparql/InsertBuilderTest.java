package uk.ac.cam.cares.jps.agent.sparql;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class InsertBuilderTest {
    private static InsertBuilder sampleBuilder;
    // Characters
    protected static final String PREFIX = "PREFIX ";
    protected static final String COLON = ":";
    protected static final String OPEN_ANCHOR = "<";
    protected static final String CLOSED_ANCHOR = ">";
    protected static final String CLOSED_BRACKET = "}";
    protected static final String WHITESPACE = " ";
    protected static final String FULLSTOP = ".\n";
    protected static final String NEWLINE = "\n";
    // Prefixes
    private static final String PREFIX1 = "test1";
    private static final String PREFIX2 = "test2";
    private static final String NAMESPACE1 = "http://www.example.org/kb/test/";
    private static final String NAMESPACE2 = "http://www.example.com/kb/test/";
    protected static final String INSERT_DATA_START = "INSERT DATA {";
    protected static final String INSERT_DATA_EMPTY = INSERT_DATA_START + CLOSED_BRACKET;
    // Triples
    private static final String SUBJECT = "test:subject";
    private static final String PREDICATE = "rdf:type";
    private static final String OBJECT = "test:person";
    private static final String SUBJECT_IRI = NAMESPACE1 + "subject";
    private static final String PREDICATE_IRI = NAMESPACE2 + "type";
    private static final String OBJECT_IRI = NAMESPACE1 + "person";

    @BeforeEach
    void init() {
        sampleBuilder = new InsertBuilder();
    }

    @Test
    void testInsertBuilderConstructor() {
        assertNotNull(new InsertBuilder());
    }

    @Test
    void testAddPrefix() {
        String expectedPrefix1 = PREFIX + PREFIX1 + COLON + OPEN_ANCHOR + NAMESPACE1 + CLOSED_ANCHOR + NEWLINE;
        String expectedPrefix2 = PREFIX + PREFIX2 + COLON + OPEN_ANCHOR + NAMESPACE2 + CLOSED_ANCHOR + NEWLINE;
        // Tests one prefix added
        sampleBuilder.addPrefix(PREFIX1, NAMESPACE1);
        assertEquals(expectedPrefix1 + INSERT_DATA_EMPTY, sampleBuilder.buildString());
        // Tests two prefix added
        sampleBuilder = new InsertBuilder();
        sampleBuilder.addPrefix(PREFIX1, NAMESPACE1);
        sampleBuilder.addPrefix(PREFIX2, NAMESPACE2);
        assertEquals(expectedPrefix2 + expectedPrefix1 + INSERT_DATA_EMPTY, sampleBuilder.buildString());
    }

    @Test
    void testAddTriplesOverload() {
        String expectedStatement = SUBJECT + WHITESPACE + PREDICATE + WHITESPACE + OBJECT + FULLSTOP;
        sampleBuilder.addTriples(SUBJECT, PREDICATE, OBJECT);
        assertEquals(INSERT_DATA_START + expectedStatement + CLOSED_BRACKET, sampleBuilder.buildString());
    }

    @Test
    void testAddTriples() {
        String expectedStatement = OPEN_ANCHOR + SUBJECT_IRI + CLOSED_ANCHOR + WHITESPACE + PREDICATE + WHITESPACE + OBJECT + FULLSTOP;
        expectedStatement += SUBJECT + WHITESPACE + OPEN_ANCHOR + PREDICATE_IRI + CLOSED_ANCHOR + WHITESPACE + OBJECT + FULLSTOP;
        expectedStatement += SUBJECT + WHITESPACE + PREDICATE + WHITESPACE + OPEN_ANCHOR + OBJECT_IRI + CLOSED_ANCHOR + FULLSTOP;
        expectedStatement += OPEN_ANCHOR + SUBJECT_IRI + CLOSED_ANCHOR + WHITESPACE + OPEN_ANCHOR + PREDICATE_IRI + CLOSED_ANCHOR + WHITESPACE + OPEN_ANCHOR + OBJECT_IRI + CLOSED_ANCHOR + FULLSTOP;
        // Tests the different options for statements
        sampleBuilder.addTriples(SUBJECT_IRI, PREDICATE, OBJECT, 1);
        sampleBuilder.addTriples(SUBJECT, PREDICATE_IRI, OBJECT, 2);
        sampleBuilder.addTriples(SUBJECT, PREDICATE, OBJECT_IRI, 3);
        sampleBuilder.addTriples(SUBJECT_IRI, PREDICATE_IRI, OBJECT_IRI, 4);
        assertEquals(INSERT_DATA_START + expectedStatement + CLOSED_BRACKET, sampleBuilder.buildString());
    }

    @Test
    void testBuildString() {
        assertEquals(INSERT_DATA_EMPTY, sampleBuilder.buildString());
    }
}