package uk.ac.cam.cares.jps.agent.sparql;

/**
 * A INSERT query builder similar to Apache Jena various builders.
 *
 * @author qhouyee
 */
class InsertBuilder {
    private StringBuilder queryBuilder;
    private static final String INSERT_STATEMENT = "INSERT DATA {";
    private static final String PREFIX = "PREFIX";
    // Characters
    private static final String WHITESPACE = " ";
    private static final String FULLSTOP = ".\n";
    private static final String COLON = ":";
    private static final String OPEN_ANCHOR = "<";
    private static final String CLOSED_ANCHOR = ">";
    private static final String NEWLINE = "\n";

    /**
     * Default constructor to initialise the Insert Query Builder
     */
    protected InsertBuilder() {
        this.queryBuilder = new StringBuilder();
        this.queryBuilder.append(INSERT_STATEMENT);
    }

    /**
     * Add a prefix mapping to the Query Builder.
     *
     * @param prefix    The prefix of an IRI.
     * @param namespace The URI namespace in `http.//www.example.org/any/` format.
     */
    protected void addPrefix(String prefix, String namespace) {
        // Add the prefix mapping a new String builder
        StringBuilder tempBuilder = new StringBuilder();
        tempBuilder.append(PREFIX + WHITESPACE + prefix + COLON + OPEN_ANCHOR + namespace + CLOSED_ANCHOR + NEWLINE);
        // As prefix is before INSERT DATA Statement, append the current contents to this temp builder
        tempBuilder.append(this.queryBuilder);
        this.queryBuilder = tempBuilder;
    }

    /**
     * An overloaded method to add subject, predicate, object in Insert Query format. No full IRI is accepted.
     *
     * @param subject   Subject Node in prefix:Inst format.
     * @param predicate Predicate Node in prefix:property format.
     * @param object    Object Node in prefix:Inst/Class format.
     */
    protected void addTriples(String subject, String predicate, String object) {
        this.addTriples(subject, predicate, object, 0);
    }

    /**
     * Add subject, predicate, object in Insert Query format. For the fullIriNode integer,
     * 0 = no full IRIs
     * 1 = subject is full IRI
     * 2 = predicate is full IRI
     * 3 = object is full IRI
     * 4 = all nodes are full IRI
     *
     * @param subject     Subject Node in prefix:Inst format. Or as a full URI if indicated in fullIriNode.
     * @param predicate   Predicate Node in prefix:property format. Or as a full URI if indicated in fullIriNode.
     * @param object      Object Node in prefix:Inst/Class format. Or as a full URI if indicated in fullIriNode.
     * @param fullIriNode An integer indicating which node is a full IRI, ie http://www.example.org/sample/class. 0
     */
    protected void addTriples(String subject, String predicate, String object, Integer fullIriNode) {
        switch (fullIriNode) {
            case (0):
                this.queryBuilder.append(subject + WHITESPACE + predicate + WHITESPACE + object + FULLSTOP);
                break;
            case (1):
                this.queryBuilder.append(OPEN_ANCHOR + subject + CLOSED_ANCHOR + WHITESPACE + predicate + WHITESPACE + object + FULLSTOP);
                break;
            case (2):
                this.queryBuilder.append(subject + WHITESPACE + OPEN_ANCHOR + predicate + CLOSED_ANCHOR + WHITESPACE + object + FULLSTOP);
                break;
            case (3):
                this.queryBuilder.append(subject + WHITESPACE + predicate + WHITESPACE + OPEN_ANCHOR + object + CLOSED_ANCHOR + FULLSTOP);
                break;
            case (4):
                this.queryBuilder.append(OPEN_ANCHOR + subject + CLOSED_ANCHOR + WHITESPACE + OPEN_ANCHOR + predicate + CLOSED_ANCHOR + WHITESPACE + OPEN_ANCHOR + object + CLOSED_ANCHOR + FULLSTOP);
                break;
        }
    }

    /**
     * Convert the builder into a string.
     *
     * @return a String containing the Insert Query
     */
    protected String buildString() {
        // Close the builder first
        this.queryBuilder.append("}");
        return this.queryBuilder.toString();
    }
}
