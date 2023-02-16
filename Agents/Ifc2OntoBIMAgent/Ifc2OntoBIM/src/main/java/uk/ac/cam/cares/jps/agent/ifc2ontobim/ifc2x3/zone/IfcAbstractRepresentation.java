package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.LinkedHashSet;

/**
 * A class representing the IfcAbstractRepresentation concept in OntoBIM.
 *
 * @author qhouyee
 */
public class IfcAbstractRepresentation {
    private final String iri;
    private final String prefix;
    private static final String BACKSLASH = "/";
    private static final String HASH = "#";
    protected static final String METRE_UNIT = "m";

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param iri The instance IRI to be created.
     */
    public IfcAbstractRepresentation(String iri) {
        this.iri = iri;
        this.prefix = iri.contains(HASH) ? StringUtils.getStringBeforeLastCharacterOccurrence(iri, HASH) + HASH :
                StringUtils.getStringBeforeLastCharacterOccurrence(iri, BACKSLASH) + BACKSLASH;
    }

    protected String getIri() {return this.iri;}
    protected String getPrefix() {return this.prefix;}

    /**
     * An abstract method that must be overridden and used in each subclass
     * to generate and add statements to the existing set.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
    }
}
