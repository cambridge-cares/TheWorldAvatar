package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

public class IfcProjectRepresentation {
    private String name = null;
    private String phase = null;
    private final String prefix;
    private final String iri;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri   The instance IRI of IfcProject in IfcOwl.
     * @param name  An optional field for the name of this IFC project.
     * @param phase An optional field for the phase of the project in String.
     */
    public IfcProjectRepresentation(String iri, String name, String phase) {
        this.prefix = iri.contains(OntoBimConstant.HASH) ? StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.HASH) + OntoBimConstant.HASH :
                StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.BACKSLASH) + OntoBimConstant.BACKSLASH;
        // Generate new project IRI
        this.iri = this.prefix + OntoBimConstant.PROJECT_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();

        // Parse the optional values
        if (name != null) {
            this.name = name;
        }
        if (phase != null) {
            this.phase = phase;
        }
    }

    protected String getIri() {return this.iri;}

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_PROJECT_CLASS);
        if (this.name != null) {
            StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDFS_LABEL, this.name, false);
        }
        if (this.phase != null) {
            StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_PHASE, this.phase, false);
        }
    }
}
