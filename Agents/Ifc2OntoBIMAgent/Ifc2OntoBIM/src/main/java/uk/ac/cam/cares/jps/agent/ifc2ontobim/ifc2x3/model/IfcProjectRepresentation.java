package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

public class IfcProjectRepresentation {
    private String name = null;
    private String phase = null;
    private final String iri;
    private final GeometricRepresentationContext context;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri   The instance IRI of IfcProject in IfcOwl.
     * @param name  An optional field for the name of this IFC project.
     * @param phase An optional field for the phase of the project in String.
     * @param context The context for all geometric representation in the project.
     */
    public IfcProjectRepresentation(String iri, String name, String phase, GeometricRepresentationContext context) {
        String prefix = iri.contains(OntoBimConstant.HASH) ? StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.HASH) + OntoBimConstant.HASH :
                StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.BACKSLASH) + OntoBimConstant.BACKSLASH;
        // Generate new project IRI
        this.iri = prefix + OntoBimConstant.PROJECT_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.context = context;
        // Parse the optional values
        if (name != null) {
            this.name = name;
        }
        if (phase != null) {
            this.phase = phase;
        }
    }

    public String getIri() {return this.iri;}

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_PROJECT_CLASS);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_CONTEXT, this.context.getIri());
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.RDFS_LABEL, this.name, false);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_PHASE, this.phase, false);
    }
}
