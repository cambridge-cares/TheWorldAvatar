package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;

public class GeometricRepresentationSubContext {
    private final String iri;
    private final String parentContextIRI;
    private final String targetViewIRI;
    private final String contextType;
    private final String contextIdentifier;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri               The instance IRI of IfcGeometricRepresentationSubContext in IfcOwl.
     * @param parentContextIri  The instance IRI of this sub context's parent context.
     * @param contextType       An context type.
     * @param contextIdentifier The context identifier.
     * @param targetViewIri     The IRI of the IFC geometric projection enum.
     */
    public GeometricRepresentationSubContext(String iri, String parentContextIri, String contextType, String contextIdentifier, String targetViewIri) {
        this.iri = StatementHandler.createInstanceFromIRI(iri, OntoBimConstant.GEOM_SUB_CONTEXT_CLASS);
        this.parentContextIRI = StatementHandler.createInstanceFromIRI(parentContextIri, OntoBimConstant.GEOM_CONTEXT_CLASS);
        this.contextType = contextType;
        this.contextIdentifier = contextIdentifier;
        this.targetViewIRI = targetViewIri;
    }

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_GEOM_SUB_CONTEXT_CLASS);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_PARENT_CONTEXT, this.parentContextIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_CONTEXT_TYPE, this.contextType, false);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_CONTEXT_IDENTIFIER, this.contextIdentifier, false);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_TARGET_VIEW, this.targetViewIRI);
    }
}
