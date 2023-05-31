package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;

/**
 * A class representing the ExtrudedAreaSolid concept in OntoBIM.
 *
 * @author qhouyee
 */
public class ExtrudedAreaSolid {
    private final String iri;
    private final Double extrusionDepth;
    private final String extrusionDirIRI;
    private final String extrusionPositionIRI;
    private final String profileDefIRI;


    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param instance             This instance's IRI in the IfcOwl triples.
     * @param extrusionPositionIri The starting position of the extrusion, which is a local placement.
     * @param extrusionDirIri      The direction of extrusion, indicated by a direction vector.
     * @param extrusionDepth       The depth of the extrusion.
     * @param profileDefIri        The extrusion's profile definition instance. Could be rectangle, circle, or others.
     */
    public ExtrudedAreaSolid(String instance, String extrusionPositionIri, String extrusionDirIri, String extrusionDepth, String profileDefIri) {
        this.iri = StatementHandler.createInstanceFromIRI(instance, OntoBimConstant.EXTRUDED_AREA_SOLID_CLASS);
        this.extrusionPositionIRI = extrusionPositionIri;
        this.extrusionDirIRI = extrusionDirIri;
        this.extrusionDepth = Double.valueOf(extrusionDepth);
        this.profileDefIRI = profileDefIri;
    }

    /**
     * Generate and add the statements required for this class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_EXTRUDED_AREA_SOLID_CLASS);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_EXTRUSION_POSITION, this.extrusionPositionIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_EXTRUSION_DIRECTION, this.extrusionDirIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_EXTRUSION_DEPTH, this.extrusionDepth);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_EXTRUSION_PROFILE, this.profileDefIRI);
    }
}
