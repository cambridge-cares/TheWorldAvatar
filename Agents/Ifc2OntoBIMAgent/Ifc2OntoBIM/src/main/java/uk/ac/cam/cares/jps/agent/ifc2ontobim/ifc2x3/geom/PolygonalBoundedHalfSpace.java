package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;

/**
 * A class representing the PolygonalBoundedHalfSpace concept in OntoBIM.
 *
 * @author qhouyee
 */
public class PolygonalBoundedHalfSpace extends HalfSpaceSolid {
    private final String iri;
    private final String positionIRI;
    private final String boundaryIRI;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param instance               This instance's IRI in the IfcOwl triples.
     * @param positionIri            The position of the half-space, indicated by a local placement.
     * @param baseSurfacePositionIri The position of the surface defining side of the half-space.
     * @param boundaryIri            The boundaries of the half-space.
     * @param agreementFlag          A boolean indicating the direction of the half-space's subset.
     */
    public PolygonalBoundedHalfSpace(String instance, String positionIri, String baseSurfacePositionIri, String boundaryIri, boolean agreementFlag) {
        super(instance, baseSurfacePositionIri, agreementFlag);
        this.iri = StatementHandler.createInstanceFromIRI(instance, OntoBimConstant.POLYGONAL_BOUNDED_HALF_SPACE_CLASS);
        this.positionIRI = positionIri;
        this.boundaryIRI = StatementHandler.createInstanceFromIRI(boundaryIri, OntoBimConstant.POLYLINE_CLASS);
    }

    /**
     * Generate and add the statements required for this class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        super.constructStatements(statementSet, this.iri);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_POLYGONAL_BOUNDED_HALF_SPACE_CLASS);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.positionIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_POLYGONAL_BOUNDARY, this.boundaryIRI);
    }
}
