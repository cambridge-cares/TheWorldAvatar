package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the PolygonalBoundedHalfSpace concept in OntoBIM.
 *
 * @author qhouyee
 */
public class PolygonalBoundedHalfSpace {
    private final String iri;
    private final String positionIRI;
    private final String baseSurfaceIRI;
    private final String baseSurfacePositionIRI;
    private final String boundaryIRI;
    private final boolean agreementFlag;

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
        this.iri = StatementHandler.createInstanceFromIRI(instance, OntoBimConstant.POLYGONAL_BOUNDED_HALF_SPACE_CLASS);
        this.positionIRI = positionIri;
        // A new surface plane instance should be generated as it never overlaps with other surfaces
        this.baseSurfaceIRI = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.SURFACE_PLANE_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.baseSurfacePositionIRI = baseSurfacePositionIri;
        this.boundaryIRI = StatementHandler.createInstanceFromIRI(boundaryIri, OntoBimConstant.POLYLINE_CLASS);
        this.agreementFlag = agreementFlag;
    }

    /**
     * Generate and add the statements required for this class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_POLYGONAL_BOUNDED_HALF_SPACE_CLASS);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.positionIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_BASE_SURFACE, this.baseSurfaceIRI);
        StatementHandler.addStatement(statementSet, this.baseSurfaceIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_SURFACE_PLANE_CLASS);
        StatementHandler.addStatement(statementSet, this.baseSurfaceIRI, OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.baseSurfacePositionIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_POLYGONAL_BOUNDARY, this.boundaryIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_AGREEMENT_FLAG, this.agreementFlag);
    }
}
