package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the HalfSpaceSolid concept in OntoBIM.
 *
 * @author qhouyee
 */
public class HalfSpaceSolid {
    private final String iri;
    private final String baseSurfaceIRI;
    private final String baseSurfacePositionIRI;
    private final boolean agreementFlag;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param instance               This instance's IRI in the IfcOwl triples.
     * @param baseSurfacePositionIri The position of the surface defining side of the half-space.
     * @param agreementFlag          A boolean indicating the direction of the half-space's subset.
     */
    public HalfSpaceSolid(String instance, String baseSurfacePositionIri, boolean agreementFlag) {
        this.iri = StatementHandler.createInstanceFromIRI(instance, OntoBimConstant.HALF_SPACE_SOLID_CLASS);
        // A new surface plane instance should be generated as it never overlaps with other surfaces
        this.baseSurfaceIRI = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.SURFACE_PLANE_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.baseSurfacePositionIRI = baseSurfacePositionIri;
        this.agreementFlag = agreementFlag;
    }

    /**
     * Generate and add the statements required for this class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_HALF_SPACE_SOLID_CLASS);
        this.constructStatements(statementSet, this.iri);
    }

    /**
     * An overloaded method to generate the duplicate statements required for subclasses to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     * @param bimIRI       The geometry instance to be attached as the main.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet, String bimIRI) {
        StatementHandler.addStatement(statementSet, bimIRI, OntoBimConstant.BIM_HAS_BASE_SURFACE, this.baseSurfaceIRI);
        StatementHandler.addStatement(statementSet, this.baseSurfaceIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_SURFACE_PLANE_CLASS);
        StatementHandler.addStatement(statementSet, this.baseSurfaceIRI, OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.baseSurfacePositionIRI);
        StatementHandler.addStatement(statementSet, bimIRI, OntoBimConstant.BIM_HAS_AGREEMENT_FLAG, this.agreementFlag);
    }
}
