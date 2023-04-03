package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.ArrayDeque;
import java.util.LinkedHashSet;
import java.util.Queue;
import java.util.UUID;

/**
 * A class representing the FacetedBrep concept in OntoBIM.
 *
 * @author qhouyee
 */
public class FacetedBrep {
    private final String bimIRI;
    private final String closedShellIRI;
    private final Queue<Object[]> faces;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param instance                     This instance's IRI in the IfcOwl triples.
     * @param faceBoundaryIri              The IRI of the first face boundary in the geometry, usually a poly loop.
     * @param isLoopNonInversedOrientation Indicates whether the boundary and face are in the same sense or orientation.
     */
    public FacetedBrep(String instance, String faceBoundaryIri, boolean isLoopNonInversedOrientation) {
        this.bimIRI = StatementHandler.createInstanceFromIRI(instance, OntoBimConstant.FACETED_BREP_CLASS);
        // A new closed shell instance should be generated as it never overlaps with other surface
        this.closedShellIRI = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.CLOSED_SHELL_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        // Generate a new queue and triples for the first face boundary
        this.faces = new ArrayDeque<>();
        appendFace(faceBoundaryIri, isLoopNonInversedOrientation);
    }

    /**
     * Append the current face and its related information to the queue.
     *
     * @param faceBoundaryIri              The IRI of the face boundary in the geometry, usually a poly loop.
     * @param isLoopNonInversedOrientation Indicates whether the boundary and face are in the same sense or orientation.
     */
    public void appendFace(String faceBoundaryIri, boolean isLoopNonInversedOrientation) {
        Object[] currentFaceValues = new Object[4];
        currentFaceValues[0] = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.FACE_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        currentFaceValues[1] = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.FACE_OUTER_BOUND_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        currentFaceValues[2] = StatementHandler.createInstanceFromIRI(faceBoundaryIri, OntoBimConstant.POLYLOOP_CLASS);
        currentFaceValues[3] = isLoopNonInversedOrientation;
        this.faces.offer(currentFaceValues);
    }


    /**
     * Generate Geometric Void statements required.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.bimIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_FACETED_BREP_CLASS);
        StatementHandler.addStatement(statementSet, this.bimIRI, OntoBimConstant.BIM_HAS_EXTERIOR_BOUNDARY, this.closedShellIRI);
        StatementHandler.addStatement(statementSet, this.closedShellIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_CLOSED_SHELL_CLASS);
        while(!this.faces.isEmpty()){
            Object[] currentFaceValues = this.faces.poll();
            StatementHandler.addStatement(statementSet, this.closedShellIRI, OntoBimConstant.BIM_HAS_CONNECTED_FACES, currentFaceValues[0]);
            StatementHandler.addStatement(statementSet, (String) currentFaceValues[0], OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_FACE_CLASS);
            StatementHandler.addStatement(statementSet, (String) currentFaceValues[0], OntoBimConstant.BIM_HAS_BOUNDS, currentFaceValues[1]);
            StatementHandler.addStatement(statementSet, (String) currentFaceValues[1], OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_FACE_OUTER_BOUND_CLASS);
            StatementHandler.addStatement(statementSet, (String) currentFaceValues[1], OntoBimConstant.BIM_HAS_FACE_BOUNDARY, currentFaceValues[2]);
            StatementHandler.addStatement(statementSet, (String) currentFaceValues[1], OntoBimConstant.BIM_IS_LOOP_NON_INVERSED_ORIENTATION, currentFaceValues[3]);
        }
    }
}
