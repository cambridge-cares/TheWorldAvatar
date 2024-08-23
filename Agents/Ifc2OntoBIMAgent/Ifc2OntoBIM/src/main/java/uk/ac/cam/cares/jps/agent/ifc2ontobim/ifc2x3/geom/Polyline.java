package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.*;

/**
 * A class representing the Polyline or its child PolyLoop concept in OntoBIM.
 *
 * @author qhouyee
 */
public class Polyline {
    private final String iri;
    private final String bimClassIRI;
    private final String startingVertexIRI;
    private final Map<String, String[]> vertexIris;

    /**
     * Overloaded Constructor initialising the common inputs and sets the default class to polyline.
     * Note that queried results are not in order, and will require the nextVertexIri to determine their order
     *
     * @param instance          This instance's IRI in the IfcOwl triples.
     * @param startingVertexIri The starting vertex of the point.
     * @param currentVertexIri  The IRI of the current vertex retrieved.
     * @param currentPointIri   The current vertex's point IRI retrieved.
     * @param nextVertexIri     An optional field for IRI of the next vertex retrieved (if any exists).
     */
    public Polyline(String instance, String startingVertexIri, String currentVertexIri, String currentPointIri, String nextVertexIri) {
        this(instance, startingVertexIri, currentVertexIri, currentPointIri, nextVertexIri, OntoBimConstant.POLYLINE_CLASS);
    }

    /**
     * Standard Constructor initialising the common inputs.
     * Note that queried results are not in order, and will require the nextVertexIri to determine their order
     *
     * @param instance          This instance's IRI in the IfcOwl triples.
     * @param startingVertexIri The starting vertex of the point.
     * @param currentVertexIri  The IRI of the current vertex retrieved.
     * @param currentPointIri   The current vertex's point IRI retrieved.
     * @param nextVertexIri     An optional field for IRI of the next vertex retrieved (if any exists).
     * @param ontoBimClass      The OntoBIM class of this instance.
     */
    public Polyline(String instance, String startingVertexIri, String currentVertexIri, String currentPointIri, String nextVertexIri, String ontoBimClass) {
        this.iri = StatementHandler.createInstanceFromIRI(instance, ontoBimClass);
        this.bimClassIRI = ontoBimClass.equals(OntoBimConstant.POLYLOOP_CLASS) ? OntoBimConstant.BIM_POLYLOOP_CLASS : OntoBimConstant.BIM_POLYLINE_CLASS;
        this.startingVertexIRI = startingVertexIri;
        // Initialise the mappings
        this.vertexIris = new HashMap<>();
        this.appendVertex(currentVertexIri, currentPointIri, nextVertexIri);
    }

    /**
     * Append current vertex and its related information to the private mappings.
     *
     * @param currentVertexIri The IRI of the current vertex retrieved.
     * @param currentPointIri  The current vertex's point IRI retrieved.
     * @param nextVertexIri    An optional field for IRI of the next vertex retrieved (if any exists).
     */
    public void appendVertex(String currentVertexIri, String currentPointIri, String nextVertexIri) {
        String[] currentVertexValues = new String[3];
        currentVertexValues[0] = StatementHandler.createInstanceFromIRI(currentVertexIri, OntoBimConstant.LINE_VERTEX_CLASS);
        currentVertexValues[1] = currentPointIri;
        currentVertexValues[2] = nextVertexIri; // If value is null, this is assigned null and becomes optional
        this.vertexIris.put(currentVertexIri, currentVertexValues);
    }

    /**
     * Generate and add the statements required for this class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, this.bimClassIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_STARTING_VERTEX, StatementHandler.createInstanceFromIRI(this.startingVertexIRI, OntoBimConstant.LINE_VERTEX_CLASS));
        // First retrieve the starting vertex information
        String[] currentVertexData = this.vertexIris.get(this.startingVertexIRI);
        // While there is still a next instance from the current instance
        while (currentVertexData[2] != null) {
            // Only create these statements if the next vertex exists, else it will be a duplicate
            if (this.vertexIris.containsKey(currentVertexData[2])) {
                // Add these statements
                StatementHandler.addStatement(statementSet, currentVertexData[0], OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_LINE_VERTEX_CLASS);
                StatementHandler.addStatement(statementSet, currentVertexData[0], OntoBimConstant.BIM_HAS_REF_POINT, currentVertexData[1]);
                StatementHandler.addStatement(statementSet, currentVertexData[0], OntoBimConstant.BIM_HAS_NEXT_VERTEX, StatementHandler.createInstanceFromIRI(currentVertexData[2], OntoBimConstant.LINE_VERTEX_CLASS));
                currentVertexData = this.vertexIris.get(currentVertexData[2]);
            } else {
                throw new IllegalArgumentException("Detected a next line vertex! But its contents does not exist for " + currentVertexData[0]);
            }
        }
        // When there is no next instance, simply generate the following statements
        // Position this here so that it will only run for the last vertex
        StatementHandler.addStatement(statementSet, currentVertexData[0], OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_LINE_VERTEX_CLASS);
        StatementHandler.addStatement(statementSet, currentVertexData[0], OntoBimConstant.BIM_HAS_REF_POINT, currentVertexData[1]);
    }
}
