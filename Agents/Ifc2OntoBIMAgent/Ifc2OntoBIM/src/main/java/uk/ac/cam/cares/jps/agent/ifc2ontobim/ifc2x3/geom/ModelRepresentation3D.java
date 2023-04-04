package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StringUtils;

import java.util.ArrayDeque;
import java.util.LinkedHashSet;
import java.util.Queue;
import java.util.UUID;

/**
 * A class representing the ModelRepresentation3D concept in OntoBIM.
 *
 * @author qhouyee
 */
public class ModelRepresentation3D {
    private final String bimIri;
    private final String repType;
    private final String subContext;
    private final Queue<String> geomIris;
    private final String sourcePlacementIri;
    private final String targetPlacementIri;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param subContextIri           The sub context IRI for this object.
     * @param geomIri                 The element's geometry representation IRI.
     * @param repType                 An optional field for the geometry representation type as text.
     * @param sourcePlacementIri      An optional field for the local placement IRI of the original geometry position.
     * @param cartesianTransformerIri An optional field for the transformation operator's IRI to translate the source to target placement.
     */
    public ModelRepresentation3D(String subContextIri, String geomIri, String repType, String sourcePlacementIri, String cartesianTransformerIri) {
        this.bimIri = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.GEOM_MODEL_REP_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.subContext = StatementHandler.createInstanceFromIRI(subContextIri,OntoBimConstant.GEOM_SUB_CONTEXT_CLASS);
        // Initialise the queue and append the geometries
        this.geomIris = new ArrayDeque<>();
        appendGeometry(geomIri);
        // Optional fields: if the argument is null, the field will still be null
        this.repType = repType;
        this.sourcePlacementIri = StatementHandler.createInstanceFromOptionalIRI(sourcePlacementIri, OntoBimConstant.LOCAL_PLACEMENT_CLASS);
        this.targetPlacementIri = StatementHandler.createInstanceFromOptionalIRI(cartesianTransformerIri, OntoBimConstant.CART_TRANS_OPERATOR_CLASS);
    }

    public String getBimIri() { return this.bimIri;}

    /**
     * Append geometry IRIs to the private queue object.
     *
     * @param geomIRI The element's geometry IRI.
     */
    public void appendGeometry(String geomIRI) {
        // Create the geom instance IRI
        geomIRI = StatementHandler.createGeometryInstanceFromIRI(geomIRI);
        // Add the IRI to the queue
        this.geomIris.offer(geomIRI);
    }

    /**
     * Generate ModelRepresentation3D statements required.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void addModelRepresentation3DStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_GEOM_MODEL_REP_CLASS);
        StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_SUBCONTEXT, this.subContext);
        // While the queue is not empty, generate statements from the values
        while (!this.geomIris.isEmpty()){
            String geomIri = this.geomIris.poll();
            StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_REP_ITEM, geomIri);
        }
        StatementHandler.addOptionalStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_REP_TYPE, this.repType, false);
        StatementHandler.addOptionalStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_SOURCE_PLACEMENT, this.sourcePlacementIri);
        StatementHandler.addOptionalStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_TARGET_PLACEMENT, this.targetPlacementIri);
    }
}
