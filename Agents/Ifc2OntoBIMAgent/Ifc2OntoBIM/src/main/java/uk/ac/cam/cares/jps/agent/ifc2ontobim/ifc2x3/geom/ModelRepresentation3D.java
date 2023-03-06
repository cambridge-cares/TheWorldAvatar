package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the ModelRepresentation3D concept in OntoBIM.
 *
 * @author qhouyee
 */
public class ModelRepresentation3D {
    private final String bimIri;
    private final String prefix;
    private final String repType;
    private final String subContext;
    private final String geomIri;
    private final String sourcePlacementIri;
    private final String targetPlacementIri;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param iri                     The element IRI from IfcOwl.
     * @param subContextIri           The sub context IRI for this object.
     * @param geomIri                 The element's geometry representation IRI.
     * @param repType                 An optional field for the geometry representation type as text.
     * @param sourcePlacementIri      An optional field for the local placement IRI of the original geometry position.
     * @param cartesianTransformerIri An optional field for the transformation operator's IRI to translate the source to target placement.
     */
    public ModelRepresentation3D(String iri, String subContextIri, String geomIri, String repType, String sourcePlacementIri, String cartesianTransformerIri) {
        this.prefix = iri.contains(OntoBimConstant.HASH) ? StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.HASH) + OntoBimConstant.HASH :
                StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.BACKSLASH) + OntoBimConstant.BACKSLASH;
        this.bimIri = this.prefix + OntoBimConstant.GEOM_MODEL_REP_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.subContext = subContextIri;
        this.geomIri = geomIri;
        // Optional fields: if the argument is null, the field will still be null
        this.repType = repType;
        this.sourcePlacementIri = sourcePlacementIri;
        this.targetPlacementIri = cartesianTransformerIri;
    }

    public String getBimIri() { return this.bimIri;}

    /**
     * Generate ModelRepresentation3D statements required.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     * @param geomType     The geometry class of the element's geometry representation.
     */
    public void addModelRepresentation3DStatements(LinkedHashSet<Statement> statementSet, String geomType) {
        StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_GEOM_MODEL_REP_CLASS);
        StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_SUBCONTEXT, this.subContext);
        StatementHandler.addStatement(statementSet, this.subContext, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_GEOM_SUB_CONTEXT_CLASS);
        StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_REP_ITEM, this.geomIri);
        StatementHandler.addStatement(statementSet, this.geomIri, OntoBimConstant.RDF_TYPE, geomType);
        if (this.repType != null) {
            StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_REP_TYPE, this.repType, false);
        }
        if (this.sourcePlacementIri != null) {
            StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_SOURCE_PLACEMENT, this.sourcePlacementIri);
            StatementHandler.addStatement(statementSet, this.sourcePlacementIri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_LOCAL_PLACEMENT_CLASS);
        }
        if (this.targetPlacementIri != null) {
            StatementHandler.addStatement(statementSet, this.getBimIri(), OntoBimConstant.BIM_HAS_TARGET_PLACEMENT, this.targetPlacementIri);
            StatementHandler.addStatement(statementSet, this.targetPlacementIri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_CART_TRANS_OPERATOR_CLASS);
        }
    }
}
