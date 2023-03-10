package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the GeometricVoid concept in OntoBIM.
 *
 * @author qhouyee
 */
public class GeometricVoid {
    private final String bimIRI;
    private final String elementIRI;
    private final String voidModelRepIRI;
    private final String voidType;
    private final String placementIRI;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param elementIri      The element IRI containing this void geometry.
     * @param voidType        The type of the geometric void as a label.
     * @param placementIri    The position IRI of the void.
     * @param voidModelRepIri The model representation IRI of this void.
     */
    public GeometricVoid(String elementIri, String voidModelRepIri, String voidType, String placementIri) {
        String prefix = elementIri.contains(OntoBimConstant.HASH) ? StringUtils.getStringBeforeLastCharacterOccurrence(elementIri, OntoBimConstant.HASH) + OntoBimConstant.HASH :
                StringUtils.getStringBeforeLastCharacterOccurrence(elementIri, OntoBimConstant.BACKSLASH) + OntoBimConstant.BACKSLASH;
        this.bimIRI = prefix + OntoBimConstant.GEOM_VOID_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.elementIRI = elementIri;
        this.voidModelRepIRI = voidModelRepIri;
        this.voidType = voidType;
        this.placementIRI = placementIri;
    }

    /**
     * Generate Geometric Void statements required.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void addGeometricVoidStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BIM_HAS_VOID, this.bimIRI);
        StatementHandler.addStatement(statementSet, this.bimIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_GEOM_VOID_CLASS);
        StatementHandler.addStatement(statementSet, this.bimIRI, OntoBimConstant.BIM_HAS_GEOM_REP, this.voidModelRepIRI);
        StatementHandler.addStatement(statementSet, this.bimIRI, OntoBimConstant.BIM_HAS_VOID_TYPE, this.voidType, false);
        StatementHandler.addStatement(statementSet, this.bimIRI, OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.placementIRI);
        StatementHandler.addStatement(statementSet, this.placementIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_LOCAL_PLACEMENT_CLASS);
    }
}
