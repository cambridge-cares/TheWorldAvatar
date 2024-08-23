package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the LocalPlacement concept in OntoBIM.
 *
 * @author qhouyee
 */
public class LocalPlacement {
    private final String iri;
    private final String refPointIRI;
    private final String refDirIRI;
    private final String axisDirIRI;
    private final String relPlacementIRI;

    /**
     * Alternate Constructor for geometries when there is no existing IRI.
     *
     * @param refPointIri      The reference point's cartesian point IRI.
     * @param refDirectionIri  An optional field for this element's reference direction IRI.
     * @param axisDirectionIri An optional field for this element's axis direction IRI.
     */
    public LocalPlacement(String refPointIri, String refDirectionIri, String axisDirectionIri) {
        this.iri = NamespaceMapper.getBaseNameSpace() + OntoBimConstant.LOCAL_PLACEMENT_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.refPointIRI = refPointIri;
        this.refDirIRI = refDirectionIri;
        this.axisDirIRI = axisDirectionIri;
        this.relPlacementIRI = null;
    }

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri              The instance IRI in IfcOwl.
     * @param refPointIri      The reference point's cartesian point IRI.
     * @param refDirectionIri  An optional field for this element's reference direction IRI.
     * @param axisDirectionIri An optional field for this element's axis direction IRI.
     * @param relPlacementIri  An optional field for this element's relative position to the parent zone/element. IfcSite usually does not have this field.
     */
    public LocalPlacement(String iri, String refPointIri, String refDirectionIri, String axisDirectionIri, String relPlacementIri) {
        this.iri = StatementHandler.createInstanceFromIRI(iri, OntoBimConstant.LOCAL_PLACEMENT_CLASS);
        this.refPointIRI = refPointIri;
        this.refDirIRI = refDirectionIri;
        this.axisDirIRI = axisDirectionIri;
        this.relPlacementIRI = StatementHandler.createInstanceFromOptionalIRI(relPlacementIri, OntoBimConstant.LOCAL_PLACEMENT_CLASS);
    }

    public String getIri() {
        return this.iri;
    }

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_LOCAL_PLACEMENT_CLASS);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_REF_POINT, this.refPointIRI);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_REF_DIRECTION, this.refDirIRI);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_AXIS_DIRECTION, this.axisDirIRI);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_REL_POSITION, this.relPlacementIRI);
    }
}
