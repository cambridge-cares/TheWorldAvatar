package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StringUtils;

import java.util.LinkedHashSet;

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
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri              The instance IRI in IfcOwl.
     * @param refPointIri      The reference point's cartesian point IRI.
     * @param refDirectionIri  An optional field for this element's reference direction IRI.
     * @param axisDirectionIri An optional field for this element's axis direction IRI.
     * @param relPlacementIri  An optional field for this element's relative position to the parent zone/element. IfcSite usually does not have this field.
     */
    public LocalPlacement(String iri, String refPointIri, String refDirectionIri, String axisDirectionIri, String relPlacementIri) {
        String prefix = iri.contains(OntoBimConstant.HASH) ? StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.HASH) + OntoBimConstant.HASH :
                StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.BACKSLASH) + OntoBimConstant.BACKSLASH;
        String instVal = StringUtils.getStringAfterLastCharacterOccurrence(iri, StringUtils.UNDERSCORE);
        this.iri = prefix + OntoBimConstant.LOCAL_PLACEMENT_CLASS + OntoBimConstant.UNDERSCORE + instVal;
        this.refPointIRI = refPointIri;
        this.refDirIRI = refDirectionIri;
        this.axisDirIRI = axisDirectionIri;
        if (relPlacementIri != null) {
            instVal = StringUtils.getStringAfterLastCharacterOccurrence(relPlacementIri, StringUtils.UNDERSCORE);
            this.relPlacementIRI = prefix + OntoBimConstant.LOCAL_PLACEMENT_CLASS + OntoBimConstant.UNDERSCORE + instVal;
        } else {
            this.relPlacementIRI = null;
        }
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
