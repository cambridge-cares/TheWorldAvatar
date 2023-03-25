package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the IfcModelRepresentation concept in OntoBIM.
 *
 * @author qhouyee
 */
public class IfcModelRepresentation {
    private final String ifcRepIri;
    private final String prefix;
    private final String name;
    private final String uid;
    private final String placementIri;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the zone's position.
     */
    public IfcModelRepresentation(String name, String uid, String placementIri) {
        this.prefix = NamespaceMapper.getBaseNameSpace();
        this.ifcRepIri = this.prefix + OntoBimConstant.ASSET_MODEL_REP_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.name = name;
        this.uid = uid;
        this.placementIri = StatementHandler.createInstanceFromIRI(placementIri, OntoBimConstant.LOCAL_PLACEMENT_CLASS);
    }

    public String getIfcRepIri() { return this.ifcRepIri;}
    public String getPrefix() { return this.prefix;}
    public String getName() { return this.name;}
    public String getUid() { return this.uid;}
    public String getPlacementIri() { return this.placementIri;}

    /**
     * An abstract method that must be overridden and used in each subclass
     * to generate and add statements to the existing set.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
    }

    /**
     * Generate IfcModelRepresentation statements required.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void addIfcModelRepresentationStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_ASSET_MODEL_REP_CLASS);
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.RDFS_LABEL, this.getName(), false);
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.BIM_HAS_ID, this.getUid(), false);
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.getPlacementIri());
    }
}
