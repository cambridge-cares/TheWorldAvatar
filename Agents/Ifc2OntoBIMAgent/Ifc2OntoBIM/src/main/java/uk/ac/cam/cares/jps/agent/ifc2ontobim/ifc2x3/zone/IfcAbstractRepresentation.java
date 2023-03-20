package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the IfcAbstractRepresentation concept in OntoBIM.
 *
 * @author qhouyee
 */
public class IfcAbstractRepresentation {
    private final String iri;
    private final String prefix;
    private final String name;
    private final String uid;
    private final String placementIri;
    protected static final String METRE_UNIT = "m";

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param iri          The instance IRI to be created.
     * @param className    The name of the OntoBIM class.
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the zone's position.
     */
    public IfcAbstractRepresentation(String iri, String className, String name, String uid, String placementIri) {
        this.prefix = iri.contains(OntoBimConstant.HASH) ? StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.HASH) + OntoBimConstant.HASH :
                StringUtils.getStringBeforeLastCharacterOccurrence(iri, OntoBimConstant.BACKSLASH) + OntoBimConstant.BACKSLASH;
        this.iri = this.prefix + className + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.name = name;
        this.uid = uid;
        String instVal = StringUtils.getStringAfterLastCharacterOccurrence(placementIri, StringUtils.UNDERSCORE);
        this.placementIri = prefix + OntoBimConstant.LOCAL_PLACEMENT_CLASS + OntoBimConstant.UNDERSCORE + instVal;
    }

    protected String getIri() { return this.iri;}

    protected String getPrefix() { return this.prefix;}

    protected String getName() { return this.name;}

    protected String getUid() { return this.uid;}

    protected String getPlacementIri() { return this.placementIri;}

    /**
     * An abstract method that must be overridden and used in each subclass
     * to generate and add statements to the existing set.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
    }

    /**
     * Generate IfcAbstractRepresentation statements required.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     * @param classIRI     The full IRI of the OntoBim class concept.
     */
    public void addIfcAbstractRepresentationStatements(LinkedHashSet<Statement> statementSet, String classIRI) {
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, classIRI);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDFS_LABEL, this.getName(), false);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_ID, this.getUid(), false);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.getPlacementIri());
        StatementHandler.addStatement(statementSet, this.getPlacementIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_LOCAL_PLACEMENT_CLASS);
    }
}
