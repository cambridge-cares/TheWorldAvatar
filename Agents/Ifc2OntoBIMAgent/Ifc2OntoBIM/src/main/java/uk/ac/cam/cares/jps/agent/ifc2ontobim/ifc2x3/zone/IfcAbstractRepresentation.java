package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the IfcAbstractRepresentation concept in OntoBIM.
 *
 * @author qhouyee
 */
public class IfcAbstractRepresentation {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private final String iri;
    private final String prefix;
    private final String name;
    private final String uid;
    private final String placementIri;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param className    The name of the OntoBIM class.
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the zone's position.
     */
    public IfcAbstractRepresentation(String className, String name, String uid, String placementIri) {
        this.prefix = NamespaceMapper.getBaseNameSpace();
        this.iri = this.prefix + className + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.name = name;
        this.uid = uid;
        this.placementIri = StatementHandler.createInstanceFromIRI(placementIri, OntoBimConstant.LOCAL_PLACEMENT_CLASS);
    }

    protected String getIri() {
        return this.iri;
    }

    protected String getPrefix() {
        return this.prefix;
    }

    protected String getName() {
        return this.name;
    }

    protected String getUid() {
        return this.uid;
    }

    protected String getPlacementIri() {
        return this.placementIri;
    }

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
        // The name should only be instantiated if it exists
        if (this.getName().isEmpty()) LOGGER.warn("The name for " + this.getIri() + " with a IFC id of " + this.getUid() + " is missing and will not be instantiated!");
        else StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDFS_LABEL, this.getName(), false);
        // Add remaining statements
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_ID, this.getUid(), false);
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_LOCAL_POSITION, this.getPlacementIri());
    }
}
