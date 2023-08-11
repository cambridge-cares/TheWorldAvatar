package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing any non-building structure elements linked to OntoBIM.
 *
 * @author qhouyee
 */
public class Element extends IfcModelRepresentation {
    private final String elementIRI;
    private final String hostZoneIRI;
    private final String geomRepIRI;
    private final String classIRI;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the element's position.
     * @param hostZoneIri  The IRI of the zone containing this element .
     * @param geomRepIri   The IRI of the element's geometry representation.
     */
    public Element(String name, String uid, String placementIri, String hostZoneIri, String geomRepIri) {
        super(name, uid, placementIri);
        this.classIRI = NamespaceMapper.retrieveClass(name);
        String className = this.classIRI.contains(OntoBimConstant.HASH) ? StringUtils.getStringAfterLastCharacterOccurrence(this.classIRI, OntoBimConstant.HASH) :
                StringUtils.getStringAfterLastCharacterOccurrence(this.classIRI, OntoBimConstant.BACKSLASH);
        this.elementIRI = this.getPrefix() + className + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.hostZoneIRI = hostZoneIri;
        this.geomRepIRI = geomRepIri;
    }

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    @Override
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        super.addIfcModelRepresentationStatements(statementSet);
        StatementHandler.addStatement(statementSet, this.hostZoneIRI, OntoBimConstant.BOT_CONTAINS_ELEMENT, this.elementIRI);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.RDF_TYPE, this.classIRI);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BIM_HAS_IFC_REPRESENTATION, this.getIfcRepIri());
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.BIM_HAS_GEOM_REP, this.geomRepIRI);
    }
}
