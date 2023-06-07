package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.IfcModelRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the Wall concept in OntoBuildingStructure.
 *
 * @author qhouyee
 */
public class Wall extends IfcModelRepresentation {
    private final String elementIRI;
    private final String hostZoneIRI;
    private final String geomRepIRI;
    private final String secGeomRepIRI;


    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param name          The name of this IFC object.
     * @param uid           The IFC uid generated for this object.
     * @param placementIri  The local placement IRI for the element's position.
     * @param hostZoneIri   The IRI of the zone containing this element .
     * @param geomRepIri    The IRI of the element's geometry representation.
     * @param secGeomRepIri An optional field for the IRI of the element's second geometry representation.
     */
    public Wall(String name, String uid, String placementIri, String hostZoneIri, String geomRepIri, String secGeomRepIri) {
        super(name, uid, placementIri);
        this.elementIRI = this.getPrefix() + OntoBimConstant.WALL_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.hostZoneIRI = hostZoneIri;
        this.geomRepIRI = geomRepIri;
        this.secGeomRepIRI = secGeomRepIri; // If the argument is null, the field will still be null
    }

    public String getElementIri() {
        return this.elementIRI;
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
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_WALL_CLASS);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BIM_HAS_IFC_REPRESENTATION, this.getIfcRepIri());
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.BIM_HAS_GEOM_REP, this.geomRepIRI);
        StatementHandler.addOptionalStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.BIM_HAS_GEOM_REP, this.secGeomRepIRI);
    }
}
