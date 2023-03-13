package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.IfcModelRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the Stair concept in OntoBuildingStructure.
 *
 * @author qhouyee
 */
public class Stair extends IfcModelRepresentation {
    private final String elementIRI;
    private final String hostZoneIRI;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param iri          The instance IRI to be created.
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the element's position.
     * @param hostZoneIri  The IRI of the zone containing this element .
     */
    public Stair(String iri, String name, String uid, String placementIri, String hostZoneIri) {
        super(iri, name, uid, placementIri);
        this.elementIRI = this.getPrefix() + OntoBimConstant.STAIR_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.hostZoneIRI = hostZoneIri;
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
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_STAIR_CLASS);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BIM_HAS_IFC_REPRESENTATION, this.getIfcRepIri());
    }
}
