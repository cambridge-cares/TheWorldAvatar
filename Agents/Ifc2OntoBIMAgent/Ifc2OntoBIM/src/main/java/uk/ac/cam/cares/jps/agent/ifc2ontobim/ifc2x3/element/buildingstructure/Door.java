package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.IfcModelRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the Door concept in OntoBuildingStructure.
 *
 * @author qhouyee
 */
public class Door extends IfcModelRepresentation {
    private final String elementIRI;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param iri          The instance IRI to be created.
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the zone's position.
     */
    public Door(String iri, String name, String uid, String placementIri) {
        super(iri, name, uid, placementIri);
        this.elementIRI = this.getPrefix() + OntoBimConstant.DOOR_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
    }

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    @Override
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        super.addIfcModelRepresentationStatements(statementSet);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_DOOR_CLASS);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BIM_HAS_IFC_REPRESENTATION, this.getIfcRepIri());
    }
}
