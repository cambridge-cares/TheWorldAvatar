package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.IfcModelRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the StairFlight concept as part of stairs in OntoBuildingStructure.
 *
 * @author qhouyee
 */
public class StairFlight extends IfcModelRepresentation {
    private final String elementIRI;
    private final String stairIRI;
    private final String geomRepIRI;
    private final Integer noOfRiser;
    private final Integer noOfTread;
    private final Double riserHeight;
    private final Double treadLength;


    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the element's position.
     * @param stairIri     The IRI of the stair assembly containing this element .
     * @param geomRepIri   The IRI of the element's geometry representation.
     * @param riserNum     The number of riser steps.
     * @param treadNum     The number of treads on the flight.
     * @param riserHeight  The height of each riser.
     * @param treadLength  The length of each tread.
     */
    public StairFlight(String name, String uid, String placementIri, String stairIri, String geomRepIri,
                       String riserNum, String treadNum, String riserHeight, String treadLength) {
        super(name, uid, placementIri);
        this.elementIRI = this.getPrefix() + OntoBimConstant.STAIR_FLIGHT_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.stairIRI = stairIri;
        this.geomRepIRI = geomRepIri;
        this.noOfRiser = Integer.valueOf(riserNum);
        this.noOfTread = Integer.valueOf(treadNum);
        this.riserHeight = Double.valueOf(riserHeight);
        this.treadLength = Double.valueOf(treadLength);
    }

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    @Override
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        super.addIfcModelRepresentationStatements(statementSet);
        StatementHandler.addStatement(statementSet, this.stairIRI, OntoBimConstant.BUILDING_STRUCTURE_CONSISTS_OF, this.elementIRI);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_STAIR_FLIGHT_CLASS);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BUILDING_STRUCTURE_HAS_RISER_NUM, this.noOfRiser);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BUILDING_STRUCTURE_HAS_TREAD_NUM, this.noOfTread);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BUILDING_STRUCTURE_HAS_RISER_HEIGHT, this.riserHeight);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BUILDING_STRUCTURE_HAS_TREAD_LENGTH, this.treadLength);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BIM_HAS_IFC_REPRESENTATION, this.getIfcRepIri());
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.BIM_HAS_GEOM_REP, this.geomRepIRI);
    }
}
