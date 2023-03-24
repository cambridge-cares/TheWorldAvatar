package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the IfcRoomRepresentation concept in OntoBIM.
 *
 * @author qhouyee
 */
public class IfcRoomRepresentation extends IfcAbstractRepresentation {
    private final String storeyIRI;
    private final String bimRoomIRI;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri       The instance IRI to be created.
     * @param name      The name of this IFC object.
     * @param uid       The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the zone's position.
     * @param storeyIri The IRI of bot:Storey that is linked to this building instance.
     */
    public IfcRoomRepresentation(String iri, String name, String uid, String placementIri, String storeyIri) {
        // Initialise the super class
        super(iri, OntoBimConstant.ROOM_REP_CLASS,  name, uid, placementIri);
        this.storeyIRI = storeyIri;
        // Generate a new bim Room IRI
        this.bimRoomIRI = this.getPrefix() + OntoBimConstant.ROOM_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
    }

    public String getBimRoomIRI() {return bimRoomIRI;}

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    @Override
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        super.addIfcAbstractRepresentationStatements(statementSet, OntoBimConstant.BIM_ROOM_REP_CLASS);
        StatementHandler.addStatement(statementSet, this.getBimRoomIRI(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_ROOM_CLASS);
        StatementHandler.addStatement(statementSet, this.getBimRoomIRI(), OntoBimConstant.BIM_HAS_IFC_REPRESENTATION, this.getIri());
        StatementHandler.addStatement(statementSet, this.storeyIRI, OntoBimConstant.BIM_HAS_ROOM, this.getBimRoomIRI());
    }
}