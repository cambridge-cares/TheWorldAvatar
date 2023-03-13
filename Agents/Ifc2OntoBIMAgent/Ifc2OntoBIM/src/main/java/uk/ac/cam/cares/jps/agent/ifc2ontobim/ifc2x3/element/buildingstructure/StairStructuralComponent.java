package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.IfcModelRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.StatementHandler;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the StairStructuralComponent concept as part of stairs in OntoBuildingStructure.
 *
 * @author qhouyee
 */
public class StairStructuralComponent extends IfcModelRepresentation {
    private final String elementIRI;
    private final String stairIRI;
    private final String geomRepIRI;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param iri          The instance IRI to be created.
     * @param name         The name of this IFC object.
     * @param uid          The IFC uid generated for this object.
     * @param placementIri The local placement IRI for the element's position.
     * @param stairIri     The IRI of the stair assembly containing this element .
     * @param geomRepIri   The IRI of the element's geometry representation.
     */
    public StairStructuralComponent(String iri, String name, String uid, String placementIri, String stairIri, String geomRepIri) {
        super(iri, name, uid, placementIri);
        this.elementIRI = this.getPrefix() + OntoBimConstant.STAIR_STRUCT_COMP_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        this.stairIRI = stairIri;
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
        StatementHandler.addStatement(statementSet, this.stairIRI, OntoBimConstant.BUILDING_STRUCTURE_CONSISTS_OF, this.elementIRI);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_STAIR_STRUCT_COMP_CLASS);
        StatementHandler.addStatement(statementSet, this.elementIRI, OntoBimConstant.BIM_HAS_IFC_REPRESENTATION, this.getIfcRepIri());
        StatementHandler.addStatement(statementSet, this.getIfcRepIri(), OntoBimConstant.BIM_HAS_GEOM_REP, this.geomRepIRI);
    }
}
