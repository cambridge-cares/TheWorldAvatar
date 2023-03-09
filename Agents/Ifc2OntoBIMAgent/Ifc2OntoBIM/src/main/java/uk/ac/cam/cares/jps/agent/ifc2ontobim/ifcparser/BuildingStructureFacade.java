package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Ceiling;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Door;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

import java.util.LinkedHashSet;

/**
 * Provides functions to generate the triples for building structure elements.
 *
 * @author qhouyee
 */
public class BuildingStructureFacade {
    private static SpatialZoneStorage zoneMappings;

    /**
     * Standard Constructor retrieving the spatial zone singleton.
     */
    public BuildingStructureFacade(){
        zoneMappings = SpatialZoneStorage.Singleton();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcCovering.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addCeilingStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CEILING);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Query statements to restrict it to specifically ceiling
        selectBuilder.addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_TYPE_DEFINITION)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATED_OBJECT, CommonQuery.ELEMENT_VAR)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATING_TYPE, CommonQuery.ELEMENT_TYPE_VAR)
                .addWhere(CommonQuery.ELEMENT_TYPE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_COVERING_TYPE)
                .addWhere(CommonQuery.ELEMENT_TYPE_VAR, CommonQuery.IFC_PREDEFINED_TYPE_COVERING, CommonQuery.IFC_CEILING_ENUM);
        // Query from the model
        ResultSet results = QueryHandler.execSelectQuery(selectBuilder.buildString(), owlModel);
        // Process query results
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ELEMENT_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String hostZone = QueryHandler.retrieveHostZone(soln, zoneMappings);
            String geomType = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_TYPE_VAR);
            ModelRepresentation3D geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
            Ceiling ceiling = new Ceiling(iri, name, uid, placement, hostZone, geomModel.getBimIri());
            ceiling.constructStatements(statementSet);
            geomModel.addModelRepresentation3DStatements(statementSet, geomType);
        }
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcDoor.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addDoorStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCDOOR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Query from the model
        ResultSet results = QueryHandler.execSelectQuery(selectBuilder.buildString(), owlModel);
        // Process query results
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ELEMENT_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String hostZone = QueryHandler.retrieveHostZone(soln, zoneMappings);
            String geomType = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_TYPE_VAR);
            ModelRepresentation3D geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
            Door door = new Door(iri, name, uid, placement, hostZone, geomModel.getBimIri());
            door.constructStatements(statementSet);
            geomModel.addModelRepresentation3DStatements(statementSet, geomType);
        }
    }
}
