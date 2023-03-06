package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Door;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcAbstractRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcBuildingRepresentation;
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
     * Generate zone triples
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public static void genZoneTriples(Model owlModel, LinkedHashSet<Statement> statementSet) {
        zoneMappings = SpatialZoneStorage.Singleton();
        execDoorQuery(owlModel, statementSet);
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcDoor.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createDoorSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.ZONE_VAR)
                .addVar(CommonQuery.PARENT_ZONE_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCDOOR)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.REL_SPATIAL_ZONE_ELEMENT)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_REL_ZONE, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_REL_ELEMENT, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcDoor.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private static void execDoorQuery(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createDoorSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ZONE_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String hostZone = retrieveHostZone(soln);
            Door door = new Door(iri, name, uid, placement, hostZone);
            door.constructStatements(statementSet);
        }
    }

    /**
     * Retrieve the IRI of the zone hosting this element, usually in the IfcBuildingStorey or IfcSpace.
     *
     * @param soln The row of the query response to retrieve this zone IRI.
     */
    private static String retrieveHostZone(QuerySolution soln) {
        String zoneIri = QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR);
        if (zoneIri.contains(CommonQuery.IFCSTOREY_CLASS)) {
            return zoneMappings.getStorey(zoneIri).getBotStoreyIRI();
        }
        return zoneMappings.getRoom(zoneIri).getBimRoomIRI();
    }
}
