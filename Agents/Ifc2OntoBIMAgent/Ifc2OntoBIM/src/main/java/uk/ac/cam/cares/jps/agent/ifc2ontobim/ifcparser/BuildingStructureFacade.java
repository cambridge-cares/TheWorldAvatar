package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Door;
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
        zoneMappings = new SpatialZoneStorage();
        execDoorQuery(owlModel, statementSet);
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcDoor.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createDoorSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.ZONE_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCDOOR);
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
            Door door = new Door(iri, name, uid, placement);
            door.constructStatements(statementSet);
        }
    }
}
