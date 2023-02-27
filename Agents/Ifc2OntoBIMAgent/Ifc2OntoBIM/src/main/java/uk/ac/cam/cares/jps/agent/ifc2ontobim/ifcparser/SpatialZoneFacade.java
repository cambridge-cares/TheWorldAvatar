package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcBuildingRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcRoomRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcSiteRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcStoreyRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

import java.util.LinkedHashSet;


/**
 * Provides functions to generate the spatial zones triples.
 *
 * @author qhouyee
 */
public class SpatialZoneFacade {
    private static SpatialZoneStorage zoneMappings;


    /**
     * Generate zone triples
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public static void genZoneTriples(Model owlModel, LinkedHashSet<Statement> statementSet) {
        zoneMappings = new SpatialZoneStorage();
        execSiteQuery(owlModel, statementSet);
        execBuildingQuery(owlModel, statementSet);
        execStoreyQuery(owlModel, statementSet);
        execRoomQuery(owlModel, statementSet);
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcSite.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createSiteSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.ZONE_VAR)
                .addVar(CommonQuery.NAME_VAR)
                .addVar(CommonQuery.UID_VAR)
                .addVar(CommonQuery.ELEVATION_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSITE)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_NAME + CommonQuery.EXPRESS_HASSTRING, CommonQuery.NAME_VAR)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_ID + CommonQuery.EXPRESS_HASSTRING, CommonQuery.UID_VAR)
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_SITE_ELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.ELEVATION_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcSite.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private static void execSiteQuery(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String siteQuery = createSiteSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(siteQuery, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ZONE_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String elev = QueryHandler.retrieveLiteral(soln, CommonQuery.ELEVATION_VAR);
            IfcSiteRepresentation site = new IfcSiteRepresentation(iri, name, uid, elev);
            zoneMappings.add(iri, site);
            site.constructStatements(statementSet);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcBuilding.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createBuildingSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.ZONE_VAR)
                .addVar(CommonQuery.NAME_VAR)
                .addVar(CommonQuery.UID_VAR)
                .addVar(CommonQuery.PARENT_ZONE_VAR)
                .addVar(CommonQuery.ELEVATION_VAR)
                .addVar(CommonQuery.TER_ELEVATION_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCBUILDING)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_NAME + CommonQuery.EXPRESS_HASSTRING, CommonQuery.NAME_VAR)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_ID + CommonQuery.EXPRESS_HASSTRING, CommonQuery.UID_VAR)
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_BUILDING_ELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.ELEVATION_VAR)
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_BUILDING_TERELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.TER_ELEVATION_VAR)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSITE)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcBuilding.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private static void execBuildingQuery(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String buildingQuery = createBuildingSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(buildingQuery, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ZONE_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String elev = QueryHandler.retrieveLiteral(soln, CommonQuery.ELEVATION_VAR);
            String terElev = QueryHandler.retrieveLiteral(soln, CommonQuery.TER_ELEVATION_VAR);
            IfcSiteRepresentation site = zoneMappings.getSite(QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR));
            IfcBuildingRepresentation building = new IfcBuildingRepresentation(iri, name, uid, site.getBotSiteIRI(), elev, terElev);
            zoneMappings.add(iri, building);
            building.constructStatements(statementSet);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcBuildingStorey.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createStoreySelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.ZONE_VAR)
                .addVar(CommonQuery.NAME_VAR)
                .addVar(CommonQuery.UID_VAR)
                .addVar(CommonQuery.PARENT_ZONE_VAR)
                .addVar(CommonQuery.ELEVATION_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSTOREY)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_NAME + CommonQuery.EXPRESS_HASSTRING, CommonQuery.NAME_VAR)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_ID + CommonQuery.EXPRESS_HASSTRING, CommonQuery.UID_VAR)
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_STOREY_ELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.ELEVATION_VAR)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCBUILDING)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcBuildingStorey.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private static void execStoreyQuery(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String storeyQuery = createStoreySelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(storeyQuery, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ZONE_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String elev = QueryHandler.retrieveLiteral(soln, CommonQuery.ELEVATION_VAR);
            IfcBuildingRepresentation building = zoneMappings.getBuilding(QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR));
            IfcStoreyRepresentation storey = new IfcStoreyRepresentation(iri, name, uid, building.getBotBuildingIRI(), elev);
            zoneMappings.add(iri, storey);
            storey.constructStatements(statementSet);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcSpace.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createRoomSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.ZONE_VAR)
                .addVar(CommonQuery.NAME_VAR)
                .addVar(CommonQuery.UID_VAR)
                .addVar(CommonQuery.PARENT_ZONE_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSPACE)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_NAME + CommonQuery.EXPRESS_HASSTRING, CommonQuery.NAME_VAR)
                .addWhere(CommonQuery.ZONE_VAR, CommonQuery.IFC_ID + CommonQuery.EXPRESS_HASSTRING, CommonQuery.UID_VAR)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSTOREY)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcSpace.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private static void execRoomQuery(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String storeyQuery = createRoomSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(storeyQuery, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ZONE_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String storeyIri = QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR);
            IfcStoreyRepresentation storey = zoneMappings.getStorey(storeyIri);
            IfcRoomRepresentation room = new IfcRoomRepresentation(iri, name, uid, storey.getBotStoreyIRI());
            zoneMappings.add(iri, room);
            room.constructStatements(statementSet);
        }
    }
}
