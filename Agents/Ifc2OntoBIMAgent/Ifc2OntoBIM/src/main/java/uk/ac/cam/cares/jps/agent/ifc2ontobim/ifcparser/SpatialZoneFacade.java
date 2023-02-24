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
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

import java.util.LinkedHashSet;


/**
 * Provides functions to generate the spatial zones triples.
 *
 * @author qhouyee
 */
public class SpatialZoneFacade {
    private static SpatialZoneStorage zoneMappings;
    private static final String ZONE_VAR = "?zone";
    private static final String PARENT_ZONE_VAR = "?subzone";
    private static final String RELAGGR_VAR = "?relaggregates";
    private static final String ELEVATION_VAR = "?elev";
    private static final String TER_ELEVATION_VAR = "?terElev";
    // IfcOwl Properties
    private static final String EXPRESS_HASDOUBLE = "/express:hasDouble";
    private static final String IFC_SITE_ELEV = NamespaceMapper.IFC_PREFIX + ":refElevation_IfcSite";
    private static final String IFC_BUILDING_ELEV = NamespaceMapper.IFC_PREFIX + ":elevationOfRefHeight_IfcBuilding";
    private static final String IFC_BUILDING_TERELEV = NamespaceMapper.IFC_PREFIX + ":elevationOfTerrain_IfcBuilding";
    private static final String IFC_STOREY_ELEV = NamespaceMapper.IFC_PREFIX + ":elevation_IfcBuildingStorey";
    private static final String IFC_PARENT_ZONE_REL = NamespaceMapper.IFC_PREFIX + ":relatingObject_IfcRelDecomposes";
    private static final String IFC_CHILD_ZONE_REL = NamespaceMapper.IFC_PREFIX + ":relatedObjects_IfcRelDecomposes";
    // IfcOwl Classes
    private static final String IFCSITE = NamespaceMapper.IFC_PREFIX + ":IfcSite";
    private static final String IFCBUILDING = NamespaceMapper.IFC_PREFIX + ":IfcBuilding";
    private static final String IFCSTOREY = NamespaceMapper.IFC_PREFIX + ":IfcBuildingStorey";
    private static final String IFCSPACE = NamespaceMapper.IFC_PREFIX + ":IfcSpace";
    private static final String RELAGG = NamespaceMapper.IFC_PREFIX + ":IfcRelAggregates";


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
        selectBuilder.addVar(ZONE_VAR)
                .addVar(ELEVATION_VAR);
        selectBuilder.addWhere(ZONE_VAR, QueryHandler.RDF_TYPE, IFCSITE)
                .addOptional(ZONE_VAR, IFC_SITE_ELEV + EXPRESS_HASDOUBLE, ELEVATION_VAR);
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
            String iri = soln.get(ZONE_VAR).toString();
            String elev = QueryHandler.retrieveLiteral(soln, ELEVATION_VAR);
            IfcSiteRepresentation site = new IfcSiteRepresentation(iri, elev);
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
        selectBuilder.addVar(ZONE_VAR)
                .addVar(PARENT_ZONE_VAR)
                .addVar(ELEVATION_VAR)
                .addVar(TER_ELEVATION_VAR);
        selectBuilder.addWhere(ZONE_VAR, QueryHandler.RDF_TYPE, IFCBUILDING)
                .addOptional(ZONE_VAR, IFC_BUILDING_ELEV + EXPRESS_HASDOUBLE, ELEVATION_VAR)
                .addOptional(ZONE_VAR, IFC_BUILDING_TERELEV + EXPRESS_HASDOUBLE, TER_ELEVATION_VAR)
                .addWhere(RELAGGR_VAR, QueryHandler.RDF_TYPE, RELAGG)
                .addWhere(RELAGGR_VAR, IFC_PARENT_ZONE_REL, PARENT_ZONE_VAR)
                .addWhere(PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, IFCSITE)
                .addWhere(RELAGGR_VAR, IFC_CHILD_ZONE_REL, ZONE_VAR);
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
            String iri = soln.get(ZONE_VAR).toString();
            String elev = QueryHandler.retrieveLiteral(soln, ELEVATION_VAR);
            String terElev = QueryHandler.retrieveLiteral(soln, TER_ELEVATION_VAR);
            IfcSiteRepresentation site = zoneMappings.getSite(QueryHandler.retrieveIri(soln, PARENT_ZONE_VAR));
            IfcBuildingRepresentation building = new IfcBuildingRepresentation(iri, site.getBotSiteIRI(), elev, terElev);
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
        selectBuilder.addVar(ZONE_VAR)
                .addVar(PARENT_ZONE_VAR)
                .addVar(ELEVATION_VAR);
        selectBuilder.addWhere(ZONE_VAR, QueryHandler.RDF_TYPE, IFCSTOREY)
                .addOptional(ZONE_VAR, IFC_STOREY_ELEV + EXPRESS_HASDOUBLE, ELEVATION_VAR)
                .addWhere(RELAGGR_VAR, QueryHandler.RDF_TYPE, RELAGG)
                .addWhere(RELAGGR_VAR, IFC_PARENT_ZONE_REL, PARENT_ZONE_VAR)
                .addWhere(PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, IFCBUILDING)
                .addWhere(RELAGGR_VAR, IFC_CHILD_ZONE_REL, ZONE_VAR);
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
            String iri = soln.get(ZONE_VAR).toString();
            String elev = QueryHandler.retrieveLiteral(soln, ELEVATION_VAR);
            IfcBuildingRepresentation building = zoneMappings.getBuilding(QueryHandler.retrieveIri(soln, PARENT_ZONE_VAR));
            IfcStoreyRepresentation storey = new IfcStoreyRepresentation(iri, building.getBotBuildingIRI(), elev);
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
        selectBuilder.addVar(ZONE_VAR)
                .addVar(PARENT_ZONE_VAR);
        selectBuilder.addWhere(ZONE_VAR, QueryHandler.RDF_TYPE, IFCSPACE)
                .addWhere(RELAGGR_VAR, QueryHandler.RDF_TYPE, RELAGG)
                .addWhere(RELAGGR_VAR, IFC_PARENT_ZONE_REL, PARENT_ZONE_VAR)
                .addWhere(PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, IFCSTOREY)
                .addWhere(RELAGGR_VAR, IFC_CHILD_ZONE_REL, ZONE_VAR);
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
            String iri = soln.get(ZONE_VAR).toString();
            String storeyIri = QueryHandler.retrieveIri(soln, PARENT_ZONE_VAR);
            IfcStoreyRepresentation storey = zoneMappings.getStorey(storeyIri);
            IfcRoomRepresentation room = new IfcRoomRepresentation(iri, storey.getBotStoreyIRI());
            zoneMappings.add(iri, room);
            room.constructStatements(statementSet);
        }
    }
}
