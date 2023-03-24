package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.GeometricRepresentationContext;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.IfcProjectRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.SpatialZoneStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;

import java.util.ArrayDeque;
import java.util.LinkedHashSet;
import java.util.Queue;


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
        zoneMappings = SpatialZoneStorage.Singleton();
        execProjectQuery(owlModel, statementSet);
        execSiteQuery(owlModel, statementSet);
        execBuildingQuery(owlModel, statementSet);
        execStoreyQuery(owlModel, statementSet);
        execRoomQuery(owlModel, statementSet);
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcProject.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createProjectSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.PROJECT_VAR)
                .addVar(CommonQuery.NAME_VAR)
                .addVar(CommonQuery.PHASE_VAR)
                .addVar(CommonQuery.REP_CONTEXT_VAR)
                .addVar(CommonQuery.SPACE_DIMENSION_VAR)
                .addVar(CommonQuery.MODEL_PRECISION_VAR)
                .addVar(CommonQuery.MODEL_PLACEMENT_VAR)
                .addVar(CommonQuery.NORTH_DIR_VAR);
        selectBuilder.addWhere(CommonQuery.PROJECT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCPROJECT)
                .addOptional(CommonQuery.PROJECT_VAR, CommonQuery.IFC_PROJECT_NAME + CommonQuery.EXPRESS_HASSTRING, CommonQuery.NAME_VAR)
                .addOptional(CommonQuery.PROJECT_VAR, CommonQuery.IFC_PROJECT_PHASE + CommonQuery.EXPRESS_HASSTRING, CommonQuery.PHASE_VAR)
                // Representation context
                .addWhere(CommonQuery.PROJECT_VAR, CommonQuery.IFC_PROJECT_REP_CONTEXT, CommonQuery.REP_CONTEXT_VAR)
                .addWhere(CommonQuery.REP_CONTEXT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCGEOM_REP_CONTEXT)
                .addWhere(CommonQuery.REP_CONTEXT_VAR, CommonQuery.IFC_PROJECT_COORD_DIM + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.SPACE_DIMENSION_VAR)
                .addOptional(CommonQuery.REP_CONTEXT_VAR, CommonQuery.IFC_PROJECT_CONTEXT_PRECISION + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.MODEL_PRECISION_VAR)
                .addWhere(CommonQuery.REP_CONTEXT_VAR, CommonQuery.IFC_PROJECT_WCS_CONTEXT, CommonQuery.MODEL_PLACEMENT_VAR)
                .addOptional(CommonQuery.REP_CONTEXT_VAR, CommonQuery.IFC_PROJECT_TRUE_NORTH, CommonQuery.NORTH_DIR_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcProject.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private static void execProjectQuery(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String projectQuery = createProjectSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(projectQuery, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.PROJECT_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String phase = QueryHandler.retrieveLiteral(soln, CommonQuery.PHASE_VAR);
            String contextIri = QueryHandler.retrieveIri(soln, CommonQuery.REP_CONTEXT_VAR);
            String dimension = QueryHandler.retrieveLiteral(soln, CommonQuery.SPACE_DIMENSION_VAR);
            String precision = QueryHandler.retrieveLiteral(soln, CommonQuery.MODEL_PRECISION_VAR);
            String wcsIri = QueryHandler.retrieveIri(soln, CommonQuery.MODEL_PLACEMENT_VAR);
            String northDirIri = QueryHandler.retrieveIri(soln, CommonQuery.NORTH_DIR_VAR);
            GeometricRepresentationContext context = new GeometricRepresentationContext(contextIri, dimension, precision, wcsIri, northDirIri);
            IfcProjectRepresentation project = new IfcProjectRepresentation(iri, name, phase, context);
            zoneMappings.add(iri, project);
            project.constructStatements(statementSet);
            context.constructStatements(statementSet);
        }
    }

    /**
     * Add the optional triples for linking the IfcProject to the root spatial zone (Site or Building).
     * As IfcProject is only linked to one such zone, it may not exist and may return no value.
     *
     * @param builder The SELECT builder to append these optional statements.
     */
    private static void addProjectRootZoneRelation(SelectBuilder builder) {
        // Set up the empty builders and add the necessary prefixes
        SelectBuilder optionalBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(optionalBuilder);
        // Add query statements
        optionalBuilder.addWhere(CommonQuery.CONTEXT_REL_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.CONTEXT_REL_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.PROJECT_VAR)
                .addWhere(CommonQuery.PROJECT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCPROJECT)
                .addWhere(CommonQuery.CONTEXT_REL_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ZONE_VAR);
        builder.addOptional(optionalBuilder);
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcSite.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createSiteSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.PROJECT_VAR)
                .addVar(CommonQuery.LAT_DEGREE_VAR).addVar(CommonQuery.LAT_MIN_VAR)
                .addVar(CommonQuery.LAT_SEC_VAR).addVar(CommonQuery.LAT_MIL_SEC_VAR)
                .addVar(CommonQuery.LONG_DEGREE_VAR).addVar(CommonQuery.LONG_MIN_VAR)
                .addVar(CommonQuery.LONG_SEC_VAR).addVar(CommonQuery.LONG_MIL_SEC_VAR)
                .addVar(CommonQuery.ELEVATION_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSITE)
                // Latitude
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_REF_LAT, CommonQuery.LAT_VAR)
                .addOptional(CommonQuery.LAT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCCOMPOUND_PLANE_ANGLE)
                .addOptional(CommonQuery.LAT_VAR, CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LAT_DEGREE_VAR)
                .addOptional(CommonQuery.LAT_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LAT_MIN_VAR)
                .addOptional(CommonQuery.LAT_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LAT_SEC_VAR)
                .addOptional(CommonQuery.LAT_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LAT_MIL_SEC_VAR)
                // Longitude
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_REF_LONG, CommonQuery.LONG_VAR)
                .addOptional(CommonQuery.LONG_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCCOMPOUND_PLANE_ANGLE)
                .addOptional(CommonQuery.LONG_VAR, CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LONG_DEGREE_VAR)
                .addOptional(CommonQuery.LONG_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LONG_MIN_VAR)
                .addOptional(CommonQuery.LONG_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LONG_SEC_VAR)
                .addOptional(CommonQuery.LONG_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASINTEGER, CommonQuery.LONG_MIL_SEC_VAR)
                // Elevation
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_SITE_ELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.ELEVATION_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ZONE_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        addProjectRootZoneRelation(selectBuilder);
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
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            Queue<String> latitude;
            // Add latitude values as a queue
            if (soln.contains(CommonQuery.LAT_DEGREE_VAR) && soln.contains(CommonQuery.LAT_MIN_VAR) &&
                    soln.contains(CommonQuery.LAT_SEC_VAR) && soln.contains(CommonQuery.LAT_MIL_SEC_VAR)) {
                latitude = new ArrayDeque<>();
                latitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LAT_DEGREE_VAR));
                latitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LAT_MIN_VAR));
                latitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LAT_SEC_VAR));
                latitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LAT_MIL_SEC_VAR));
            } else {
               latitude = null;
            }
            Queue<String> longitude;
            // Add longitude values as a queue
            if (soln.contains(CommonQuery.LONG_DEGREE_VAR) && soln.contains(CommonQuery.LONG_MIN_VAR) &&
                    soln.contains(CommonQuery.LONG_SEC_VAR) && soln.contains(CommonQuery.LONG_MIL_SEC_VAR)) {
                longitude = new ArrayDeque<>();
                longitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LONG_DEGREE_VAR));
                longitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LONG_MIN_VAR));
                longitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LONG_SEC_VAR));
                longitude.offer(QueryHandler.retrieveLiteral(soln, CommonQuery.LONG_MIL_SEC_VAR));
            } else {
                longitude = null;
            }
            String elev = QueryHandler.retrieveLiteral(soln, CommonQuery.ELEVATION_VAR);
            String project = soln.contains(CommonQuery.PROJECT_VAR) ?
                    zoneMappings.getProject(QueryHandler.retrieveIri(soln, CommonQuery.PROJECT_VAR)).getIri() : null;
            IfcSiteRepresentation site = new IfcSiteRepresentation(iri, name, uid, placement, project, latitude, longitude, elev);
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
        selectBuilder.addVar(CommonQuery.PROJECT_VAR)
                .addVar(CommonQuery.PARENT_ZONE_VAR)
                .addVar(CommonQuery.ELEVATION_VAR)
                .addVar(CommonQuery.TER_ELEVATION_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCBUILDING)
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_BUILDING_ELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.ELEVATION_VAR)
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_BUILDING_TERELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.TER_ELEVATION_VAR)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSITE)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ZONE_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        addProjectRootZoneRelation(selectBuilder);
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
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String elev = QueryHandler.retrieveLiteral(soln, CommonQuery.ELEVATION_VAR);
            String terElev = QueryHandler.retrieveLiteral(soln, CommonQuery.TER_ELEVATION_VAR);
            String project = soln.contains(CommonQuery.PROJECT_VAR) ?
                    zoneMappings.getProject(QueryHandler.retrieveIri(soln, CommonQuery.PROJECT_VAR)).getIri() : null;
            IfcSiteRepresentation site = zoneMappings.getSite(QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR));
            IfcBuildingRepresentation building = new IfcBuildingRepresentation(iri, name, uid, placement, project, site.getBotSiteIRI(), elev, terElev);
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
        selectBuilder.addVar(CommonQuery.PARENT_ZONE_VAR)
                .addVar(CommonQuery.ELEVATION_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSTOREY)
                .addOptional(CommonQuery.ZONE_VAR, CommonQuery.IFC_STOREY_ELEV + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.ELEVATION_VAR)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCBUILDING)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ZONE_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
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
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String elev = QueryHandler.retrieveLiteral(soln, CommonQuery.ELEVATION_VAR);
            IfcBuildingRepresentation building = zoneMappings.getBuilding(QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR));
            IfcStoreyRepresentation storey = new IfcStoreyRepresentation(iri, name, uid, placement, building.getBotBuildingIRI(), elev);
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
        selectBuilder.addVar(CommonQuery.PARENT_ZONE_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSPACE)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.PARENT_ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCSTOREY)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ZONE_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
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
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String storeyIri = QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR);
            IfcStoreyRepresentation storey = zoneMappings.getStorey(storeyIri);
            IfcRoomRepresentation room = new IfcRoomRepresentation(iri, name, uid, placement, storey.getBotStoreyIRI());
            zoneMappings.add(iri, room);
            room.constructStatements(statementSet);
        }
    }
}
