package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcBuildingRepresentation;
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
    private static final String ZONE_VAR = "?zone";
    private static final String ELEVATION_VAR = "?elev";
    private static final String TER_ELEVATION_VAR = "?terElev";
    private static final String EXPRESS_HASDOUBLE = "/express:hasDouble";


    /**
     * Generate zone triples
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public static void genZoneTriples(Model owlModel, LinkedHashSet<Statement> statementSet) {
        execSiteQuery(owlModel, statementSet);
        execBuildingQuery(owlModel, statementSet);
        execStoreyQuery(owlModel, statementSet);
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
        selectBuilder.addWhere(ZONE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcSite")
                .addOptional(ZONE_VAR, "ifc:refElevation_IfcSite" + EXPRESS_HASDOUBLE, ELEVATION_VAR);
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
                .addVar(ELEVATION_VAR)
                .addVar(TER_ELEVATION_VAR);
        selectBuilder.addWhere(ZONE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcBuilding")
                .addOptional(ZONE_VAR, "ifc:elevationOfRefHeight_IfcBuilding" + EXPRESS_HASDOUBLE, ELEVATION_VAR)
                .addOptional(ZONE_VAR, "ifc:elevationOfTerrain_IfcBuilding" + EXPRESS_HASDOUBLE, TER_ELEVATION_VAR);
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
            IfcBuildingRepresentation building = new IfcBuildingRepresentation(iri, elev, terElev);
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
                .addVar(ELEVATION_VAR);
        selectBuilder.addWhere(ZONE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcBuildingStorey")
                .addOptional(ZONE_VAR, "ifc:elevation_IfcBuildingStorey" + EXPRESS_HASDOUBLE, ELEVATION_VAR);
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
            IfcStoreyRepresentation storey = new IfcStoreyRepresentation(iri, elev);
            storey.constructStatements(statementSet);
        }
    }
}
