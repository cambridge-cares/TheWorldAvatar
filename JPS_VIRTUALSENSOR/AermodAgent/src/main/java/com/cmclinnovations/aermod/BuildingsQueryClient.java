/*  This calss contains all SPARQL queries used by the buildings class. */

package com.cmclinnovations.aermod;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.constraint.SparqlFunction;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.json.JSONArray;
import com.cmclinnovations.aermod.sparqlbuilder.ValuesPattern;
import it.unibz.inf.ontop.model.vocabulary.GEO;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.arq.querybuilder.handlers.WhereHandler;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementService;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;

public class BuildingsQueryClient {
    private static final Logger LOGGER = LogManager.getLogger(BuildingsQueryClient.class);

    public static final String PREFIX_DISP = "http://www.theworldavatar.com/kg/dispersion/";
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final String ONTO_BUILD = "https://www.theworldavatar.com/kg/ontobuiltenv/";
    public static final String CHEM = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#";
    public static final String ONTO_CITYGML = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";

    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));
    private static final Prefix P_BUILD = SparqlBuilder.prefix("build", iri(ONTO_BUILD));
    private static final Prefix P_CHEM = SparqlBuilder.prefix("chem", iri(CHEM));
    private static final Prefix P_OCGML = SparqlBuilder.prefix("ocgml", iri(ONTO_CITYGML));

    // classes
    public static final String REPORTING_STATION = "https://www.theworldavatar.com/kg/ontoems/ReportingStation";
    public static final String NX = PREFIX_DISP + "nx";
    public static final String NY = PREFIX_DISP + "ny";
    public static final String SCOPE = PREFIX_DISP + "Scope";
    public static final String SIMULATION_TIME = PREFIX_DISP + "SimulationTime";
    public static final String NO_X = PREFIX_DISP + "NOx";
    public static final String UHC = PREFIX_DISP + "uHC";
    public static final String CO = PREFIX_DISP + "CO";
    public static final String SO2 = PREFIX_DISP + "SO2";
    public static final String PM10 = PREFIX_DISP + "PM10";
    public static final String PM25 = PREFIX_DISP + "PM2.5";
    public static final String DENSITY = OM_STRING + "Density";
    public static final String TEMPERATURE = OM_STRING + "Temperature";
    public static final String MASS_FLOW = OM_STRING + "MassFlow";
    private static final Iri CHEMICALPLANT = P_CHEM.iri("ChemicalPlant");
    private static final Iri PLANTITEM = iri(
            "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#PlantItem");
    private static final Iri BUILDING = iri("http://www.purl.org/oema/infrastructure/Building");

    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri CONTAINS = P_GEO.iri("ehContains");
    private static final Iri OCGML_REP = P_BUILD.iri("hasOntoCityGMLRepresentation");
    private static final Iri HAS_INDIVIDUALCO2Emission = P_CHEM.iri("hasIndividualCO2Emission");
    private static final Iri OCGML_GEOM = P_OCGML.iri("GeometryType");
    private static final Iri OCGML_CITYOBJECT = P_OCGML.iri("cityObjectId");
    private static final Iri OCGML_OBJECTCLASSID = P_OCGML.iri("objectClassId");
    private static final Iri OCGML_LOD2MULTISURFACEID = P_OCGML.iri("lod2MultiSurfaceId");
    private static final Iri OCGML_BUILDINGID = P_OCGML.iri("buildingId");
    private static final Iri OCGML_ENVELOPETYPE = P_OCGML.iri("EnvelopeType");

    /* SPARQL queries for buildings class */

    public static JSONArray StackQuery(String StackQueryIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_GEO, P_BUILD, P_OM, P_CHEM);
        Variable IRI = SparqlBuilder.var("IRI");
        Variable emission = SparqlBuilder.var("emission");
        Variable chemPlant = SparqlBuilder.var("chemPlant");
        Variable plantItem = SparqlBuilder.var("plantItem");
        Variable CO2 = SparqlBuilder.var("CO2");

        GraphPattern gp = GraphPatterns.and(chemPlant.has(RDF.TYPE, CHEMICALPLANT).andHas(CONTAINS, plantItem),
                plantItem.has(RDF.TYPE, PLANTITEM).andHas(OCGML_REP, IRI).andHas(HAS_INDIVIDUALCO2Emission, CO2),
                CO2.has(HAS_NUMERICALVALUE, emission));
        query.select(IRI, emission).where(gp).limit(Integer.parseInt(EnvConfig.NUMBER_SOURCES));

        JSONArray StackIRIQueryResult = AccessAgentCaller.queryStore(StackQueryIRI, query.getQueryString());
        return StackIRIQueryResult;
    }

    public static JSONArray BuildingQuery(String StackQueryIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_GEO, P_BUILD, P_OM, P_CHEM);
        Variable IRI = SparqlBuilder.var("IRI");
        Variable chemPlant = SparqlBuilder.var("chemPlant");
        Variable building = SparqlBuilder.var("building");

        GraphPattern gp = GraphPatterns.and(chemPlant.has(RDF.TYPE, CHEMICALPLANT).andHas(CONTAINS, building),
                building.has(RDF.TYPE, BUILDING).andHas(OCGML_REP, IRI));
        query.select(IRI).where(gp).limit(Integer.parseInt(EnvConfig.NUMBER_BUILDINGS));

        JSONArray BuildingIRIQueryResult = AccessAgentCaller.queryStore(StackQueryIRI, query.getQueryString());
        return BuildingIRIQueryResult;
    }

    public static JSONArray StackGeometricQuery(String GeospatialQueryIRI, List<String> ObjectIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable geometricIRI = SparqlBuilder.var("geometricIRI");
        Variable polygonData = SparqlBuilder.var("polygonData");
        Variable objectIRI = SparqlBuilder.var("objectIRI");

        GraphPattern gp = GraphPatterns
                .and(geometricIRI.has(OCGML_GEOM, polygonData).andHas(OCGML_CITYOBJECT, objectIRI));
        ValuesPattern<Iri> vp = new ValuesPattern<>(objectIRI,
                ObjectIRI.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        query.select(polygonData, objectIRI).where(gp, vp).orderBy(objectIRI);
        JSONArray GeometricQueryResult = AccessAgentCaller.queryStore(GeospatialQueryIRI, query.getQueryString());

        return GeometricQueryResult;
    }

    public static JSONArray BuildingGeometricQuery(String GeospatialQueryIRI, List<String> ObjectIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable geometricIRI = SparqlBuilder.var("geometricIRI");
        Variable polygonData = SparqlBuilder.var("polygonData");
        Variable objectIRI = SparqlBuilder.var("objectIRI");
        Variable surfaceIRI = SparqlBuilder.var("surfaceIRI");

        GraphPattern gp = GraphPatterns.and(surfaceIRI.has(OCGML_GEOM, polygonData),
                geometricIRI.has(OCGML_LOD2MULTISURFACEID, surfaceIRI).andHas(OCGML_BUILDINGID, objectIRI));
        ValuesPattern<Iri> vp = new ValuesPattern<>(objectIRI,
                ObjectIRI.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        query.select(polygonData, objectIRI).where(gp, vp).orderBy(objectIRI);
        JSONArray GeometricQueryResult = AccessAgentCaller.queryStore(GeospatialQueryIRI, query.getQueryString());

        return GeometricQueryResult;
    }

    public static JSONArray EnvelopeQuery(String GeoSpatialQueryIRI, List<String> targetIRI) {
        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable objectIRI = SparqlBuilder.var("objectIRI");
        Variable envelopeData = SparqlBuilder.var("envelopeData");

        GraphPattern gp = GraphPatterns.and(objectIRI.has(OCGML_ENVELOPETYPE, envelopeData));
        ValuesPattern<Iri> vp = new ValuesPattern<>(objectIRI,
                targetIRI.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        query.select(envelopeData, objectIRI).where(gp, vp);
        JSONArray EnvelopeQueryResult = AccessAgentCaller.queryStore(GeoSpatialQueryIRI, query.getQueryString());
        return EnvelopeQueryResult;
    }

    /**
     * builds a SPARQL geospatial query for city object id of buildings whose
     * envelope are within lowerBounds and upperBounds
     * 
     * @param uriString   city object id of the target building
     * @param lowerBounds coordinates of customFieldsLowerBounds as a string
     * @param upperBounds coordinates of customFieldsUpperBounds as a string
     * @return returns a query string
     * @throws org.apache.jena.sparql.lang.sparql_11.ParseException
     */
    public static JSONArray getBuildingsWithinBounds(String GeoSpatialQueryIRI, String lowerBounds, String upperBounds)
            throws org.apache.jena.sparql.lang.sparql_11.ParseException {

        String geoUri = "http://www.bigdata.com/rdf/geospatial#";
        // where clause for geospatial search
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ocgml", ONTO_CITYGML)
                .addPrefix("geo", geoUri)
                .addWhere("?cityObject", "geo:predicate", "ocgml:EnvelopeType")
                .addWhere("?cityObject", "geo:searchDatatype", "<http://localhost/blazegraph/literals/POLYGON-3-15>")
                .addWhere("?cityObject", "geo:customFields", "X0#Y0#Z0#X1#Y1#Z1#X2#Y2#Z2#X3#Y3#Z3#X4#Y4#Z4")
                // PLACEHOLDER because lowerBounds and upperBounds would be otherwise added as
                // doubles, not strings
                .addWhere("?cityObject", "geo:customFieldsLowerBounds", "PLACEHOLDER" + lowerBounds)
                .addWhere("?cityObject", "geo:customFieldsUpperBounds", "PLACEHOLDER" + upperBounds);

        // where clause to check that the city object is a building
        WhereBuilder wb2 = new WhereBuilder()
                .addPrefix("ocgml", ONTO_CITYGML)
                .addWhere("?cityObject", "ocgml:objectClassId", "?id")
                .addFilter("?id=26");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?cityObject");

        Query query = sb.build();
        // add geospatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(geoUri + "search", wb.build().getQueryPattern()));
        body.addElement(wb2.build().getQueryPattern());
        query.setQueryPattern(body);

        WhereHandler wh = new WhereHandler(query.cloneQuery());

        // add city object graph
        String cityObjectGraph = "http://www.theworldavatar.com:83/citieskg/namespace/" + GeoSpatialQueryIRI
                + "/sparql/cityobject/";
        WhereHandler wh2 = new WhereHandler(sb.build());
        wh2.addGraph(NodeFactory.createURI(cityObjectGraph), wh);

        String queryString = query.toString().replace("PLACEHOLDER", "");
        JSONArray BuildingIRIQueryResult = AccessAgentCaller.queryStore(GeoSpatialQueryIRI, queryString);
        return BuildingIRIQueryResult;
    }

    public static JSONArray BuildingGeometricQuery2(String GeospatialQueryIRI, List<String> ObjectIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable geometricIRI = SparqlBuilder.var("geometricIRI");
        Variable polygonData = SparqlBuilder.var("polygonData");
        Variable objectIRI = SparqlBuilder.var("objectIRI");
        Variable surfaceIRI = SparqlBuilder.var("surfaceIRI");
        Variable datatype = SparqlBuilder.var("datatype");
        Variable objectClassId = SparqlBuilder.var("objectClassId");

        // RdfLiteral.NumericLiteral objectClassId = Rdf.literalOf(35);
        List<Integer> objectClassIdValues = new ArrayList<>(Arrays.asList(33, 35));

        Expression dataType = Expressions.function(SparqlFunction.DATATYPE, polygonData);

        GraphPattern gp = GraphPatterns
                .and(surfaceIRI.has(OCGML_GEOM, polygonData).andHas(OCGML_CITYOBJECT, geometricIRI),
                        geometricIRI.has(OCGML_OBJECTCLASSID, objectClassId).andHas(OCGML_BUILDINGID, objectIRI))
                .filter(Expressions.not(Expressions.function(SparqlFunction.IS_BLANK, polygonData)));
        ValuesPattern<Iri> vp = new ValuesPattern<>(objectIRI,
                ObjectIRI.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        ValuesPattern<Integer> vp2 = new ValuesPattern<>(objectClassId, objectClassIdValues, Integer.class);

        query.select(polygonData, objectIRI, dataType.as(datatype), objectClassId).where(gp, vp, vp2).orderBy(objectIRI,
                objectClassId);
        JSONArray GeometricQueryResult = AccessAgentCaller.queryStore(GeospatialQueryIRI, query.getQueryString());

        return GeometricQueryResult;
    }

    public static List<byte[]> getElevationData() {

        EndpointConfig endpointConfig = new EndpointConfig();
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());

        String sql = "SELECT ST_AsGDALRaster(rast, 'GTiff') AS tiff FROM elevation";
        List<byte[]> elevData = new ArrayList<>();
        try (Statement stmt = rdbStoreClient.getConnection().createStatement()) {
            ResultSet result = stmt.executeQuery(sql);
            while (result.next()) {
                byte[] rasterBytes = result.getBytes("raster_data");
                elevData.add(rasterBytes);
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }
        return elevData;
    }

}
