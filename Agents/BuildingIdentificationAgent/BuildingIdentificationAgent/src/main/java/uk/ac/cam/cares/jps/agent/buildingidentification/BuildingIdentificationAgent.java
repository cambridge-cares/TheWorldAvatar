package uk.ac.cam.cares.jps.agent.buildingidentification;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;
import org.apache.jena.sparql.function.library.print;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.sql.*;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.impl.CoordinateArraySequence;
import org.locationtech.jts.geom.Point;

import uk.ac.cam.cares.jps.agent.buildingidentification.objects.Factory;

@WebServlet(urlPatterns = { BuildingIdentificationAgent.URI_RUN })
public class BuildingIdentificationAgent extends JPSAgent {

    public static final String KEY_REQ_URL = "requestUrl";
    public static final String URI_RUN = "/run";
    public static final String KEY_DISTANCE = "maxDistance";
    private static final String rdfs = "http://www.w3.org/2000/01/rdf-schema#";
    private static final String rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    private static final String ontoCompanyPrefix = "http://www.theworldavatar.com/kg/ontocompany/";
    private static final String contactPrefix = "http://ontology.eil.utoronto.ca/icontact.owl#";
    private static final String ontoMeasurePrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/";

    private RemoteRDBStoreClient rdbStoreClient;
    private RemoteStoreClient storeClient;

    private List<Factory> factories = new ArrayList<>();
    private Set<String> factoryTypes = new HashSet<>();

    private static final Logger LOGGER = LogManager.getLogger(BuildingIdentificationAgent.class);

    // Properties of database containing buildings data.
    private String dbUrl = null;
    private String dbUser = null;
    private String dbPassword = null;
    private String dbName = null;
    private int dbSrid;
    private double maxDistance;
    private int numberBuildingsIdentified = 0, numberFactoriesQueried = 0;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {

            String endpoint = null;
            EndpointConfig endpointConfig = null;

            if (requestParams.has("endpoint"))
                endpoint = requestParams.getString("endpoint");
            else {
                String namespace = requestParams.getString("namespace");
                endpointConfig = new EndpointConfig();
                endpoint = endpointConfig.getKgurl(namespace);
            }

            storeClient = new RemoteStoreClient(endpoint);

            if (requestParams.has("dbUrl")) {
                dbUrl = requestParams.getString("dbUrl");
                dbUser = requestParams.getString("dbUser");
                dbPassword = requestParams.getString("dbPassword");
            } else {
                dbName = requestParams.getString("dbName");
                dbUrl = endpointConfig.getDbUrl(dbName);
                dbUser = endpointConfig.getDbUser();
                dbPassword = endpointConfig.getDbPassword();
            }

            rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);
            getDbSrid();
            maxDistance = Double.parseDouble(requestParams.getString(KEY_DISTANCE));
            // Reset all variables
            factories.clear();
            factoryTypes.clear();
            numberBuildingsIdentified = 0;
            numberFactoriesQueried = 0;

            getFactoryProperties();
            linkBuildings();
            createFactoriesTable();
            createGeoServerLayers();
        }

        JSONObject responseObject = new JSONObject();
        responseObject.put("number_factories", numberFactoriesQueried);
        responseObject.put("number_buildings", numberBuildingsIdentified);
        return responseObject;
    }

    /**
     * Checks validity of incoming request
     * 
     * @param requestParams Request parameters as JSONObject
     * @return Validity of request
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {
        return isNumber(requestParams.getString(KEY_DISTANCE));
    }

    /**
     * Queries database for the SRID
     * 
     */
    private void getDbSrid() {
        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {
            String sqlString = "SELECT srid,gml_srs_name from citydb.database_srs";
            ResultSet result = stmt.executeQuery(sqlString);
            if (result.next()) {
                dbSrid = result.getInt("srid");
            } else {
                LOGGER.warn("Could not retrieve srid from database.");
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }
    }

    private void getFactoryProperties() {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontocompany", ontoCompanyPrefix)
                .addPrefix("con", contactPrefix).addPrefix("rdfs", rdfs)
                .addPrefix("om", ontoMeasurePrefix).addPrefix("rdf", rdf);

        String addressVar = "?address";
        wb.addWhere("?factory", "con:hasAddress", addressVar)
                .addWhere("?company", "ontocompany:isOwnerOf", "?factory")
                .addWhere("?company", "rdfs:label", "?name")
                .addWhere(addressVar, "ontocompany:hasLongitudeEPSG4326", "?lon").addWhere(addressVar,
                        "ontocompany:hasLatitudeEPSG4326", "?lat")
                .addWhere("?factory", "ontocompany:hasGeneratedHeat/om:hasValue/om:hasNumericalValue", "?heat")
                .addWhere("?factory", "rdf:type", "?facType");
        SelectBuilder sb = new SelectBuilder()
                .addVar("factory")
                .addVar("lon")
                .addVar("lat")
                .addVar("name")
                .addVar("heat")
                .addVar("facType")
                .addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());

        for (int i = 0; i < queryResult.length(); i++) {
            String factoryIri = queryResult.getJSONObject(i).getString("factory");
            Double longitude = queryResult.getJSONObject(i).getDouble("lon");
            Double latitude = queryResult.getJSONObject(i).getDouble("lat");
            String factoryName = queryResult.getJSONObject(i).getString("name");
            Double heat = queryResult.getJSONObject(i).getDouble("heat");
            String facType = queryResult.getJSONObject(i).getString("facType");
            String originalSrid = "EPSG:4326";
            double[] xyOriginal = { longitude, latitude };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + dbSrid, xyOriginal);
            Point factoryLocation = new Point(
                    new CoordinateArraySequence(
                            new Coordinate[] { new Coordinate(xyTransformed[0], xyTransformed[1]) }),
                    new GeometryFactory());

            factoryLocation.setSRID(dbSrid);
            Factory plant = new Factory(facType, factoryIri, factoryName, heat, factoryLocation);
            factories.add(plant);
            factoryTypes.add(facType);

        }

        numberFactoriesQueried = factories.size();
    }

    /**
     * Identifies the building whose envelope centroid is closest to the coordinate
     * of each factory.
     * 
     * 
     * @return None
     */

    private void linkBuildings() {

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            for (Factory fac : factories) {
                String sqlString = String.format("select id, wkt, height, dist from ( " + System.lineSeparator() +
                        "select cityobject.id as id, measured_height as height, public.ST_AsText(envelope) as wkt, "
                        +
                        System.lineSeparator() +
                        "public.ST_DISTANCE(public.ST_Point(%f,%f, %d), envelope) AS dist from citydb.cityobject, citydb.building"
                        +
                        System.lineSeparator() +
                        "WHERE cityobject.objectclass_id = 26 AND cityobject.id = building.id" +
                        System.lineSeparator() +
                        ") AS sub" + System.lineSeparator() +
                        "order by dist" + System.lineSeparator() +
                        "limit 1",
                        fac.location.getX(), fac.location.getY(), dbSrid);

                ResultSet result = stmt.executeQuery(sqlString);

                while (result.next()) {
                    fac.buildingId = result.getInt("id");
                    String wktLiteral = result.getString("wkt");
                    fac.buildingFootprint = WKTReader.extract(wktLiteral).getGeometry();
                    fac.buildingHeight = result.getDouble("height");
                    Double dist = result.getDouble("dist");

                    if (dist > maxDistance)
                        LOGGER.warn("Nearest footprint for factory with IRI {} is {} meters away",
                                fac.factoryIri, dist);
                    numberBuildingsIdentified++;
                }

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

    }

    /*
     * Creates POSTGIS table with factory properties.
     */

    private void createFactoriesTable() {

        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");
        JSONArray features = new JSONArray();

        for (Factory fac : factories) {

            Geometry footPrint = fac.buildingFootprint;
            if (footPrint == null)
                continue;
            JSONObject geometry = new JSONObject();
            geometry.put("type", "Polygon");
            JSONArray geomArray = new JSONArray();
            Arrays.stream(footPrint.getCoordinates()).forEach(coord -> {
                double[] xyOriginal = { coord.x, coord.y };
                geomArray.put(new JSONArray(xyOriginal));
            });

            JSONArray coordinates = new JSONArray();
            coordinates.put(geomArray);

            geometry.put("coordinates", coordinates);
            JSONObject feature = new JSONObject();
            feature.put("type", "Feature");
            feature.put("geometry", geometry);
            JSONObject properties = new JSONObject();
            properties.put("iri", fac.factoryIri);
            properties.put("height", fac.buildingHeight);
            properties.put("heat", fac.heatEmission);
            properties.put("factory_type", fac.factoryClass);
            properties.put("name", fac.companyName);
            properties.put("building_id", fac.buildingId);
            feature.put("properties", properties);
            features.put(feature);

        }

        featureCollection.put("features", features);

        GDALClient gdalClient = GDALClient.getInstance();
        gdalClient.uploadVectorStringToPostGIS(dbName, "citydb.factories",
                featureCollection.toString(), new Ogr2OgrOptions(), false);
        LOGGER.info("Created factories table with {} records.", numberBuildingsIdentified);

    }

    /**
     * Creates Geoserver layers for all industries
     */

    private void createGeoServerLayers() {

        String geoserverWorkspace = "heat";
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        geoServerClient.deleteWorkspace(geoserverWorkspace);
        geoServerClient.createWorkspace(geoserverWorkspace);

        for (String facType : factoryTypes) {
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            virtualTable.setSql(String.format(
                    "select * from citydb.factories where factory_type = \'%s\'", facType));
            virtualTable.setEscapeSql(true);
            String[] facSplit = facType.split("/");
            String tableName = facSplit[facSplit.length - 1];
            virtualTable.setName(tableName);
            virtualTable.addVirtualTableGeometry("wkb_geometry", "Polygon", String.valueOf(dbSrid));
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISLayer(geoserverWorkspace, dbName, tableName, geoServerVectorSettings);

        }

    }

    /**
     * Checks if a string is able to be parsable as a number
     * 
     * @param number string to check
     * @return boolean value of check
     */
    public boolean isNumber(String number) {
        try {
            Double.parseDouble(number);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

}