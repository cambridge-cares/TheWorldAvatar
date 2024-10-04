package uk.ac.cam.cares.jps.agent.osmagent;

import com.bigdata.bop.rdf.aggregate.COUNT;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.GeometryMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageShareCalculator;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import com.cmclinnovations.stack.clients.ontop.OntopClient;

import javax.servlet.annotation.WebServlet;
import java.nio.file.Path;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import org.json.JSONObject;

@WebServlet(urlPatterns = "/update")

public class OSMAgent extends JPSAgent {
    private static final String PROPERTIES_PATH = "/resources/config.properties";
    private static final Path obdaFile = Path.of("/resources/building_usage.obda");

    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;
    private String osmSchema;
    private String dbUser;
    private String dbPassword;
    public String pointTable;
    public String polygonTable;
    public String landUseTable;
    public String landGeometry;
    public String landUseCsv;

    public static final String DATA_SCHEMA = "buildinginfo";
    public static final String USAGE_TABLE = DATA_SCHEMA + ".usage";
    public static final String ADDRESS_TABLE = DATA_SCHEMA + ".address";
    public static final String COUNT_SUFFIX = "_count";
    public static final String AREA_SUFFIX = "_area";
    public static final String USAGE_COUNT = USAGE_TABLE + COUNT_SUFFIX;
    public static final String USAGE_AREA = USAGE_TABLE + AREA_SUFFIX;
    public static final String GEO_VIEW_COUNT = DATA_SCHEMA + ".usage_geoserver" + COUNT_SUFFIX;
    public static final String GEO_VIEW_AREA = DATA_SCHEMA + ".usage_geoserver" + AREA_SUFFIX;

    public static final String KEY_BOUND = "bound_wkt";
    public static final String KEY_BOUND_SRID = "bound_srid";
    public void init() {
        readConfig();
        this.pointTable = osmSchema + "." + "points";
        this.polygonTable = osmSchema + "." + "polygons";
        this.dbUrl = endpointConfig.getDbUrl(dbName);
        this.dbUser = endpointConfig.getDbUser();
        this.dbPassword = endpointConfig.getDbPassword();
    }

    public void readConfig() {
        try (InputStream input = FileReader.getStream(PROPERTIES_PATH)) {
            Properties prop = new Properties();
            prop.load(input);
            this.dbName = prop.getProperty("db.name");
            this.osmSchema = prop.getProperty("osm.schema");
            this.landUseTable = prop.getProperty("landuse.table");
            this.landGeometry = prop.getProperty("landuse.geometry");
            this.landUseCsv = prop.getProperty("landuse.csv");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found.");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {
            String bound = null;
            Integer boundSRID = null;

            if (requestParams.has(KEY_BOUND)) {
                bound = requestParams.getString(KEY_BOUND);
                if (requestParams.has(KEY_BOUND_SRID)) {
                    boundSRID = requestParams.getInt(KEY_BOUND_SRID);
                }
                else {
                    throw new JPSRuntimeException("bound_wkt was specified for running OSM agent for selected area, but its SRID was not specified.");
                }

            }

            UsageMatcher usageMatcher = new UsageMatcher(dbUrl, dbUser, dbPassword);
            UsageShareCalculator shareCalculator = new UsageShareCalculator(dbUrl, dbUser, dbPassword);
            GeometryMatcher geometryMatcher = new GeometryMatcher(dbUrl, dbUser, dbPassword);

            // match OSM usage to OntoBuiltEnv:PropertyUsage classes
            usageMatcher.checkAndAddColumns(pointTable, polygonTable);
            usageMatcher.updateOntoBuilt(pointTable, polygonTable);

            // match OSM geometries with building IRI
            geometryMatcher.matchGeometry(pointTable, polygonTable, bound, boundSRID);

            // intialise usage table and copy building IRI that has OSM usage
            usageMatcher.copyUsage(pointTable, polygonTable, DATA_SCHEMA, USAGE_TABLE);

            usageMatcher.copyAddress(pointTable, polygonTable, DATA_SCHEMA, ADDRESS_TABLE);

            // match buildings without OSM usage with land use
            if (!landUseTable.isEmpty()) {
                geometryMatcher.updateLandUse(USAGE_TABLE, landUseTable, landGeometry, landUseCsv, bound, boundSRID);
            }

            // assign OntoBuiltEnv:PropertyUsage and calculate usage share for mixed usage
            // buildings
            shareCalculator.createUsageIRI(USAGE_TABLE);
            shareCalculator.usageShareCount(USAGE_COUNT, USAGE_TABLE, pointTable, polygonTable);
            shareCalculator.usageShareArea(USAGE_AREA, USAGE_TABLE, pointTable, polygonTable);

            // add materialized view for geoserver layer
            shareCalculator.addGeoserverView(GEO_VIEW_COUNT, USAGE_COUNT, pointTable, polygonTable);
            shareCalculator.addGeoserverView(GEO_VIEW_AREA, USAGE_AREA, pointTable, polygonTable);

            // create geoserver layer
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName= "twa";
            String schema = DATA_SCHEMA;
            geoServerClient.createWorkspace(workspaceName);
            geoServerClient.createPostGISDataStore(workspaceName, "building_usage" , dbName, schema);

            UpdatedGSVirtualTableEncoder virtualCount = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoCount = new GeoServerVectorSettings();
            virtualCount.setSql(String.format(buildingSQLQuery, GEO_VIEW_COUNT));
            virtualCount.setEscapeSql(true);
            virtualCount.setName("building_usage_count");
            virtualCount.addVirtualTableGeometry("geometry", "Geometry", "4326"); // geom needs to match the sql query
            geoCount.setVirtualTable(virtualCount);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "building_usage_count" ,geoCount);


            UpdatedGSVirtualTableEncoder virtualArea = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoArea = new GeoServerVectorSettings();
            virtualArea.setSql(String.format(buildingSQLQuery, GEO_VIEW_AREA));
            virtualArea.setEscapeSql(true);
            virtualArea.setName("building_usage_area");
            virtualArea.addVirtualTableGeometry("geometry", "Geometry", "4326"); // geom needs to match the sql query
            geoArea.setVirtualTable(virtualArea);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "building_usage_area" ,geoArea);

            try {
                OntopClient ontopClient = OntopClient.getInstance();
                ontopClient.updateOBDA(obdaFile);
            }
            catch (Exception e) {
                System.out.println("Could not retrieve building_usage.obda file.");
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }


    private static final String buildingSQLQuery = "SELECT iri, ontobuilt, usageshare, name, geometry, building_height FROM %s";
}