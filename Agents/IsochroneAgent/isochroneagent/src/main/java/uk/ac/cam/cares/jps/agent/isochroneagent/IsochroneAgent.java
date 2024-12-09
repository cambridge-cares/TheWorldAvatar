package uk.ac.cam.cares.jps.agent.isochroneagent;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import com.cmclinnovations.stack.clients.ontop.OntopClient;

@WebServlet(urlPatterns = "/update")

public class IsochroneAgent extends JPSAgent {

    private static String isochroneFunction = null;
    private static int timeThreshold;
    private static int timeInterval;
    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private static final Path obdaFile = Path.of("/inputs/isochrone.obda");
    private final String FUNCTION_KEY = "function";
    private final String TIMETHRESHOLD_KEY = "timethreshold";
    private final String TIMEINTERVAL_KEY = "timeinterval";

    private static final Logger LOGGER = LogManager.getLogger(IsochroneAgent.class);

    private EndpointConfig endpointConfig = new EndpointConfig();
    private String dbName;
    private static String populationTables;
    private String kgEndpoint;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    public double segmentization_length;
    public ArrayList<String> populationTableList;
    private List<Integer> floodDepthList = Arrays.asList(10, 30, 90);
    private Integer floodCutoff = 30;
    private String poiTableName = "poi_nearest_node";
    private String routeTableName = "routing_ways";
    private String workspaceName = "twa";
    private String schema = "public";
    private Boolean oncePerPOI = true;
    private Boolean updateOntop = false;

    /**
     * Initialise agent
     */
    public void init() {
        readConfig();

        if (!kgEndpoint.isEmpty()) {
            try {
                this.storeClient = new RemoteStoreClient(kgEndpoint, kgEndpoint);
            } catch (Exception e) {
                System.out.println(e + "Invalid blazegraph endpoint specified");
            }
        } else { // Follow the running stack's blazegraph URL
            this.storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        }

        this.remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(dbName),
                endpointConfig.getDbUser(), endpointConfig.getDbPassword());
    }

    /**
     * Read configuration settings from config.properties
     */
    public void readConfig() {
        try (InputStream input = FileReader.getStream(PROPETIES_PATH)) {
            Properties prop = new Properties();
            prop.load(input);
            this.dbName = prop.getProperty("db.name");
            this.segmentization_length = Double.parseDouble(prop.getProperty("segmentization_length"));
            this.kgEndpoint = prop.getProperty("kgEndpoint");
            this.populationTables = prop.getProperty("populationTables");
            // Split the string using the comma as the delimiter
            String[] tableNames = populationTables.split("\\s*,\\s*");
            this.populationTableList = new ArrayList<>(Arrays.asList(tableNames));
            if (prop.getProperty("flood_depth_cm") != null) {
                String floodDepthString = prop.getProperty("flood_depth_cm");
                this.floodDepthList = Arrays.stream(floodDepthString.split(",")).map(Integer::parseInt)
                        .collect(Collectors.toList());
            }
            if (prop.getProperty("floodCutoff") != null) {
                this.floodCutoff = Integer.parseInt(prop.getProperty("floodCutoff"));
            }
            if (prop.getProperty("poiTableName") != null) {
                this.poiTableName = prop.getProperty("poiTableName");
            }
            if (prop.getProperty("routeTableName") != null) {
                this.routeTableName = prop.getProperty("routeTableName");
            }
            if (prop.getProperty("workspaceName") != null) {
                this.workspaceName = prop.getProperty("workspaceName");
            }
            if (prop.getProperty("schema") != null) {
                this.schema = prop.getProperty("schema");
            }
            if (prop.getProperty("oncePerPOI") != null) {
                this.oncePerPOI = Boolean.parseBoolean(prop.getProperty("oncePerPOI"));
            }
            if (prop.getProperty("updateOntop") != null) {
                this.updateOntop = Boolean.parseBoolean(prop.getProperty("updateOntop"));
            }
            if (floodDepthList.stream().noneMatch(x -> Objects.equals(x, floodCutoff))) {
                throw new JPSRuntimeException("Flood cutoff must be included in the list of flood depth.");
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Process request parameters and run functions within agent in the following
     * flow
     * 1) Read files
     * 2) Retrieve POI locations from KG
     * 3) Segmentize road networks, find nearest_nodes of POIs
     * 4) Generate isochrones based on settings
     * 5) Map population to the isochrones
     * 6) Create geoserver layer using geoserverclient
     * 7) Upload .obda mapping using ontopclient
     * 
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        this.isochroneFunction = requestParams.getString(FUNCTION_KEY);
        this.timeThreshold = requestParams.getInt(TIMETHRESHOLD_KEY);
        this.timeInterval = requestParams.getInt(TIMEINTERVAL_KEY);

        Path FUNCTION_PATH = Path.of("/inputs/" + isochroneFunction);

        LOGGER.info("Successfully set timeThreshold to " + timeThreshold);
        LOGGER.info("Successfully set timeInterval to " + timeInterval);
        LOGGER.info("Successfully set isochroneFunction to " + isochroneFunction);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set isochroneFunction to " + isochroneFunction + ", timeInterval = "
                + timeInterval + " minutes, timeThreshold = " + timeThreshold + " minutes");

        Path POI_PATH = FUNCTION_PATH.resolve("POIqueries");
        Path EDGESTABLESQL_PATH = FUNCTION_PATH.resolve("edgesSQLTable");

        try {
            init();
            // Read SPARQL and SQL files.
            Map<String, String> POImap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> EdgesTableSQLMap = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

            // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs
            // to the cumulative array
            JSONArray cumulativePOI = FileReader.getPOILocation(storeClient, POImap);

            // Split road into multiple smaller segment and find the nearest_node
            RouteSegmentization routeSegmentization = new RouteSegmentization(poiTableName, routeTableName);
            if (!routeSegmentization.doesTableExist(remoteRDBStoreClient)) {
                // If segment table doesnt exist, segment table
                routeSegmentization.segmentize(remoteRDBStoreClient, segmentization_length);
                if (isochroneFunction.equals("UR")) {
                    for (Integer floodDepth : floodDepthList) {
                        routeSegmentization.createFloodCost(remoteRDBStoreClient, floodDepth);
                    }
                }
            }

            // Create a table to store nearest_node
            routeSegmentization.insertPoiData(remoteRDBStoreClient, cumulativePOI);

            // Isochrone generator SQL will take 4 inputs (remoteRDBStoreClient,
            // timeThreshold, timeInterval, EdgesTableSQLMap)
            IsochroneGenerator isochroneGenerator = new IsochroneGenerator(poiTableName, routeTableName);
            isochroneGenerator.generateIsochrone(remoteRDBStoreClient, timeThreshold, timeInterval, EdgesTableSQLMap,
                    oncePerPOI);
            isochroneGenerator.createIsochroneBuilding(remoteRDBStoreClient);

            // Population matcher
            PopulationMapper populationMapper = new PopulationMapper();
            populationMapper.addPostgisRasterAndGDALDriver(remoteRDBStoreClient, dbName);
            populationMapper.checkAndAddColumns(remoteRDBStoreClient, populationTableList);
            populationMapper.mapPopulation(remoteRDBStoreClient, populationTableList);

            // Create geoserver layer
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            geoServerClient.createWorkspace(workspaceName);

            String isochroneAggregatedSQL = "SELECT minute, transportmode, transportmode_iri, poi_type, "
                    + "CONCAT('https://www.theworldavatar.com/kg/ontoisochrone/',iri) as iri, "
                    + "CONCAT(transportmode,' (', poi_type,')') as name, roadcondition, roadcondition_iri, geometry_iri, "
                    + populationTables + ", ST_Force2D(geom) as geom FROM isochrone_aggregated";

            createGeoserverLayer(geoServerClient, isochroneAggregatedSQL, "isochrone_aggregated_virtualTable",
                    "isochrone_aggregated");

            if (isochroneFunction.equals("UR")) {
                String unreachableSQL = "WITH cf AS (SELECT af.minute, "
                        + "ST_Difference(ST_ConcaveHull(ST_Points(fp.geom),0.2, false), af.geom) AS geom "
                        + "FROM flood_polygon_single_"+floodCutoff+"cm AS fp, (SELECT minute, geom AS geom "
                        + "FROM isochrone_aggregated WHERE roadcondition = 'Flooded') AS af) SELECT 'Unreachable Area' AS name, "
                        + "cf.minute, CAST(subquery.sum AS INT) AS population, cf.geom AS geom FROM cf, "
                        + "(SELECT cf.geom, SUM((ST_SummaryStats(ST_Clip(population.rast, cf.geom, TRUE))).sum) "
                        + "FROM population, cf GROUP BY cf.geom) AS subquery WHERE subquery.geom = cf.geom";
                createGeoserverLayer(geoServerClient, unreachableSQL, "unreachable", "unreachable");
                String affectedSQL = "SELECT 'Affected Area' as name, af.minute, "
                        + "an.population - af.population AS population, ST_Difference(an.geom, af.geom) AS geom "
                        + "FROM (SELECT minute, ST_Union(geom) AS geom, SUM(population) AS population "
                        + "FROM isochrone_aggregated WHERE roadcondition = 'Flooded' GROUP BY minute) AS af "
                        + "JOIN (SELECT minute, ST_Union(geom) AS geom, SUM(population) AS population "
                        + "FROM isochrone_aggregated WHERE roadcondition = 'Normal' GROUP BY minute) AS an "
                        + "ON af.minute = an.minute\n";
                createGeoserverLayer(geoServerClient, affectedSQL, "affected", "affected");
            }

            // Upload Isochrone Ontop mapping
            OntopClient ontopClient = OntopClient.getInstance();
            if (updateOntop) {
                try {
                    ontopClient.updateOBDA(obdaFile);
                    System.out.println("Ontop mapping updated.");
                } catch (Exception e) {
                    System.out.println("Could not retrieve isochrone .obda file.");
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        return response;
    }

    /**
     * Check if the JSONObject in the processRequestParameters inputs are correct or
     * missing.
     * 
     * @param requestParams
     * @return
     * @throws BadRequestException
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (!requestParams.has(FUNCTION_KEY)) {
            LOGGER.error("Function is missing.");
            return false;
        }
        if (!requestParams.has(TIMEINTERVAL_KEY)) {
            LOGGER.error("TimeInterval is missing.");
            return false;
        }
        if (!requestParams.has(TIMETHRESHOLD_KEY)) {
            LOGGER.error("TimeThreshold is missing.");
            return false;
        }
        return true;
    }

    void createGeoserverLayer(GeoServerClient geoServerClient, String sql, String virtualTableName, String layerName) {
        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql(sql);
        virtualTable.setEscapeSql(true);
        virtualTable.setName(virtualTableName);
        virtualTable.addVirtualTableGeometry("geometry", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName, layerName, dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName, schema, layerName, geoServerVectorSettings);
    }
}