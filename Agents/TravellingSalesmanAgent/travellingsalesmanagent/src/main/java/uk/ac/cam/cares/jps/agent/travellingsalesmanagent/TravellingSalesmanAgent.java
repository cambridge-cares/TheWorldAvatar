package uk.ac.cam.cares.jps.agent.travellingsalesmanagent;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Properties;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;

@WebServlet(urlPatterns = "/runtsp")

public class TravellingSalesmanAgent extends JPSAgent {

    private static String tspFunction = null;

    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private final String FUNCTION_KEY = "function";

    private static final Logger LOGGER = LogManager.getLogger(TravellingSalesmanAgent.class);

    private EndpointConfig endpointConfig = new EndpointConfig();
    private String dbName;
    private String kgEndpoint;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    private String poiTableName = "poi_tsp_nearest_node";
    private String poiLayerName = "poi_tsp_nearest_node";
    private String floodTableName = "flood_polygon_single_10cm";
    private Double floodCutOff = 0.0;
    private String routeTablePrefix = "routing_";
    private String workspaceName = "twa";
    private String schema = "public";

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
            this.kgEndpoint = prop.getProperty("kgEndpoint");
            if (prop.getProperty("poiTableName") != null) {
                this.poiTableName = prop.getProperty("poiTableName");
            }
            if (prop.getProperty("poiLayerName") != null) {
                this.poiLayerName = prop.getProperty("poiLayerName");
            }
            if (prop.getProperty("floodTableName") != null) {
                this.floodTableName = prop.getProperty("floodTableName");
            }
            if (prop.getProperty("floodCutOff") != null) {
                this.floodCutOff = Double.parseDouble(prop.getProperty("floodCutOff"));
            }
            if (prop.getProperty("routeTablePrefix") != null) {
                this.routeTablePrefix = prop.getProperty("routeTablePrefix");
            }
            if (prop.getProperty("workspaceName") != null) {
                this.workspaceName = prop.getProperty("workspaceName");
            }
            if (prop.getProperty("schema") != null) {
                this.schema = prop.getProperty("schema");
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
     * 3) Find nearest_nodes of POIs
     * 4) Create TSP layer using geoserverclient - TSP Route, TSP sequence
     * 
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        this.tspFunction = requestParams.getString(FUNCTION_KEY);
        Path FUNCTION_PATH = Path.of("/inputs/" + tspFunction);

        if (!Files.exists(FUNCTION_PATH)) {
            throw new JPSRuntimeException("Function " + tspFunction + " is not set up with this agent.");
        }

        LOGGER.info("Successfully set tspFunction to " + tspFunction);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set tspFunction to " + tspFunction);

        Path POI_PATH = FUNCTION_PATH.resolve("POIqueries");
        Path EDGESTABLESQL_PATH = FUNCTION_PATH.resolve("edgesSQLTable");

        try {
            init();
            // Read SPARQL and SQL files.
            Map<String, String> poiMap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> edgesTableSQLMap = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

            // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs
            // to the cumulative array
            JSONArray cumulativePOI = FileReader.getPOILocation(storeClient, poiMap);

            // Split road into multiple smaller segment and find the nearest_node
            NearestNodeFinder nearestNodeFinder = new NearestNodeFinder(poiTableName);

            // Create a table to store nearest_node
            nearestNodeFinder.insertPoiData(remoteRDBStoreClient, cumulativePOI);

            // Create geoserver layer
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            geoServerClient.createWorkspace(workspaceName);

            GeoServerInteractor.addPOIGeoserverLayer(geoServerClient, workspaceName, schema, dbName, poiTableName,
                    poiLayerName);

            /**
             * Loop through the edgeTable SQL and generate two geoserver layer for each
             * edgeTableSQL
             * - TSP_route: Gives the geometry of the shortest path for all TSP points
             * - TSP_seq: Gives the sequence of order to visit all the TSP points
             */

            TSPRouteGenerator tspRouteGenerator = new TSPRouteGenerator(poiTableName, floodTableName, routeTablePrefix,
                    floodCutOff);

            tspRouteGenerator.updatePOIFloodStatus(remoteRDBStoreClient);
            for (Map.Entry<String, String> entry : edgesTableSQLMap.entrySet()) {
                String layerName = "TSP_" + entry.getKey();
                String sql = entry.getValue();
                tspRouteGenerator.generateTSPLayer(geoServerClient, workspaceName, schema, dbName, layerName, sql);
                tspRouteGenerator.generateSequenceLayer(geoServerClient, workspaceName, schema, dbName, layerName,
                        sql);
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
            LOGGER.error("The request parameter 'function' must be specified but is missing.");
            return false;
        }
        return true;
    }

}