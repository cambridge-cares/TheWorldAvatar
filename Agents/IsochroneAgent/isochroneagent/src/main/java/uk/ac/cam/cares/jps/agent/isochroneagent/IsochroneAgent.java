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
import java.util.Map;
import java.util.Properties;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.ontop.OntopClient;

@WebServlet(urlPatterns = "/update")

public class IsochroneAgent extends JPSAgent {
    
    private static String isochroneFunction = null; 
    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private static final Path obdaFile = Path.of("/inputs/isochrone.obda");
    private final String FUNCTION_KEY = "function";

    private static final Logger LOGGER = LogManager.getLogger(IsochroneAgent.class);

    private EndpointConfig endpointConfig = new EndpointConfig();
    private String dbName;
    private String kgEndpoint;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    public int timeThreshold;
    public int timeInterval;
    public double segmentization_length;
    public ArrayList<String> populationTableList;

    /**
     * Initialise agent
     */
    public void init() {
        readConfig();

        if(!kgEndpoint.isEmpty() ){
            try {
                this.storeClient = new RemoteStoreClient(kgEndpoint, kgEndpoint);
            } catch (Exception e) {
             System.out.println(e + "Invalid blazegraph endpoint specified");
            }
        }else
        {   //Follow the running stack's blazegraph URL
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
            this.timeThreshold = Integer.parseInt(prop.getProperty("timeThreshold"));
            this.timeInterval = Integer.parseInt(prop.getProperty("timeInterval"));
            this.segmentization_length = Double.parseDouble(prop.getProperty("segmentization_length"));
            this.kgEndpoint = prop.getProperty("kgEndpoint");

            String populationTables = prop.getProperty("populationTables");
            // Split the string using the comma as the delimiter
            String[] tableNames = populationTables.split("\\s*,\\s*");
            this.populationTableList = new ArrayList<String>(Arrays.asList(tableNames));

        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Process request parameters and run functions within agent in the following flow
     * 1) Read files
     * 2) Retrieve POI locations from KG
     * 3) Segmentize road networks, find nearest_nodes of POIs
     * 4) Generate isochrones based on settings
     * 5) Map population to the isochrones
     * 6) Create geoserver layer using geoserverclient
     * 7) Upload .obda mapping using ontopclient
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        this.isochroneFunction = requestParams.getString(FUNCTION_KEY);
        LOGGER.info("Successfully set isochroneFunction to " + isochroneFunction);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set isochroneFunction to " + isochroneFunction);

        Path POI_PATH = Path.of("/inputs/"+isochroneFunction+"/POIqueries");
        Path EDGESTABLESQL_PATH = Path.of("/inputs/"+isochroneFunction+"/edgesSQLTable");


        try {
            init();
            // Read SPARQL and SQL files.
            Map<String, String> POImap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> EdgesTableSQLMap = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

            // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs to the cumulative array
            JSONArray cumulativePOI = FileReader.getPOILocation(storeClient, POImap);

            // Split road into multiple smaller segment and find the nearest_node
            RouteSegmentization routeSegmentization = new RouteSegmentization();
            routeSegmentization.segmentize(remoteRDBStoreClient, segmentization_length);

            // Create a table to store nearest_node
            routeSegmentization.insertPoiData(remoteRDBStoreClient, cumulativePOI);


            // Isochrone generator SQL will take 4 inputs (remoteRDBStoreClient, timeThreshold, timeInterval, EdgesTableSQLMap)
            IsochroneGenerator isochroneGenerator = new IsochroneGenerator();
            isochroneGenerator.generateIsochrone(remoteRDBStoreClient, timeThreshold, timeInterval, EdgesTableSQLMap);
            isochroneGenerator.createIsochroneBuilding(remoteRDBStoreClient);

            // Population matcher
            PopulationMapper populationMapper = new PopulationMapper();
            populationMapper.addPostgisRasterAndGDALDriver(remoteRDBStoreClient, dbName);
            populationMapper.checkAndAddColumns(remoteRDBStoreClient, populationTableList);
            populationMapper.mapPopulation(remoteRDBStoreClient, populationTableList);

            //Create geoserver layer
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName= "isochrone";
            String schema = "public";
            geoServerClient.createWorkspace(workspaceName);
            geoServerClient.createPostGISDataStore(workspaceName,"isochrone_aggregated" , dbName, schema);

            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            geoServerClient.createPostGISLayer(workspaceName, dbName,"isochrone_aggregated" ,geoServerVectorSettings);

            //Upload Isochrone Ontop mapping
            try {
                OntopClient ontopClient = OntopClient.getInstance();
                ontopClient.updateOBDA(obdaFile);
            } catch (Exception e) {
                System.out.println("Could not retrieve isochrone .obda file.");
            }

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        return response;
    }

    /**
     * Check if the JSONObject in the processRequestParameters inputs are correct or missing.
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
        return true;
    }
}