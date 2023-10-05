package uk.ac.cam.cares.jps.agent.isochroneagent;

import org.json.JSONArray;
import org.json.JSONObject;

import com.bigdata.concurrent.TxDag.Edge;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

import javax.servlet.annotation.WebServlet;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.*;

@WebServlet(urlPatterns = "/update")

public class IsochroneAgent extends JPSAgent {
    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private static final Path POI_PATH = Path.of("/inputs/15MSC/POIqueries");
    private static final Path EDGESTABLESQL_PATH = Path.of("/inputs/15MSC/edgesSQLTable");

    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String kgEndpoint;

    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private RemoteStoreClient ontopStoreClient;

    public int timeThreshold;
    public int timeInterval;
    public double segmentization_length;
    public ArrayList<String> populationTableList;

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
        this.ontopStoreClient = new RemoteStoreClient(endpointConfig.getOntopurl());
    }

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
     * Process request params
     * 
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {
            init();
            // Read SPARQL and SQL files. 
            Map<String, String> POImap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> EdgesTableSQLMap = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

            // // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs to the cumulative array
            // JSONArray cumulativePOI = FileReader.getPOILocation(storeClient, POImap);

            // // Split road into multiple smaller segment and find the nearest_node 
            // RouteSegmentization routeSegmentization = new RouteSegmentization();
            // routeSegmentization.segmentize(remoteRDBStoreClient, segmentization_length);
            
            // // Create a table to store nearest_node 
            // routeSegmentization.insertPoiData(remoteRDBStoreClient, cumulativePOI);


            // // Isochrone generator SQL will take 4 inputs (remoteRDBStoreClient, timeThreshold, timeInterval, EdgesTableSQLMap)
            // IsochroneGenerator isochroneGenerator = new IsochroneGenerator();
            // isochroneGenerator.generateIsochrone(remoteRDBStoreClient, timeThreshold, timeInterval, EdgesTableSQLMap);

            // // Population matcher
            // PopulationMapper populationMapper = new PopulationMapper();
            // populationMapper.checkAndAddColumns(remoteRDBStoreClient, populationTableList);
            // populationMapper.mapPopulation(remoteRDBStoreClient, populationTableList);

            // Create geoserver layer            
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName= "isochrone"; 
            String schema = "public";
            geoServerClient.createWorkspace(workspaceName);
            geoServerClient.createPostGISDataStore(workspaceName,"isochrone_aggregated" , dbName, schema);
            
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            geoServerClient.createPostGISLayer(workspaceName, dbName,"isochrone_aggregated" ,geoServerVectorSettings);
            

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        return requestParams;
    }

}