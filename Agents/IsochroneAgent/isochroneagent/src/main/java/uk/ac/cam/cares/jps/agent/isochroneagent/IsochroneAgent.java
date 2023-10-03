package uk.ac.cam.cares.jps.agent.isochroneagent;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.*;
import java.util.Map.Entry;
import org.json.JSONArray;
import org.json.JSONObject;

@WebServlet(urlPatterns = "/update")

public class IsochroneAgent extends JPSAgent {
    private static final String PROPETIES_PATH = "/usr/local/tomcat/data/config.properties";
    private static final String SQL_PATH = "/usr/local/tomcat/data/sql";
    private static final Path POI_PATH = Path.of("/inputs/15MSC/POIqueries");
    private static final Path EDGESTABLESQL_PATH = Path.of("/inputs/15MSC/edgesSQLTable");

    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;

    private String dbUser;
    private String dbPassword;
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
            String[] tableNames = populationTables.split(",");
            this.populationTableList = new ArrayList<>(Arrays.asList(tableNames));

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

            // Try to read
            Map<String, String> POImap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> EdgesTableSQL = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);


            // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs
            // to the cumulative array
            // cumulativePOI contains the POI type and wkt
            JSONArray cumulativePOI = new JSONArray();
            for (Map.Entry<String, String> entry : POImap.entrySet()) {
                String value = entry.getValue();
                JSONArray POI = storeClient.executeQuery(value);

                // Iterate through the POIs in this iteration and add them to the cumulative
                // array
                for (int i = 0; i < POI.length(); i++) {
                    cumulativePOI.put(POI.get(i));
                }
            }

            // Segmentize road segments
            NearestNode.segmentization(remoteRDBStoreClient, segmentization_length);

            // // // Retrieve nearest nodes from segmentized road segments
            // NearestNode.insertPoiData(remoteRDBStoreClient, cumulativePOI);

            // // Take the nearest node and input into isochrone generator
            // JSONArray pois = NearestNode.getPOInearest_node();

            // Isochrone generator SQL will take 3 inputs (poi, upperTimeLimit,
            // edgesTableSQL)

            // Population matcher

            // Create geoserver layer

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        return requestParams;
    }

}