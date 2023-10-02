package uk.ac.cam.cares.jps.agent.isochroneagent;

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

@WebServlet(urlPatterns = "/update")

public class IsochroneAgent extends JPSAgent {
    private static final String PROPETIES_PATH = "/usr/local/tomcat/resources/config.properties";
    private static final Path POI_PATH = Path.of("C:/TheWorldAvatar/Agents/IsochroneAgent/inputs/15MSC/POIqueries");
    private static final Path EDGESTABLESQL_PATH = Path.of("C:/TheWorldAvatar/Agents/IsochroneAgent/inputs/15MSC/edgesSQLTable");


    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;

    private String dbUser;
    private String dbPassword;
    private String kgEndpoint;

    private RemoteStoreClient storeClient;

    private RemoteRDBStoreClient remoteRDBStoreClient;
    private QueryClient queryClient;
    private RemoteStoreClient ontopStoreClient;

    public int timeThreshold;
    public int timeInterval;
    public ArrayList<String> populationTableList;

    public void init() {
        readConfig();
        this.dbUrl = endpointConfig.getDbUrl(dbName);
        this.dbUser = endpointConfig.getDbUser();
        this.dbPassword = endpointConfig.getDbPassword();
        this.kgEndpoint = endpointConfig.getKgurl();

        this.storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        this.remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(dbName), endpointConfig.getDbUser(), endpointConfig.getDbPassword());
        this.ontopStoreClient = new RemoteStoreClient(endpointConfig.getOntopurl());
        this.queryClient = new QueryClient(storeClient, ontopStoreClient, remoteRDBStoreClient);
    }

    public void readConfig() {
        try (InputStream input = FileReader.getStream(PROPETIES_PATH)) {
            Properties prop = new Properties();
            prop.load(input);
            this.dbName = prop.getProperty("db.name");
            this.timeThreshold = Integer.parseInt(prop.getProperty("timeThreshold"));
            this.timeInterval = Integer.parseInt(prop.getProperty("timeInterval"));
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
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {

            //Try to read
            Map POImap = FileReader.readPOIsparql(POI_PATH);
            Map EdgesTableSQL = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);


            QueryClient queryClient = new QueryClient(storeClient,ontopStoreClient,remoteRDBStoreClient);

            System.out.println(EdgesTableSQL);

            // Execute SPARQL queries
//            queryClient.getPOIwkt();

            // Segmentize road segments


            // Retrieve nearest nodes from segmentized road segments

            // Take the nearest node and input into isochrone generator

            // Create geoserver layer


        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        return requestParams;
    }


}