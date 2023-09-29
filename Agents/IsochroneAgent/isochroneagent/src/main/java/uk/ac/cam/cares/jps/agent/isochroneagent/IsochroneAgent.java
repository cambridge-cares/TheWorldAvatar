package uk.ac.cam.cares.jps.agent.isochroneagent;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import uk.ac.cam.cares.jps.agent.isochroneagent.QueryClient;

import javax.servlet.annotation.WebServlet;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Properties;

@WebServlet(urlPatterns = "/update")

public class IsochroneAgent extends JPSAgent {
    private static final String PROPETIES_PATH = "/usr/local/tomcat/resources/config.properties";
    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;

    private String dbUser;
    private String dbPassword;
    private String kgEndpoint;

    public int timeThreshold;
    public int timeInterval;
    public ArrayList<String> populationTableList;

    public void init() {
        readConfig();
        this.dbUrl = endpointConfig.getDbUrl(dbName);
        this.dbUser = endpointConfig.getDbUser();
        this.dbPassword = endpointConfig.getDbPassword();
        this.kgEndpoint = endpointConfig.getKgurl();

        slPostGisClient = new SLPostGISClient(endpointConfig.getDburl(), endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
        tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class);
        RemoteStoreClient ontopStoreClient = new RemoteStoreClient(endpointConfig.getOntopurl());
        queryClient = new QueryClient(storeClient, ontopStoreClient, remoteRDBStoreClient);
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

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {

            QueryClient queryClient = new QueryClient();

            // Execute SPARQL queries
            QueryClient.class

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