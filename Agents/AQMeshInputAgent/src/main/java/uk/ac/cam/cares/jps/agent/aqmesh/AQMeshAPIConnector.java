package uk.ac.cam.cares.jps.agent.aqmesh;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpRequest;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.util.Properties;

/**
 * Class to directly communicate with the AQMesh API.
 * @author Niklas Kasenburg
 */
public class AQMeshAPIConnector {

    // Fields to access API
    private String username;
    private String password;
    private String api_url = "https://apitest.aqmeshdata.net/api/";
    // Token needed to all API calls (except to retrieve token)
    private String token = "";
    // Location needed for retrieving data
    private String location = "";
    // Static fields for specific paths in the API
    protected static final String AUTHENTICATE_PATH = "Authenticate";
    protected static final String PING_PATH = "serverping";
    protected static final String ASSETS_PATH = "Pods/Assets";
    protected static final String READINGS_PATH = "LocationData/Next";
    protected static final String CELSIUS_MASS_PER_VOLUME = "01";
    protected static final String TPC = "1";
    // Static fields for specific keys in response bodies
    protected static final String TOKEN_KEY = "token";
    protected static final String LOCATION_KEY = "location_number";

    // Enum class for allowed reading types
    private enum ReadingType {
        GAS("1"),
        PARTICLE("2");

        public final String param;

        ReadingType(String param) {
            this.param = param;
        }
    }

    /**
     * Standard constructor
     * @param username the username to access AQMesh API
     * @param password the password to access AQMesh API
     * @param url the  URL of AQMesh API
     */
    public AQMeshAPIConnector(String username, String password, String url) {
        this.username = username;
        this.password = password;
        this.api_url = url;
    }

    /**
     * Constructor using a properties file
     * @param filepath Path to the properties file from which to read the username, password and URL
     */
    public AQMeshAPIConnector(String filepath) throws IOException {
        loadAPIConfigs(filepath);
    }

    /**
     * Connects to the AQMesh API by retrieving and setting an access token that is required for all other API calls for authentication
     */
    public void connect() {
        try {
            token = getAccessToken();
            String serverTime = ping();
            System.out.println("Connection successful at server time " + serverTime + ". Token will be valid for 120 minutes.");
        } catch (IOException e) {
            throw  new JPSRuntimeException("Unable to connect to AQMesh API!", e);
        }
    }

    /**
     * Get method for the access token. String is empty if connect() was not run
     * @return The access token as string
     */
    public String getToken() {
        return token;
    }

    /**
     * Get method for the location. String can be empty if no location was set yet.
     * @return The location as string
     */
    public String getLocation() {
        return location;
    }

    /**
     * Retrieves an access token from the AQMesh API
     * @return The access token as string
     */
    private String getAccessToken() throws IOException {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpPost postRequest = new HttpPost(api_url + AUTHENTICATE_PATH);
            JSONObject body = new JSONObject();
            body.put("username", username);
            body.put("password", password);
            postRequest.setEntity(new StringEntity(body.toString(), ContentType.APPLICATION_JSON));

            try (CloseableHttpResponse response = httpclient.execute(postRequest)) {
                int status = response.getStatusLine().getStatusCode();
                switch (status) {
                    case 400:
                    case 401:
                        throw new HttpResponseException(status, "Invalid username or password.");
                    case 200:
                        JSONObject responseBody = new JSONObject(EntityUtils.toString(response.getEntity()));
                        return responseBody.get(TOKEN_KEY).toString();
                    default:
                        throw new HttpResponseException(status, "Could not retrieve access token.");
                }
            }
        }
    }

    /**
     * Method to test whether the API is accessible after the token was set
     * @return The current server time
     */
    public String ping() throws IOException {
        if (token.equals("")) {
            throw new JPSRuntimeException("Token is not set. Use the connect method first.");
        }

        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet pingRequest = new HttpGet(api_url + PING_PATH);
            setTokenAuthorization(pingRequest);

            try (CloseableHttpResponse response = httpclient.execute(pingRequest)) {
                if (response.getStatusLine().getStatusCode() != 200) {
                    throw new HttpResponseException(response.getStatusLine().getStatusCode(),
                            "Unexpected status code.");
                }
                else {
                    JSONObject responseBody = new JSONObject(EntityUtils.toString(response.getEntity()));
                    return responseBody.getString("server_time");
                }
            }
        }
    }

    /**
     * Retrieves and sets the location of the AQMesh
     */
    private void setLocation() throws IOException, JSONException {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet assetRequest = new HttpGet(api_url + ASSETS_PATH);
            setTokenAuthorization(assetRequest);

            try (CloseableHttpResponse response = httpclient.execute(assetRequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                        JSONArray responseBody = new JSONArray(EntityUtils.toString(response.getEntity()));
                        if (responseBody.isEmpty()) {
                            throw new JSONException("No assets available in returned JSON Array.");
                        }
                        else {
                            int location = responseBody.getJSONObject(0).getInt(LOCATION_KEY);
                            this.location = Integer.toString(location);
                        }
                }
                else {
                    throw new HttpResponseException(status, "Could not retrieve location number.");
                }
            }
        }
    }

    /**
     * Sets the current token as authorization in the header of the request
     * @param request The request to which to add the token authorization
     */
    private void setTokenAuthorization(HttpRequest request) {
        String authHeader = "Bearer " + token;
        request.setHeader(HttpHeaders.AUTHORIZATION, authHeader);
    }

    /**
     * Retrieves the latest particle readings from the AqMesh API
     * @return Readings in a JSON Array with a JSON object for each measurement time
     */
    public JSONArray getParticleReadings() {
        try {
            return retrieveReadings(ReadingType.PARTICLE);
        }
        catch (IOException e) {
            throw new JPSRuntimeException("Particle readings could not be retrieved", e);
        }
    }

    /**
     * Retrieves the latest gas readings from the AqMesh API
     * @return Readings in a JSON Array with a JSON object for each measurement time
     */
    public JSONArray getGasReadings() {
        try {
            return retrieveReadings(ReadingType.GAS);
        }
        catch (IOException e) {
            throw new JPSRuntimeException("Gas readings could not be retrieved", e);
        }
    }

    /**
     * Retrieves the latest readings from the AqMesh API
     * @param readingType Specifies the type of readings (GAS or PARTICLE)
     * @return Readings in a JSON Array with a JSON object for each measurement time
     */
    private JSONArray retrieveReadings(ReadingType readingType) throws IOException, JSONException {

        if (location.equals("")) {
            System.out.println("No location (pod) set. Will retrieve location number first.");
            setLocation();
        }

        String readingPath = String.join("/",api_url, READINGS_PATH, location, readingType.param, CELSIUS_MASS_PER_VOLUME);
        // For particle readings add TPC to the API path
        if (readingType.equals(ReadingType.PARTICLE)) {
            readingPath = readingPath + "/" + TPC;
        }

        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readingRequest = new HttpGet(readingPath);
            setTokenAuthorization(readingRequest);

            try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONArray(EntityUtils.toString(response.getEntity()));
                }
                else {
                    throw new HttpResponseException(status, "Could not retrieve readings due to server error.");
                }
            }
        }
    }

    /**
     * Reads the username, password and URL needed to connect to the API from a properties file and saves it in fields
     * @param filepath Path to the properties file from which to read the username, password and URL
     */
    private void loadAPIConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        // Read username and password for AQMesh API from properties file
        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            // Get username and password from properties file
            if (prop.containsKey("aqmesh.username")) {
                this.username = prop.getProperty("aqmesh.username");
            } else {
                throw new IOException("Properties file is missing \"aqmesh.username=<aqmesh_username>\"");
            }
            if (prop.containsKey("aqmesh.password")) {
                this.password = prop.getProperty("aqmesh.password");
            } else {
                throw new IOException("Properties file is missing \"aqmesh.password=<aqmesh_password>\"");
            }
            if (prop.containsKey("aqmesh.url")) {
                this.api_url = prop.getProperty("aqmesh.url");
            } else {
                throw new IOException("Properties file is missing \"aqmesh.url=<aqmesh_url>\"");
            }
        }
    }

}
