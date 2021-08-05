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
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.io.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
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
    private static final String AUTHENTICATE_PATH = "Authenticate";
    private static final String ASSETS_PATH = "Pods/Assets";
    // Static fields for specific keys in response bodies
    private  static final String TOKEN_KEY = "token";
    private  static final String LOCATION_KEY = "location_number";

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
        } catch (IOException e) {
            throw  new JPSRuntimeException("Access token for AQMesh API could not be retrieved!", e);
        }
        System.out.println("Token received from AQMesh API. It will be valid for 120 minutes.");
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
                        throw new HttpResponseException(status, "Could not retrieve token.");
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
                            location = responseBody.getJSONObject(0).getString(LOCATION_KEY);
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

    public TimeSeries<LocalDateTime> getParticleReadings() {

        JSONArray particleReadings = retrieveReadings();
        List<LocalDateTime> times = new ArrayList<>();
        List<List<?>> values = new ArrayList<>();
        List<String> dataIRI = new ArrayList<>();

        return new TimeSeries<LocalDateTime>(times, dataIRI, values);
    }

    public TimeSeries<LocalDateTime> getGasReadings() {

        JSONArray particleReadings = retrieveReadings();
        List<LocalDateTime> times = new ArrayList<>();
        List<List<?>> values = new ArrayList<>();
        List<String> dataIRI = new ArrayList<>();

        return new TimeSeries<LocalDateTime>(times, dataIRI, values);
    }

    private JSONArray retrieveReadings() {
        return new JSONArray();
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
