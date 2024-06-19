package uk.ac.cam.cares.jps.agent.pips;

import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;

public class KeycloakConnector {
    private static final Logger LOGGER = LogManager.getLogger(KeycloakConnector.class);

    // variables
    private String tokenEndpoint = null;
    private String introspectionEndpoint = null;

    private static final String tokenEndpointKey = "token_endpoint";
    private static final String introspectionEndpointKey = "introspection_endpoint";
    private static final String keycloakRealmPath = System.getenv("KEYCLOAK_REALM_PATH");
    private static final String client_id = System.getenv("CLIENT_ID");

    // error messages
    private static final String GET_ENDPOINTS_ERROR = "Unable to get keycloak endpoints!";
    private static final String INVALID_GRANT_ERROR_KEY = "Invalid Grant Type";
    private static final String UNDEFINED_GRANT_ERROR_MSG = "Provided an invalid grant type";

    /**
     * Constructor that retrieves keycloak endpoints upon initalisation
     */
    public KeycloakConnector() {
        try {
            JSONObject endpoints = getEndpoints();
            this.tokenEndpoint = endpoints.getString(tokenEndpointKey);
            this.introspectionEndpoint = endpoints.getString(introspectionEndpointKey);
        } catch (Exception e) {
            throw new JPSRuntimeException(GET_ENDPOINTS_ERROR);
        }
    }

    /**
     * get the endpoints of the keycloak server
     * @return JSONObject containing metadata and endpoints of keycloak
     * @throws IOException
     */
    private JSONObject getEndpoints() throws IOException {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readingRequest = new HttpGet(keycloakRealmPath + "/.well-known/uma2-configuration");
            try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                }
                else {
                    throw new HttpResponseException(status, response.getEntity().toString());
                }
            }
        }
    }

    /**
     * get access and refresh tokens from keycloak token endpoint
     * @return JSONObject containing results of request
     * @throws IOException
     */
    private JSONObject getTokensViaCredentials() throws IOException {
        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(tokenEndpoint);

        String client_secrets = readFromFile(System.getenv("CLIENT_SECRETS"));
        String username = readFromFile(System.getenv("USERNAME"));
        String password = readFromFile(System.getenv("PASSWORD"));
        
        // Construct the request body
        String requestBody = "grant_type=password"
                + "&client_id=" + client_id
                + "&client_secret=" + client_secrets
                + "&username=" + username
                + "&password=" + password;

        // Set headers
        httpPost.setHeader("Content-Type", "application/x-www-form-urlencoded");

        // Set request body
        StringEntity entity = new StringEntity(requestBody, ContentType.APPLICATION_FORM_URLENCODED);
        httpPost.setEntity(entity);
        try (CloseableHttpResponse response = httpClient.execute(httpPost)) {
            int status = response.getStatusLine().getStatusCode();
            if (status == 200) {
                return new JSONObject(EntityUtils.toString(response.getEntity()));
            }
            else {
                throw new HttpResponseException(status, response.getEntity().toString());
            }
        }
    }

    /**
     * get access token and refresh token from keycloak token endpoint
     * @return JSONObject containing results of request
     * @throws IOException
     */
    public JSONObject getTokens(String grant_type) throws IOException {
        JSONObject jsonMessage = new JSONObject();
        switch (grant_type) {
            case "password":
            jsonMessage = getTokensViaCredentials();
            break;
            default:
            jsonMessage.put(INVALID_GRANT_ERROR_KEY, UNDEFINED_GRANT_ERROR_MSG + grant_type);
        }
        return jsonMessage;
    }

    /**
     * get access token and refresh token from keycloak token endpoint
     * @return JSONObject containing results of request
     * @throws IOException
     */
    public JSONObject refreshToken(String refreshToken) throws IOException {
        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(tokenEndpoint);

        String client_secrets = readFromFile(System.getenv("CLIENT_SECRETS"));
        
        // Construct the request body
        String requestBody = "grant_type=refresh_token"
                + "&refresh_token=" + refreshToken
                + "&client_id=" + client_id
                + "&client_secret=" + client_secrets;

        // Set headers
        httpPost.setHeader("Content-Type", "application/x-www-form-urlencoded");

        // Set request body
        StringEntity entity = new StringEntity(requestBody, ContentType.APPLICATION_FORM_URLENCODED);
        httpPost.setEntity(entity);
        try (CloseableHttpResponse response = httpClient.execute(httpPost)) {
            int status = response.getStatusLine().getStatusCode();
            if (status == 200) {
                return new JSONObject(EntityUtils.toString(response.getEntity()));
            }
            else {
                throw new HttpResponseException(status, response.getEntity().toString());
            }
        }
    }

    /**
     * introspect token via keycloak introspection endpoint
     * @param token token to introspect
     * @return JSONObject containing introspection results
     * @throws IOException
     */
    private JSONObject introspectToken(String token) throws IOException {
        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(introspectionEndpoint);

        String client_secrets = readFromFile(System.getenv("CLIENT_SECRETS"));
        
        // Construct the request body
        String requestBody = "token=" + token
                + "&client_id=" + client_id
                + "&client_secret=" + client_secrets;

        // Set headers
        httpPost.setHeader("Content-Type", "application/x-www-form-urlencoded");

        // Set request body
        StringEntity entity = new StringEntity(requestBody, ContentType.APPLICATION_FORM_URLENCODED);
        httpPost.setEntity(entity);
        try (CloseableHttpResponse response = httpClient.execute(httpPost)) {
            int status = response.getStatusLine().getStatusCode();
            if (status == 200) {
                return new JSONObject(EntityUtils.toString(response.getEntity()));
            }
            else {
                throw new HttpResponseException(status, response.getEntity().toString());
            }
        }
    }

    /**
     * check status of token
     * @param token token to check for
     * @return whether the token is still active (true or false)
     * @throws IOException
     */
    public Boolean checkTokenStatus(String token) throws IOException {
        return introspectToken(token).getBoolean("active");
    }

    /**
     * read content of files
     * @param filePath file path
     * @return content of the file
     * @throws IOException
     */
    private String readFromFile(String filePath) throws IOException {
        String content = null;
        try (DataInputStream reader = new DataInputStream(new FileInputStream(filePath))) {
            int nBytesToRead = reader.available();
            if(nBytesToRead > 0) {
                byte[] bytes = new byte[nBytesToRead];
                reader.read(bytes);
                content = new String(bytes);
            }
        }
        return content;
    }
}
