package uk.ac.cam.cares.jps.agent.pips;

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

    // error messages
    private static final String GET_ENDPOINTS_ERROR = "Unable to get keycloak endpoints!";

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
            String keycloakRealmPath = System.getenv("KEYCLOAK_REALM_PATH");
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

    // Getter for tokenEndpoint
    public String getTokenEndpoint() {
        return tokenEndpoint;
    }
        
    // Getter for introspectionEndpoint
    public String getIntrospectionEndpoint() {
        return introspectionEndpoint;
    }

    /**
     * verify Authorization via UMA protocol by sending the token to the keycloak token endpoint
     * @return JSONObject containing results of request
     * @throws IOException
     */
    public JSONObject checkAuthorizationViaUMA(String token) throws IOException {
        String client_id = System.getenv("CLIENT_ID");
        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(tokenEndpoint);
        
        // Construct the request body
        String requestBody = "grant_type=urn:ietf:params:oauth:grant-type:uma-ticket"
                + "&audience=" + client_id;

        // Set headers
        httpPost.setHeader("Content-Type", "application/x-www-form-urlencoded");

        // Set request body and bearer token
        StringEntity entity = new StringEntity(requestBody, ContentType.APPLICATION_FORM_URLENCODED);
        httpPost.setEntity(entity);
        setTokenAuthorization(httpPost, token);
        LOGGER.info("The post request is " + httpPost.toString());
        try (CloseableHttpResponse response = httpClient.execute(httpPost)) {
            int status = response.getStatusLine().getStatusCode();
            if (status == 200) {
                return new JSONObject(EntityUtils.toString(response.getEntity()));
            }
            else {
                throw new HttpResponseException(status, EntityUtils.toString(response.getEntity()));
            }
        }
    }

    /**
     * Sets the current token as authorization in the header of the request
     * @param request The request to which to add the token authorization
     */
    private void setTokenAuthorization(HttpRequest request, String accessToken) {
        String authHeader = "Bearer " + accessToken;
        request.setHeader(HttpHeaders.AUTHORIZATION, authHeader);
    }
}
