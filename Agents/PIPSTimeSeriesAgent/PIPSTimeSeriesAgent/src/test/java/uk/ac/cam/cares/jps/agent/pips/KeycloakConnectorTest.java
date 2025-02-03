package uk.ac.cam.cares.jps.agent.pips;

import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;

import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

public class KeycloakConnectorTest {

    private static final String KEYCLOAK_HOST = "test_pips_timeseries_agent-keycloak-1";
    private static final int KEYCLOAK_PORT = 7080;
    private static final String MOCK_CLIENT = "testclient";
    private static final String MOCK_TOKEN_CLIENT = "tokenclient";
    private static final String MOCK_TOKEN_CLIENT_SECRETS = "ckhtvLJJB2GZTOzVkmRgSFCSTN4fayfd";
    private static final String MOCK_USERNAME = "test";
    private static final String MOCK_PASSWORD = "test";
    private static final String MOCK_USERNAME_02 = "user_02";
    private static final String MOCK_PASSWORD_02 = "user_02_password";



    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

        /**
     * get access and refresh tokens from keycloak token endpoint
     * @return JSONObject containing results of request
     * @throws IOException
     */
    private JSONObject getTokensViaCredentials(String tokenEndpoint, String username, String password, String client_id) throws IOException {
        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(tokenEndpoint);
        String requestBody;
        String client_secrets = MOCK_TOKEN_CLIENT_SECRETS;
        // Construct the request body
        requestBody = "grant_type=password"
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
     * Successful test of KeycloakConnector.getEndpoints
     * test retrieval of keycloak token and introspection endpoints
     * @throws Exception
     */
    @Test
    public void testKeycloakEndpoints() throws Exception {
        TimeUnit.SECONDS.sleep(40);
        withEnvironmentVariable("KEYCLOAK_REALM_PATH", "http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm")
        .execute(() -> {
            KeycloakConnector keycloakConnector = new KeycloakConnector();
            Assert.assertEquals("http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm" + "/protocol/openid-connect/token", keycloakConnector.getTokenEndpoint());
            Assert.assertEquals("http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm" + "/protocol/openid-connect/token/introspect", keycloakConnector.getIntrospectionEndpoint());
            }
        );
    }

    /**
     * Test of KeycloakConnector.checkAuthorizationViaUMA()
     * @throws Exception
     */
    @Test
    public void testCheckAuthorizationViaUMA() throws Exception {
        // Retrieve token for test user from tokenclient, this user has authorization to access testclient
        // Check token for authorization to access testclient
        withEnvironmentVariable("KEYCLOAK_REALM_PATH", "http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm")
        .and("CLIENT_ID", MOCK_CLIENT)
        .execute(() -> {
            KeycloakConnector keycloakConnector = new KeycloakConnector();
            JSONObject response = getTokensViaCredentials(keycloakConnector.getTokenEndpoint(), MOCK_USERNAME, MOCK_PASSWORD, MOCK_TOKEN_CLIENT);
            String accessToken = response.getString("access_token");
            response = keycloakConnector.checkAuthorizationViaUMA(accessToken);
            
            Assert.assertTrue(response.has("access_token") && response.getString("access_token").length() > 1);
            }
        );

        // Retrieve token for user_02 from tokenclient, this user has no authorization to access testclient
        // Check token for authorization to access testclient
        withEnvironmentVariable("KEYCLOAK_REALM_PATH", "http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm")
        .and("CLIENT_ID", MOCK_CLIENT)
        .execute(() -> {
            KeycloakConnector keycloakConnector = new KeycloakConnector();
            
            JSONObject response = getTokensViaCredentials(keycloakConnector.getTokenEndpoint(), MOCK_USERNAME_02, MOCK_PASSWORD_02, MOCK_TOKEN_CLIENT);
            String accessToken = response.getString("access_token");
            try {
                response = keycloakConnector.checkAuthorizationViaUMA(accessToken);
            } catch (Exception e) {
                Assert.assertTrue(e.toString().contains("\"error\":\"access_denied\",\"error_description\":\"not_authorized\""));
            }
        }
        );
    }
}