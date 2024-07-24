package uk.ac.cam.cares.jps.agent.pips;

import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;

import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class KeycloakConnectorTest {

    private static final String KEYCLOAK_HOST = "test_pips_request_agent-keycloak-1";
    private static final int KEYCLOAK_PORT = 7080;
    private static final String MOCK_CLIENT_SECRETS = "0GricBzRHrk2yjnzMwhM5ULRFKJC0JsR";
    private static final String MOCK_USERNAME = "test";
    private static final String MOCK_PASSWORD = "test";

    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

    @Before
    public void sleep() throws InterruptedException {
        TimeUnit.SECONDS.sleep(40);
    }

    /**
     * Write to file
     * @param filepath path of file to write to
     * @param text String of characters to write into file
     * @throws IOException
     */
    private void writeToFile(String filepath, String text) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        writer.write(text);
        // Close the file and return the file
        writer.close();
    }

    private List<String> setUpTempFiles() throws IOException {
        String usernameTempFile = Paths.get(folder.getRoot().toString(), "username.txt").toString();
        writeToFile(usernameTempFile,MOCK_USERNAME);
        String passwordTempFile = Paths.get(folder.getRoot().toString(), "password.txt").toString();
        writeToFile(passwordTempFile,MOCK_PASSWORD);
        String clientSecretsTempFile = Paths.get(folder.getRoot().toString(), "client_secrets.txt").toString();
        writeToFile(clientSecretsTempFile,MOCK_CLIENT_SECRETS);
        ArrayList<String> list = new ArrayList<String>();
        list.add(usernameTempFile);
        list.add(passwordTempFile);
        list.add(clientSecretsTempFile);
        return list;
    }
    

    @Test
    public void testKeycloakEndpoints() throws Exception {
        withEnvironmentVariable("KEYCLOAK_REALM_PATH", "http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm")
        .execute(() -> {
            KeycloakConnector keycloakConnector = new KeycloakConnector();
            Assert.assertEquals("http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm" + "/protocol/openid-connect/token", keycloakConnector.getTokenEndpoint());
            Assert.assertEquals("http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm" + "/protocol/openid-connect/token/introspect", keycloakConnector.getIntrospectionEndpoint());
            }
        );
    }

    @Test
    public void testGetToken() throws Exception {
        List<String> list = setUpTempFiles();
        withEnvironmentVariable("KEYCLOAK_REALM_PATH", "http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm").and("USERNAME", list.get(0)).and("PASSWORD", list.get(1))
        .and("CLIENT_ID", "testclient").and("CLIENT_SECRETS", list.get(2))
        .execute(() -> {
            KeycloakConnector keycloakConnector = new KeycloakConnector();
            JSONObject response = keycloakConnector.getTokens("password");
            String accessToken = response.getString("access_token");
            Assert.assertTrue(accessToken != null && accessToken.length() != 0);
            }
        );
    }

    @Test
    public void testCheckTokenStatus() throws Exception {
        List<String> list = setUpTempFiles();
        withEnvironmentVariable("KEYCLOAK_REALM_PATH", "http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm").and("USERNAME", list.get(0)).and("PASSWORD", list.get(1))
        .and("CLIENT_ID", "testclient").and("CLIENT_SECRETS", list.get(2))
        .execute(() -> {
            KeycloakConnector keycloakConnector = new KeycloakConnector();
            Assert.assertFalse(keycloakConnector.checkTokenStatus("abc"));

            JSONObject response = keycloakConnector.getTokens("password");
            String accessToken = response.getString("access_token");
            Assert.assertTrue(keycloakConnector.checkTokenStatus(accessToken));
            }
        );
    }

    @Test
    public void testRefreshToken() throws Exception {
        List<String> list = setUpTempFiles();
        withEnvironmentVariable("KEYCLOAK_REALM_PATH", "http://" + KEYCLOAK_HOST + ":" + KEYCLOAK_PORT + "/realms/testrealm").and("USERNAME", list.get(0)).and("PASSWORD", list.get(1))
        .and("CLIENT_ID", "testclient").and("CLIENT_SECRETS", list.get(2))
        .execute(() -> {
            KeycloakConnector keycloakConnector = new KeycloakConnector();
            JSONObject response = keycloakConnector.getTokens("password");
            String accessToken = response.getString("access_token");
            String refreshToken = response.getString("refresh_token");
            response = keycloakConnector.refreshToken(refreshToken);
            String newAccessToken = response.getString("access_token");
            Assert.assertTrue(accessToken != newAccessToken);
            Assert.assertTrue(keycloakConnector.checkTokenStatus(accessToken));
            }
        );
    }
}
