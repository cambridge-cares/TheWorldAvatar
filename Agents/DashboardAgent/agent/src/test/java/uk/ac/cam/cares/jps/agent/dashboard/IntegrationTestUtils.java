package uk.ac.cam.cares.jps.agent.dashboard;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.google.gson.*;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.Duration;
import java.util.*;

public class IntegrationTestUtils {
    // Local host for docker containers is at 172.17.0.1, ports correspond to the expose ports in docker-compose.test.yml
    public static final String DOCKER_HOST_NAME = "172.17.0.1";
    public static final String SPARQL_ENDPOINT = "http://" + DOCKER_HOST_NAME + ":9998/blazegraph/";
    public static final BlazegraphEndpointConfig SPARQL_ENDPOINT_CONFIG = new BlazegraphEndpointConfig("blazegraph", DOCKER_HOST_NAME, "9998", "", null);
    public static final String SPATIAL_ZONE_NAMESPACE = "zone";
    public static final String SPATIAL_ZONE_SPARQL_ENDPOINT = SPARQL_ENDPOINT + "namespace/" + SPATIAL_ZONE_NAMESPACE + "/sparql";
    public static final String GENERAL_NAMESPACE = "asset";
    public static final String GENERAL_SPARQL_ENDPOINT = SPARQL_ENDPOINT + "namespace/" + GENERAL_NAMESPACE + "/sparql";
    public static final String TEST_POSTGIS_JDBC = "jdbc:postgresql://" + DOCKER_HOST_NAME + ":5431/";
    public static final String TEST_POSTGIS_USER = "postgres";
    public static final String TEST_POSTGIS_PASSWORD = "pg123";
    public static final String TEST_POSTGIS_PASSWORD_PATH = "postgis_password.txt";
    public static final PostGISEndpointConfig POSTGIS_ENDPOINT_CONFIG = new PostGISEndpointConfig("postgis", DOCKER_HOST_NAME, "5431", TEST_POSTGIS_USER, TEST_POSTGIS_PASSWORD_PATH);
    public static final String NAME_KEY = "name";
    public static final String SERVICE_ACCOUNT_NAME = "grafana";
    public static final String SERVICE_ACCOUNT_SEARCH_SUB_ROUTE = "/search";
    public static final String DASHBOARD_ROUTE = "/api/dashboards/uid/";
    public static final String DASHBOARD_ACCOUNT_USER = "admin";
    public static final String DASHBOARD_ACCOUNT_PASS = "admin";
    public static final String TEST_DASHBOARD_PASSWORD_PATH = "grafana_password.txt";
    public static final GrafanaEndpointConfig DASHBOARD_ENDPOINT_CONFIG = new GrafanaEndpointConfig("grafana", DOCKER_HOST_NAME, "3068", DASHBOARD_ACCOUNT_USER, TEST_DASHBOARD_PASSWORD_PATH);
    public static final String SPARQL_DELETE = "DELETE WHERE {?s ?p ?o}";
    private static final HttpClient HTTP_CLIENT = HttpClient.newHttpClient();
    private static final String ID_KEY = "id";

    public static void createNamespace(String namespace) {
        // Generate XML properties for request
        String payload =
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" +
                        "<!DOCTYPE properties SYSTEM \"http://java.sun.com/dtd/properties.dtd\">" +
                        "<properties>" +
                        "  <entry key=\"com.bigdata.rdf.sail.truthMaintenance\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.textIndex\">false</entry>" +
                        "  <entry key=\"com.bigdata.namespace." + namespace + ".lex.com.bigdata.btree.BTree.branchingFactor\">400</entry>" +
                        "  <entry key=\"com.bigdata.namespace." + namespace + ".spo.com.bigdata.btree.BTree.branchingFactor\">1024</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.justify\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.axiomsClass\">com.bigdata.rdf.axioms.NoAxioms</entry>" +
                        "  <entry key=\"com.bigdata.rdf.sail.namespace\">" + namespace + "</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.quads\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.geoSpatial\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.sail.isolatableIndices\">false</entry>" +
                        "</properties>";
        StringEntity configEntity = new StringEntity(payload, ContentType.create("application/xml", "UTF-8"));
        // Create a new post request
        HttpPost request = new HttpPost(SPARQL_ENDPOINT + "namespace");
        request.setHeader("Accept", "application/xml");
        request.addHeader("Content-Type", "application/xml");
        request.setEntity(configEntity);
        // Send request
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            httpClient.execute(request);
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to create namespace: " + e.getMessage());
        }
    }

    public static void updateEndpoint(String endpoint, String updateQuery) {
        try (RDFConnection conn = RDFConnection.connect(endpoint)) {
            UpdateRequest update = UpdateFactory.create(updateQuery);
            conn.update(update);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to update queries at SPARQL endpoint: " + e.getMessage());
        }
    }

    public static Object retrieveServiceAccounts(String user, String password) {
        String route = DASHBOARD_ENDPOINT_CONFIG.getServiceAccountServiceUrl();
        // Send a get request to target API for a response
        HttpResponse response = sendGetRequest(route + SERVICE_ACCOUNT_SEARCH_SUB_ROUTE, user, password);
        // Will always be a hash map
        Map<String, Object> responseMap = (Map<String, Object>) retrieveResponseBody(response);
        // Response will always contain service accounts key, even if there is no account
        return responseMap.get("serviceAccounts");
    }

    public static JsonArray retrieveDataSources(String user, String password) {
        String route = DASHBOARD_ENDPOINT_CONFIG.getDataSourceServiceUrl();
        // Send a get request to target API for a response
        HttpResponse response = sendGetRequest(route, user, password);
        JsonArray responseMap = (JsonArray) retrieveResponseBody(response);
        return responseMap;
    }

    public static Object retrieveDashboard(String uid, String user, String password) {
        String route = DASHBOARD_ENDPOINT_CONFIG.getServiceUrl() + DASHBOARD_ROUTE + uid;
        // Send a get request to target API for a response
        HttpResponse response = sendGetRequest(route, user, password);
        // Will always be a hash map
        Map<String, Object> responseMap = (Map<String, Object>) retrieveResponseBody(response);
        // Response will always contain service accounts key, even if there is no account
        return responseMap.get("dashboard");
    }

    public static void deleteServiceAccounts() {
        String route = DASHBOARD_ENDPOINT_CONFIG.getServiceAccountServiceUrl();
        List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) retrieveServiceAccounts(DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
        int accountId = -1;
        if (accountInfo.size() > 0) {
            Double idDoubleFormat = (Double) accountInfo.get(0).get(ID_KEY);
            accountId = idDoubleFormat.intValue();
        }
        if (accountId != -1) {
            sendDeleteRequest(route + "/" + accountId, DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
        }
    }

    public static void deleteDataSources() {
        List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) retrieveServiceAccounts(DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
        int accountId = -1;
        if (accountInfo.size() > 0 && accountInfo.get(0).get(NAME_KEY).equals(SERVICE_ACCOUNT_NAME)) {
            Double idDoubleFormat = (Double) accountInfo.get(0).get(ID_KEY);
            accountId = idDoubleFormat.intValue();
        }
        if (accountId != -1) {
            String route = DASHBOARD_ENDPOINT_CONFIG.getDataSourceServiceUrl();
            HttpResponse response = sendGetRequest(route, DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
            JsonArray dataSources = (JsonArray) retrieveResponseBody(response);
            for (JsonElement dataSource : dataSources) {
                if (dataSource.isJsonObject()) {
                    JsonObject jsonObject = dataSource.getAsJsonObject();
                    String id = jsonObject.get("uid").getAsString();
                    sendDeleteRequest(route + "/uid/" + id, DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
                }
            }
        }
    }

    public static void deleteDashboard(String uid) {
        String route = DASHBOARD_ENDPOINT_CONFIG.getServiceUrl() + DASHBOARD_ROUTE + uid;
        sendDeleteRequest(route, DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
    }

    public static Connection connectDatabase(String jdbc) {
        try {
            return DriverManager.getConnection(jdbc, TEST_POSTGIS_USER, TEST_POSTGIS_PASSWORD);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to connect to database: " + e.getMessage());
        }
    }

    public static void updateDatabase(Connection connection, String query) {
        try (Statement statement = connection.createStatement()) {
            statement.executeUpdate(query);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to execute updates: " + e.getMessage());
        }
    }

    public static void createPasswordFile(String path, String password) {
        try {
            Files.write(Paths.get(path), password.getBytes());
        } catch (IOException ex) {
            throw new IllegalArgumentException("Failed to write the password file.", ex);
        }
    }

    public static void deletePasswordFile(String filePath) {
        Path path = Paths.get(filePath);
        // Only delete if there is a file
        if (Files.exists(path)) {
            try {
                Files.delete(path);
            } catch (IOException ex) {
                throw new RuntimeException("Error deleting password file " + ex.getMessage());
            }
        }
    }

    public static Object retrieveResponseBody(HttpResponse response) {
        JsonElement jsonResponse = JsonParser.parseString(response.body().toString());
        // When the response is a JSON Object
        if (jsonResponse.isJsonObject()) {
            Gson gson = new Gson();
            // Return a hashmap parsed from it
            return gson.fromJson(response.body().toString(), HashMap.class);
        } else {
            // When the response is a JSON array, return a JSON array
            return jsonResponse.getAsJsonArray();
        }
    }

    public static HttpResponse sendGetRequest(String url, String userName, String password) {
        try {
            HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .GET()
                    .timeout(Duration.ofSeconds(3600));
            // If username and password are provided, add the authentication
            if (!userName.isEmpty() && !password.isEmpty())
                requestBuilder.header("Authorization", getBasicAuthenticationHeader(userName, password));
            // Await response before continue executing the rest of the code
            return HTTP_CLIENT.send(requestBuilder.build(), HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
        } catch (InterruptedException e) {
            throw new JPSRuntimeException("Thread has been interrupted! " + e.getMessage());
        }
    }

    private static HttpResponse sendDeleteRequest(String url, String userName, String password) {
        try {
            HttpRequest requestBuilder = HttpRequest.newBuilder()
                    .header("Authorization", getBasicAuthenticationHeader(userName, password))
                    .uri(URI.create(url))
                    .DELETE()
                    .timeout(Duration.ofSeconds(3600))
                    .build();
            // Await response before continue executing the rest of the code
            return HTTP_CLIENT.send(requestBuilder, HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
        } catch (InterruptedException e) {
            throw new JPSRuntimeException("Thread has been interrupted! " + e.getMessage());
        }
    }

    private static String getBasicAuthenticationHeader(String username, String password) {
        String valueToEncode = username + ":" + password;
        return "Basic " + Base64.getEncoder().encodeToString(valueToEncode.getBytes());
    }
}