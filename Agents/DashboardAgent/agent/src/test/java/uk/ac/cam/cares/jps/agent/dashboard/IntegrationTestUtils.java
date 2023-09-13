package uk.ac.cam.cares.jps.agent.dashboard;

import com.google.gson.Gson;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class IntegrationTestUtils {
    public static final String TEST_DASHBOARD_URL = "http://172.27.0.2:3000";
    public static final String NAME_KEY = "name";
    public static final String SERVICE_ACCOUNT_NAME = "grafana";
    private static final HttpClient HTTP_CLIENT = HttpClient.newHttpClient();
    private static final String SERVICE_ACCOUNT_ROUTE = "/api/serviceaccounts";
    private static final String SERVICE_ACCOUNT_SEARCH_SUB_ROUTE = "/search";
    private static final String DATA_SOURCE_ROUTE = "/api/datasources";
    private static final String ID_KEY = "id";

    public static void deleteServiceAccounts(String user, String password) {
        String route = TEST_DASHBOARD_URL + SERVICE_ACCOUNT_ROUTE;
        List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) retrieveServiceAccounts(user, password);
        int accountId = -1;
        if (accountInfo.size() > 0 && accountInfo.get(0).get(NAME_KEY).equals(SERVICE_ACCOUNT_NAME)) {
            Double idDoubleFormat = (Double) accountInfo.get(0).get(ID_KEY);
            accountId = idDoubleFormat.intValue();
        }
        if (accountId != -1) {
            sendDeleteRequest(route + "/" + accountId, user, password);
        }
    }

    public static Object retrieveServiceAccounts(String user, String password) {
        String route = TEST_DASHBOARD_URL + SERVICE_ACCOUNT_ROUTE;
        // Send a get request to target API for a response
        HttpResponse response = sendGetRequest(route + SERVICE_ACCOUNT_SEARCH_SUB_ROUTE, user, password);
        Map<String, Object> responseMap = retrieveResponseBodyAsMap(response);
        // Response will always contain service accounts key, even if there is no account
        return responseMap.get("serviceAccounts");
    }

    public static Object retrieveDataSources(String user, String password) {
        String route = TEST_DASHBOARD_URL + DATA_SOURCE_ROUTE;
        // Send a get request to target API for a response
        HttpResponse response = sendGetRequest(route, user, password);
        Map<String, Object> responseMap = retrieveResponseBodyAsMap(response);
        return responseMap;
    }

    public static Map<String, Object> retrieveResponseBodyAsMap(HttpResponse response) {
        Gson gson = new Gson();
        // Although the method returns the result as a HashMap, it can be abstracted to its parent Map object
        return gson.fromJson(response.body().toString(), HashMap.class);
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