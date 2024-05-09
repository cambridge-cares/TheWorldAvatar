package uk.ac.cam.cares.jps.agent.carpark;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import com.github.tomakehurst.wiremock.junit5.WireMockTest;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

@WireMockTest (httpPort = 8089)
public class CarparkAPIConnectorTest {

    @TempDir
    File tempDir;

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String AVAILABLE_LOTS_TEST_URL = "http://localhost:" + PORT + "/AvailableLotsTesting";
    private static final String TEST_API_TOKEN = "testing";
    private static final String RATES_TEST_URL = "http://localhost:" + PORT + "/Rates";

    private CarparkAPIConnector testConnector;

    @BeforeEach
    public void initializeTestConnector() {
        testConnector = new CarparkAPIConnector(AVAILABLE_LOTS_TEST_URL, TEST_API_TOKEN, RATES_TEST_URL);
    }

    @Test
    public void carparkAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
        // One connector constructed using the api config properties directly
        CarparkAPIConnector connector = new CarparkAPIConnector(AVAILABLE_LOTS_TEST_URL, TEST_API_TOKEN, RATES_TEST_URL);
        // One connector constructed using a properties file
        File apiProperties = new File(tempDir, "api.properties");
        String propertiesFile = Paths.get(apiProperties.getAbsolutePath()).toString();
        writePropertyFile(propertiesFile, Arrays.asList("carpark.api.lot.endpoint="+AVAILABLE_LOTS_TEST_URL, "carpark.api.lot.token="+TEST_API_TOKEN, "carpark.api.pricing.endpoint="+RATES_TEST_URL));
        CarparkAPIConnector connectorFile = new CarparkAPIConnector(propertiesFile);

        // Retrieve private fields for api_key, stationId and api_url. Check that they were set correctly
        Field availableLotsEndpointField = CarparkAPIConnector.class.getDeclaredField("lotApiEndpoint");
        availableLotsEndpointField.setAccessible(true);
        Assert.assertEquals(AVAILABLE_LOTS_TEST_URL, availableLotsEndpointField.get(connector));
        Assert.assertEquals(AVAILABLE_LOTS_TEST_URL, availableLotsEndpointField.get(connectorFile));

        Field tokenField = CarparkAPIConnector.class.getDeclaredField("lotApiToken");
        tokenField.setAccessible(true);
        Assert.assertEquals(TEST_API_TOKEN, tokenField.get(connector));
        Assert.assertEquals(TEST_API_TOKEN, tokenField.get(connectorFile));

        Field pricingEndpointField = CarparkAPIConnector.class.getDeclaredField("pricingApiEndpoint");
        pricingEndpointField.setAccessible(true);
        Assert.assertEquals(RATES_TEST_URL, pricingEndpointField.get(connector));
        Assert.assertEquals(RATES_TEST_URL, pricingEndpointField.get(connectorFile));
    }

    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

    @Test
    public void testGetAvailableLots_Success() throws NoSuchFieldException, IllegalAccessException, JSONException, IOException {
        // API returns a response
        JSONObject responseBody = new JSONObject();
        JSONArray value = new JSONArray();
        JSONObject mockCarpark = new JSONObject();
        mockCarpark.put("CarparkID", "1");
        mockCarpark.put("Area", "Marina");
        mockCarpark.put("Development", "Suntec City");
        mockCarpark.put("Location", "1.29375 103.85718");
        mockCarpark.put("AvailableLots", 1310);
        mockCarpark.put("LotType", "C");
        mockCarpark.put("Agency", "LTA");
        value.put(mockCarpark);
        responseBody.put("value", value);
        stubFor(get(urlEqualTo("/AvailableLotsTesting")).withHeader("accountKey", containing(TEST_API_TOKEN))
                .willReturn(ok().withBody(responseBody.toString())));

        Assert.assertEquals(responseBody.toString(), testConnector.getAvailableLots().toString());
    }

    @Test
    public void testGetAvailableLots_Fail() throws NoSuchFieldException, IllegalAccessException, JSONException, IOException {
        // API returns a response
        JSONObject responseBody = new JSONObject();
        JSONArray value = new JSONArray();
        JSONObject mockCarpark = new JSONObject();
        mockCarpark.put("CarparkID", "1");
        mockCarpark.put("Area", "Marina");
        mockCarpark.put("Development", "Suntec City");
        mockCarpark.put("Location", "1.29375 103.85718");
        mockCarpark.put("AvailableLots", 1310);
        mockCarpark.put("LotType", "C");
        mockCarpark.put("Agency", "LTA");
        value.put(mockCarpark);
        responseBody.put("value", value);

        testConnector=new CarparkAPIConnector(AVAILABLE_LOTS_TEST_URL, "invalid-token", AVAILABLE_LOTS_TEST_URL); 
        stubFor(get(urlEqualTo("/AvailableLotsTesting")).withHeader("accountKey", containing(TEST_API_TOKEN))
                .willReturn(ok().withBody(responseBody.toString())));
        try {
            testConnector.getAvailableLots();
        } catch (Exception e) {
            Assert.assertEquals("Carpark available lots data could not be retrieved", e.getMessage());
        }
    }

    @Test
    public void testGetCarparkRates_Success() throws NoSuchFieldException, IllegalAccessException, JSONException, IOException {
        // API returns a response
        JSONObject responseBody = new JSONObject();
        JSONObject results = new JSONObject();

        JSONArray record = new JSONArray();
        JSONObject mockCarpark = new JSONObject();
        mockCarpark.put("carpark", "Suntec City");
        mockCarpark.put("category", "South & CBD");
        mockCarpark.put("weekdays_rate_1", "7am-5pm: $2.20 for 1st hr; $1.10 for sub. 1/2 hr (excluding PH)");
        mockCarpark.put("weekdays_rate_2", "5pm-12am:$2.20 per entry; Aft 12am:$1.10 per hr");
        mockCarpark.put("saturday_rate", "$1.10 per hr");
        mockCarpark.put("sunday_publicholiday_rate", "Same as Saturday");
        record.put(mockCarpark);
        results.put("results", record);
        responseBody.put("result", results);
        stubFor(get(urlEqualTo("/Rates"))
                .willReturn(ok().withBody(responseBody.toString())));

        Assert.assertEquals(responseBody.toString(), testConnector.getCarparkRates().toString());
    }

    @Test
    public void testGetCarparkRates_Fail() throws NoSuchFieldException, IllegalAccessException, JSONException, IOException {
        // API returns a response
        JSONObject responseBody = new JSONObject();
        testConnector=new CarparkAPIConnector(AVAILABLE_LOTS_TEST_URL, TEST_API_TOKEN, "invalid url"); 
        stubFor(get(urlEqualTo("/Rates"))
                .willReturn(ok().withBody(responseBody.toString())));
        try {
            testConnector.getCarparkRates();
        } catch (Exception e) {
            Assert.assertEquals("Carpark rates data could not be retrieved", e.getMessage());
        }
    }
}