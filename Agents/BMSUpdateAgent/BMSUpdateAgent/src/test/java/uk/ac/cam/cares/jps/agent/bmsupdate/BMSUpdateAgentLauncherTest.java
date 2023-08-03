package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.json.JSONObject;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.nio.file.Paths;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper.readClientPropertiesTemplateFile;
import static uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper.writeToTempFolder;
import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

public class BMSUpdateAgentLauncherTest {
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

    static BMSUpdateAgentLauncher launcher;

    @BeforeClass
    public static void setUpBeforeClass() {
        launcher = new BMSUpdateAgentLauncher();
    }

    @Test
    public void validateInput() throws Exception {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock-Temperature");
        jsonObject.put("temperature", 26.0);
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> assertTrue(launcher.validateInput(jsonObject)));
    }

    @Test
    public void validateInput_EmptyRequestParam() {
        JSONObject jsonObject = new JSONObject();
        try {
            launcher.validateInput(jsonObject);
        } catch (JPSRuntimeException e) {
            assertEquals("The request param is empty", e.getMessage());
        }
    }

    @Test
    public void validateInput_DataIRIMissing() throws Exception {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("temperature", 26.0);
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> assertFalse(launcher.validateInput(jsonObject)));
    }

    @Test
    public void validateInput_TemperatureMissing() throws Exception {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock-Temperature");
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> assertFalse(launcher.validateInput(jsonObject)));
    }

    @Test
    public void validateInput_TemperatureNotDouble() throws Exception {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock-Temperature");
        jsonObject.put("temperature", "not double");
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> assertFalse(launcher.validateInput(jsonObject)));
    }

    @Test
    public void validateInput_PropertyFileMissing() throws Exception {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock-Temperature");
        jsonObject.put("temperature", 26.0);

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> assertFalse(launcher.validateInput(jsonObject)));
    }

    @Test
    public void validateInput_PropertyFileNotExist() {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock-Temperature");
        jsonObject.put("temperature", 26.0);
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");

        assertFalse(launcher.validateInput(jsonObject));

    }


    @Test
    public void testLauncherProcessRequestParam_MissingESPHomeHost() throws Exception {
        List<String> lines = readClientPropertiesTemplateFile(null, "esp_update_host", "blazegraph_query_host", "blazegraph_update_host");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> {
                    try {
                        launcher.processRequestParameters(getLauncherInput(26));
                    } catch (JPSRuntimeException e) {
                        assertEquals("Properties file is missing \"esphome.agent.toggle=<esphome_agent_toggle>\"", e.getMessage());
                    }
                });
    }

    @Test
    public void testLauncherProcessRequestParam_MissingESPUpdateHost() throws Exception {
        List<String> lines = readClientPropertiesTemplateFile("esp_home_host", null, "blazegraph_query_host", "blazegraph_update_host");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> {
                    try {
                        launcher.processRequestParameters(getLauncherInput(26));
                    } catch (JPSRuntimeException e) {
                        assertEquals("Properties file is missing \"esphome.update.agent.retrieve=<esphome_update_agent_retrieve>\"", e.getMessage());
                    }
                });
    }

    @Test
    public void testLauncherProcessRequestParam_MissingBlazegraphQueryHost() throws Exception {
        List<String> lines = readClientPropertiesTemplateFile("esp_home_host", "esp_update_host", null, "blazegraph_update_host");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> {
                    try {
                        launcher.processRequestParameters(getLauncherInput(26));
                    } catch (JPSRuntimeException e) {
                        assertEquals("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\"", e.getMessage());
                    }
                });
    }

    @Test
    public void testLauncherProcessRequestParam_MissingBlazegraphUpdateHost() throws Exception {
        List<String> lines = readClientPropertiesTemplateFile("esp_home_host", "esp_update_host", "blazegraph_query_host", null);
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);

        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> {
                    try {
                        launcher.processRequestParameters(getLauncherInput(26));
                    } catch (JPSRuntimeException e) {
                        assertEquals("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\"", e.getMessage());
                    }
                });
    }


    private JSONObject getLauncherInput(double temperature) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock_setpoint");
        jsonObject.put("temperature", temperature);
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");
        return jsonObject;
    }

}
