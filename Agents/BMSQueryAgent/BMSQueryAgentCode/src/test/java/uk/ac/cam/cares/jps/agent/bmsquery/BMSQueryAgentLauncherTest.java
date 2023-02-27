package uk.ac.cam.cares.jps.agent.bmsquery;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;

import static com.github.stefanbirkner.systemlambda.SystemLambda.withEnvironmentVariable;
import static org.junit.Assert.*;
import static org.junit.Assert.assertFalse;

public class BMSQueryAgentLauncherTest {
    BMSQueryAgentLauncher launcher;

    private final String CLIENT_PROPERTIES_FILE_NAME = "client.properties";
    private final String CLIENT_PROPERTIES_ENV_VAR = "CLIENT_PROPERTIES";

    @Before
    public void setup() throws IOException {
        launcher = new BMSQueryAgentLauncher();
    }

    @Ignore("Unable to test valid input for ProcessRequestParameter, because it needs to access the RDB which should belong to the integration test")
    @Test
    public void testProcessRequestParameters_ValidParameters() {
        JSONObject testInput = new JSONObject();
        testInput.put("dataIRI", "http://example.com/example_iri");
        testInput.put("clientProperties", CLIENT_PROPERTIES_FILE_NAME);
    }

    @Test
    public void testProcessRequestParameters_InvalidParametersKey() {
        JSONObject testInput = new JSONObject();
        testInput.put("data", "http://example.com/example_iri");
        testInput.put("clientProperties", CLIENT_PROPERTIES_FILE_NAME);

        try {
            launcher.processRequestParameters(testInput);
        } catch (JPSRuntimeException e) {
            assertEquals(BMSQueryAgentLauncher.PARAMETERS_VALIDATION_ERROR_MSG, e.getMessage());
        }

    }

    /**
     * This test method does not validate the existence of properties file. It only checks whether the properties file is set in the environment variables.
     * The check of file existence is done in TimeSeriesClient initialization, which is called in launcher.initializeAgent() and is not tested in this unit test file.
     */
    @Test
    public void testValidateInput_ValidParameters() throws Exception {
        JSONObject testInput = new JSONObject();
        testInput.put("dataIRI", "http://example.com/example_iri");
        testInput.put("clientProperties", CLIENT_PROPERTIES_ENV_VAR);

        boolean validateResult = withEnvironmentVariable(CLIENT_PROPERTIES_ENV_VAR, CLIENT_PROPERTIES_FILE_NAME)
                .execute(() -> launcher.validateInput(testInput));
        assertTrue(validateResult);
    }

    @Test
    public void testValidateInput_EmptyRequest() {
        JSONObject testInput = new JSONObject();

        assertFalse(launcher.validateInput(testInput));
    }

    @Test
    public void testValidateInput_MissingDataIRI() {
        JSONObject testInput = new JSONObject();
        testInput.put("clientProperties", CLIENT_PROPERTIES_FILE_NAME);

        assertFalse(launcher.validateInput(testInput));
    }

    @Test
    public void testValidateInput_MissingClientProperty() {
        JSONObject testInput = new JSONObject();
        testInput.put("dataIRI", "http://example.com/example_iri");

        assertFalse(launcher.validateInput(testInput));
    }

    @Test
    public void testValidateInput_ClientPropertyNotFound() {
        JSONObject testInput = new JSONObject();
        testInput.put("dataIRI", "http://example.com/example_iri");
        testInput.put("clientProperties", "not_exist_file_path");

        assertFalse(launcher.validateInput(testInput));
    }}
