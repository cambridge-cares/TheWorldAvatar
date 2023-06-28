package uk.ac.cam.cares.jps.agent.ifcowlconverter;

import be.ugent.IfcSpfReader;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import static org.junit.jupiter.api.Assertions.*;

class IfcOwlConverterTest {
    private static IfcOwlConverterAgent agent;
    private static final String KEY_BASEURI = "uri";

    @BeforeEach
    void setup() {agent = new IfcOwlConverterAgent();}

    @Test
    void testProcessRequestParameters() {
        JSONObject requestParams = new JSONObject();
        String uri = "http://www.theworldavatar.com/ifc/building/";
        requestParams.put(KEY_BASEURI, uri);
        // Run the entire method and sub-method within same class, and mock external classes
        try (MockedStatic<IfcSpfReader> mockReader = Mockito.mockStatic(IfcSpfReader.class)){
            // Stub the static method to do nothing when it is called
            mockReader.when(()-> IfcSpfReader.main(Mockito.any())).thenAnswer((Answer<Void>) invocation -> null);;
            JSONObject testMessage = agent.processRequestParameters(requestParams);
            // Verify the parameters are set accurately
            assertEquals(uri, agent.getBaseURI());
            // Verify the response
            String expected = "IfcOwl conversion is successfully completed!";
            assertTrue(testMessage.toString().contains(expected));
        }
    }

    @Test
    void testProcessRequestParametersDefaultUri() {
        JSONObject requestParams = new JSONObject();
        String uri = "default";
        requestParams.put(KEY_BASEURI, uri);
        // Run the entire method and sub-method within same class, and mock external classes
        try (MockedStatic<IfcSpfReader> mockReader = Mockito.mockStatic(IfcSpfReader.class)){
            // Stub the method to do nothing when it is called
            mockReader.when(()-> IfcSpfReader.main(Mockito.any())).thenAnswer((Answer<Void>) invocation -> null);;
            agent.processRequestParameters(requestParams);
            // Verify the parameters are properly set
            assertTrue(agent.getBaseURI().contains("http://www.theworldavatar.com/ifc/resources_"));
        }
    }

    @Test
    void testProcessRequestParametersEmptyParams() {
        JSONObject requestParams = new JSONObject();
        JSONObject testMessage = agent.processRequestParameters(requestParams);
        String expected = "Request parameters for the IfcOwlConverter are not defined correctly.";
        assertTrue(testMessage.toString().contains(expected));
    }

    @Test
    void testValidateInput() {
        // Test both variations of accepted uris
        JSONObject requestParams = new JSONObject();
        String uri = "http://www.theworldavatar.com/";
        requestParams.put(KEY_BASEURI, uri);
        assertTrue(agent.validateInput(requestParams));

        requestParams = new JSONObject();
        uri = "https://www.theworldavatar.com/ifc/bim#";
        requestParams.put(KEY_BASEURI, uri);
        assertTrue(agent.validateInput(requestParams));

        // Test default uri passed
        requestParams = new JSONObject();
        uri = "default";
        requestParams.put(KEY_BASEURI, uri);
        assertTrue(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputFail() {
        // Test if the string does not start with http://www. , it returns false
        JSONObject requestParams = new JSONObject();
        String uri = "www.theworldavatar.com/";
        requestParams.put(KEY_BASEURI, uri);
        assertFalse(agent.validateInput(requestParams));

        // Test if the string does not end with / or #, it returns false
        requestParams = new JSONObject();
        uri = "https://www.theworldavatar.com/ifc";
        requestParams.put(KEY_BASEURI, uri);
        assertFalse(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputForEmptyParams() {
        JSONObject requestParams = new JSONObject();
        assertFalse(agent.validateInput(requestParams));
    }
}