package uk.ac.cam.cares.jps.agent.androidstatus;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import javax.servlet.http.HttpServletRequest;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AndroidStatusAgentTest {

    private static final Logger LOGGER = LogManager.getLogger(AndroidStatusAgentTest.class);
    private AndroidStatusAgent agent;

    @Before
    public void setup() {
        agent = new AndroidStatusAgent();
    }

    @Test
    public void testProcessRequestParameters_EmptyOutput() {
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(request.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/get");

        JSONObject input = new JSONObject();
        JSONObject result = agent.processRequestParameters(input, request);

        assertEquals("", result.getString("iri"));
    }

    @Test
    public void testProcessRequestParameters_NormalOutput() {
        HttpServletRequest setRequest = mock(HttpServletRequest.class);
        when(setRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/set?iri=http://www.example.com/test-element");

        JSONObject setInput = new JSONObject();
        setInput.put("iri", "http://www.example.com/test-element");
        agent.processRequestParameters(setInput, setRequest);

        HttpServletRequest getRequest = mock(HttpServletRequest.class);
        when(getRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/get");

        JSONObject result = agent.processRequestParameters(new JSONObject(), getRequest);
        assertEquals("http://www.example.com/test-element", result.getString("iri"));
    }

    @Test
    public void testProcessRequestParameters_EmptyIRIInput() {
        HttpServletRequest setRequest = mock(HttpServletRequest.class);
        when(setRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/set?iri=");

        JSONObject input = new JSONObject();
        input.put("iri", "");

        JSONObject result = agent.processRequestParameters(input, setRequest);
        assertEquals("Successfully set iri to ", result.getString("message"));
    }
}
