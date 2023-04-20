package uk.ac.cam.cares.jps.agent.androidstatus;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import static org.junit.Assert.assertEquals;

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

        assertEquals("", result.getString("equipmentIRI"));
    }

    @Test
    public void testProcessRequestParameters_NormalOutput() {
        HttpServletRequest setRequest = mock(HttpServletRequest.class);
        when(setRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/set?equipmentIRI=http://www.example.com/test-element");

        JSONObject setInput = new JSONObject();
        setInput.put("equipmentIRI", "http://www.example.com/test-element");
        agent.processRequestParameters(setInput, setRequest);

        HttpServletRequest getRequest = mock(HttpServletRequest.class);
        when(getRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/get");

        JSONObject result = agent.processRequestParameters(new JSONObject(), getRequest);
        assertEquals("http://www.example.com/test-element", result.getString("equipmentIRI"));
    }

//    @Test
//    public void testDoPost_DoGet_NormalOutput() throws IOException, ServletException {
//        HttpServletRequest setRequest = mock(HttpServletRequest.class);
//        HttpServletResponse setResponse = mock(HttpServletResponse.class);
//        when(setRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/set?equipmentIRI=http://www.example.com/test-element");
//        when(setRequest.getParameter("equipmentIRI")).thenReturn("http://www.example.com/test-element");
//
//        agent.doPost(setRequest, setResponse);
//
//        HttpServletRequest getRequest = mock(HttpServletRequest.class);
//        HttpServletResponse getResponse = mock(HttpServletResponse.class);
//        when(getRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/get");
//        StringWriter stringWriter = new StringWriter();
//        PrintWriter writer = new PrintWriter(stringWriter);
//        when(getResponse.getWriter()).thenReturn(writer);
//
//        agent.doGet(getRequest, getResponse);
//        JSONObject result = new JSONObject(stringWriter.toString());
//        assertEquals("http://www.example.com/test-element", result.getString("equipmentIRI"));
//    }

    @Test
    public void testProcessRequestParameters_EmptyIRIInput() {
        HttpServletRequest setRequest = mock(HttpServletRequest.class);
        when(setRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/set?equipmentIRI=");

        JSONObject input = new JSONObject();
        input.put("equipmentIRI", "");

        JSONObject result = agent.processRequestParameters(input, setRequest);
        assertEquals("Successfully set equipment iri to ", result.getString("message"));
    }

//    @Test
//    public void testDoPost_EmptyValue() throws ServletException, IOException {
//        HttpServletRequest setRequest = mock(HttpServletRequest.class);
//        HttpServletResponse setResponse = mock(HttpServletResponse.class);
//        when(setRequest.getRequestURI()).thenReturn("http://localhost:3838/android-status-agent/set?equipmentIRI=");
//        when(setRequest.getParameter("equipmentIRI")).thenReturn("");
//
//        try {
//            agent.doPost(setRequest, setResponse);
//        } catch (JPSRuntimeException e) {
//            assertEquals("Unable to validate request sent to the agent.", e.getMessage());
//        }
//
//    }
}
