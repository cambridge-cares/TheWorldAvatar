package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.commons.collections.map.HashedMap;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class BMSQueryAgentLauncherTest {
    BMSQueryAgentLauncher launcher;

    @Before
    public void setup() {
        launcher = new BMSQueryAgentLauncher();
    }

    @Test
    public void testProcessRequestParameters_Status() {
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(request.getRequestURI()).thenReturn("http://localhost:3838/bms-query-agent/status");

        JSONObject result = launcher.processRequestParameters(new JSONObject(), request);
        assertEquals("BMSQueryAgent is ready." , result.getString("description"));
    }

    @Test
    public void testValidateInput_ValidParameters() {
        HttpServletRequest request = mock(HttpServletRequest.class);

        HashedMap paramMap = new HashedMap() ;
        paramMap.put(BMSQueryAgentLauncher.KEY_ROOMIRI, new String[] {"http://example.com/example_iri"});

        when(request.getParameterMap()).thenReturn(paramMap);
        when(request.getParameter(BMSQueryAgentLauncher.KEY_ROOMIRI)).thenReturn("http://example.com/example_iri");

        assertTrue(launcher.validateInput(request));
    }

    @Test
    public void testValidateInput_EmptyRequest() {
        HttpServletRequest request = mock(HttpServletRequest.class);

        assertFalse(launcher.validateInput(request));
    }

}
