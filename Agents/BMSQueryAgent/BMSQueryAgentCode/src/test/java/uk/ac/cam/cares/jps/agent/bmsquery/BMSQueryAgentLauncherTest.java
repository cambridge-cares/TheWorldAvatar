package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.commons.collections.map.HashedMap;
import org.junit.Before;
import org.junit.Test;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class BMSQueryAgentLauncherTest {
    BMSQueryAgentLauncher launcher;

    @Before
    public void setup() throws IOException {
        launcher = new BMSQueryAgentLauncher();
    }

    @Test
    public void testDoGet_Status() throws IOException {
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(request.getRequestURI()).thenReturn("http://localhost:3838/bms-query-agent/status");
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);
        when(response.getWriter()).thenReturn(writer);


        launcher.doGet(request, response);
        writer.flush();
        assertEquals("{\"description\":\"BMSQueryAgent is ready.\"}" , stringWriter.toString());

    }



    @Test
    public void testValidateInput_ValidParameters() throws Exception {
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
