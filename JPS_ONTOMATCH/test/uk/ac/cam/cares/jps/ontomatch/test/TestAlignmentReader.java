package uk.ac.cam.cares.jps.ontomatch.test;

import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.ontomatch.AlignmentReader;
import uk.ac.cam.cares.jps.ontomatch.CoordinationAgent;
import uk.ac.cam.cares.jps.ontomatch.test.TestCoordinationAgent.CoordinationAgentForTest;

public class TestAlignmentReader extends Mockito{
	/**an workaround to test the protected HTTP method*/
	class AlignmentReaderForTest extends AlignmentReader{
	 public void testGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		 super.testGet(request, response);
	 }
	}
	  @Test 
	    public void testTermCoordi() throws Exception {
	        HttpServletRequest request = mock(HttpServletRequest.class);       
	        HttpServletResponse response = mock(HttpServletResponse.class);
            //TODO:later make this suitable for another set up
			String alignmentFileIRI = "http://localhost:3000/a.owl";
			JSONObject jo  = new JSONObject();
			jo.put("alignmentIRI", alignmentFileIRI);
	        jo.put("threshold", 0.0);


	        Reader inputString = new StringReader(jo.toString());
	        BufferedReader reader = new BufferedReader(inputString);
	        
	        when(request.getMethod()).thenReturn("POST");
	        when(request.getReader()).thenReturn(reader);

	        StringWriter stringWriter = new StringWriter();
	        PrintWriter writer = new PrintWriter(stringWriter);
	        
	        when(response.getWriter()).thenReturn(writer);

	        new AlignmentReaderForTest().testGet(request, response);

	        writer.flush(); // it may not have been flushed yet...
	        assertTrue(stringWriter.toString().contains("alignmentlist"));
	    }
	
}
