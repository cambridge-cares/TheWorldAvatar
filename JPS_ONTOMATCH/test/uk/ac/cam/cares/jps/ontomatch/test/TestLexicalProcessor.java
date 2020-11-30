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
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.ontomatch.LexicalProcessor;
import uk.ac.cam.cares.jps.ontomatch.test.TestCoordinationAgent.CoordinationAgentForTest;

public class TestLexicalProcessor extends Mockito{

	class LexicalProcessorForTest extends LexicalProcessor{
		 public void testGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			 super.testGet(request, response);
		 }
		}

	   
	    @Test 
	    public void testServlet() throws Exception {
	        HttpServletRequest request = mock(HttpServletRequest.class);       
	        HttpServletResponse response = mock(HttpServletResponse.class);


	    	String stubIRI = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
	    	String stubAddress = "D:/workwork/ontoMatchFiles/targetOntology.pkl";
	        JSONArray jaw = new JSONArray();
	        jaw.put(0.4);jaw.put(0.4);jaw.put(0.2);
			JSONObject jo  = new JSONObject();
	        jo.put("ontologyIRI", stubIRI);
	        jo.put("saveAddress", stubAddress);



	        Reader inputString = new StringReader(jo.toString());
	        BufferedReader reader = new BufferedReader(inputString);
	        
	        when(request.getMethod()).thenReturn("POST");
	        when(request.getReader()).thenReturn(reader);

	        StringWriter stringWriter = new StringWriter();
	        PrintWriter writer = new PrintWriter(stringWriter);
	        
	        when(response.getWriter()).thenReturn(writer);

	        new LexicalProcessorForTest().testGet(request, response);

	        writer.flush(); // it may not have been flushed yet...
	        assertTrue(stringWriter.toString().contains("success"));
	    }
	
	
}
