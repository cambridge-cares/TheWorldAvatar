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

import uk.ac.cam.cares.jps.ontomatch.CoordinationAgent;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher;

public class TestCoordinationAgent extends Mockito{
	/**an workaround to test the protected HTTP method*/
	class CoordinationAgentForTest extends CoordinationAgent{
	 public void testGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		 super.testGet(request, response);
	 }
	}

    @Test 
    public void testTermCoordi() throws Exception {
        HttpServletRequest request = mock(HttpServletRequest.class);       
        HttpServletResponse response = mock(HttpServletResponse.class);


		String mt = "TERM";
		String stubSavePath = "C:/Users/morta/WebstormProjects/MatchAgentVisual/public/kb";
		//TODO: substitute with actual tmp file please
		String stubTgt = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		String stubSrc = "D://workwork//testFiles//ontologies/dbpedia_2014.owl";
        double[] jaw = new double[3];
        jaw[0] = 0.4;jaw[1] = 0.4;jaw[2] = 0.2;
		JSONObject jo  = new JSONObject();
        jo.put("aIRI", stubSavePath);
        jo.put("sourceIRI", stubSrc);
        jo.put("targetIRI", stubTgt);
        jo.put("matchingType", mt);
        jo.put("threshold", 0.6);
        jo.put("weights", jaw);
        jo.put("dictAddress", "D:\\workwork\\testFiles\\model\\dictionarylevel2.gensim");
        jo.put("modelAddress", "D:\\workwork\\testFiles\\model\\model30t5p5a.gensim");

        Reader inputString = new StringReader(jo.toString());
        BufferedReader reader = new BufferedReader(inputString);
        
        when(request.getMethod()).thenReturn("POST");
        when(request.getReader()).thenReturn(reader);

        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);
        
        when(response.getWriter()).thenReturn(writer);

        new CoordinationAgentForTest().testGet(request, response);

        writer.flush(); // it may not have been flushed yet...
        assertTrue(stringWriter.toString().contains("success"));
    }
}
