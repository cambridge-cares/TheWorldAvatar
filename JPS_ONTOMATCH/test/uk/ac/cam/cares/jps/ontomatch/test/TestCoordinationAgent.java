package uk.ac.cam.cares.jps.ontomatch.test;

import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.ontomatch.AlignmentReader;
import uk.ac.cam.cares.jps.ontomatch.CoordinationAgent;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher;

public class TestCoordinationAgent extends Mockito{
 
	@Test
	public void testAddTbox() {
		String saveAddr = "D:/workwork/ontoMatchFiles/jpsppcombine.rdf";
	    String tboxIRI = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
	    String aboxIRI = "D:/workwork/ontoMatchFiles/jpspp.rdf";
		CoordinationAgent a =new CoordinationAgent();
        try {
		
				a.addTBOX(tboxIRI, aboxIRI, saveAddr);

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
	
	@Ignore
	@Test 
	public void testRetrieveTriples() {
		String ep = "http://dbpedia.org/sparql";
		CoordinationAgent a =new CoordinationAgent();
		String targetClassIRI = "http://dbpedia.org/ontology/PowerStation" ;
		String saveIRI =  "http://www.theworldavatar.com/tmpdbp.owl";
		String tIRI = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
		String[] IRIs = {targetClassIRI}; 
		try {
			a.queryPotentialInstanceAndSave(IRIs, ep,tIRI, true, saveIRI);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    
	}
	
	@Ignore
    @Test 
    public void testTermCoordi() throws Exception {
        HttpServletRequest request = mock(HttpServletRequest.class);       
        HttpServletResponse response = mock(HttpServletResponse.class);
        System.out.println("test coordi");

		String mt = "TERM";
		String stubSavePath = "http://www.theworldavatar.com/final.owl";
		//TODO: substitute with actual tmp file please
		String stubTgt = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		String stubSrc = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
		List<Double> weights = new ArrayList<Double>();
		weights.add(0.4);
		weights.add(0.4);
		weights.add(0.2);
		JSONObject jo  = new JSONObject();
        jo.put("aIRI", stubSavePath);
        jo.put("sourceIRI", stubSrc);
        jo.put("targetIRI", stubTgt);
        jo.put("matchingType", mt);
        jo.put("threshold", 0.6);
        jo.put("weights", weights);
        jo.put("dictAddress", "D:/workwork/ontoMatchData/simMatch/model/modellevel2/dictionarylevel2.gensim");
        jo.put("modelAddress", "D:/workwork/ontoMatchData/simMatch/model/modellevel2/model30t5p5a.gensim");


		CoordinationAgent a =new CoordinationAgent();
		Reader inputString = new StringReader(jo.toString());
		BufferedReader reader = new BufferedReader(inputString);

		when(request.getMethod()).thenReturn("POST");
		when(request.getReader()).thenReturn(reader);
		JSONObject result = a.processRequestParameters(jo, request);
        assertTrue(result.has("success"));
    }
}
