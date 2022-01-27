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

import org.apache.commons.validator.routines.UrlValidator;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.ontomatch.AlignmentReader;
import uk.ac.cam.cares.jps.ontomatch.CoordinationAgent;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher;

public class TestCoordinationAgent extends Mockito{
 
	@Ignore
	@Test
	public void testLoadTBox() {
		CoordinationAgent a =new CoordinationAgent();
		String from = "D:/workwork/ontoMatchFiles/tmpdbpAll.owl";
		String to = "D:/workwork/ontoMatchFiles/tmpdbpAllT.owl";
		String t = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
		try {
			a.loadTBOX(from, t, to);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Ignore
	@Test
	public void testRetrieveMap() {
		CoordinationAgent a =new CoordinationAgent();
		String aIRI = "http://localhost:3000/finalTestAggregaotr.owl";
        try {
			List[] maps = a.retrieveClassAlignmentMap(aIRI);
			System.out.println(maps[0]);
		} catch (ParseException | FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	}
	
	@Ignore
	@Test
	public void testRetrieveTriplesLocal() {
		CoordinationAgent a =new CoordinationAgent();
		String targetClassIRI = "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant";
		List targetClassIRIs = new ArrayList();
		targetClassIRIs.add(targetClassIRI);
		String ep = "http://localhost:8080/rdf4j-workbench/repositories/jpspowerplant/query";
		String saveIRI = "http://localhost:3000/jpspowerplants.rdf";
		try {
			a.queryPotentialInstanceAndSave(targetClassIRIs, ep, ep, false, saveIRI);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
	}


@Ignore
	@Test 
	public void testRetrieveTriplesRemote() {
		String ep = "http://dbpedia.org/sparql";
		CoordinationAgent a =new CoordinationAgent();
		String targetClassIRI = "http://dbpedia.org/ontology/PowerStation" ;
		String saveIRI =  "http://www.theworldavatar.com/tmpdbpAll.owl";
		String tIRI = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
		List IRIs = new ArrayList<String>();
		IRIs.add(targetClassIRI);
		
		try {
			a.queryPotentialInstanceAndSave(IRIs, ep,tIRI, true, saveIRI);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    
	}


	
    @Test
    public void testIndiCoordi() {//56x5000
        HttpServletRequest request = mock(HttpServletRequest.class);       
        HttpServletResponse response = mock(HttpServletResponse.class);
        System.out.println("test coordi");

		String mt = "INDIVIDUAL";
		String stubSavePath = "http://www.theworldavatar.com/finalPowerPlants1123.owl";
		//TODO: substitute with actual tmp file please
		String stubTBOXTarget = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		String stubTBOX= "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
		String stubTgt="D:/workwork/testFiles/ontologies/jpspp.rdf";
		String stubEP= "http://dbpedia.org/sparql";
		String classAlign = "http://www.theworldavatar.com/finalTestAggregaotr.owl";
		List<Double> weights = new ArrayList<Double>();
		weights.add(0.6);
		weights.add(0.3);
		weights.add(0.1);
		List<String> choices = new ArrayList<String>();
        choices.add("PENALIZING");
        choices.add("CARDINALITY");

		JSONObject jo  = new JSONObject();
        jo.put("aIRI", stubSavePath);
        jo.put("sourceIRI", stubEP);
        jo.put("targetIRI", stubTgt);
        jo.put("matchingType", mt);
        jo.put("choices", choices);
        jo.put("threshold", 0.55);
        jo.put("weights", weights);
        jo.put("classAlignment", classAlign);
        jo.put("sourceTBOX", stubTBOX);
        jo.put("targetTBOX", stubTBOXTarget);
        jo.put("pFactor", 0.6);
        jo.put("sameClassThrehold", 0.6);


		CoordinationAgent a =new CoordinationAgent();
		Reader inputString = new StringReader(jo.toString());
		BufferedReader reader = new BufferedReader(inputString);

		when(request.getMethod()).thenReturn("POST");
		try {
			when(request.getReader()).thenReturn(reader);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		JSONObject result = a.processRequestParameters(jo, request);
        assertTrue(result.has("success"));
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
