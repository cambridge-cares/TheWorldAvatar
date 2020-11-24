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

import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher.MATCHERTYPE;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher.MATCHING_TYPE;


public class TestElementMatcher extends Mockito{

   
    @Test 
    public void testValueMatcher() throws Exception {
        HttpServletRequest request = mock(HttpServletRequest.class);       
        HttpServletResponse response = mock(HttpServletResponse.class);
		String type = "VALUE";
		String mt = "INDIVIDUAL";
		String stubSavePath = "D:/workwork/ontoMatchFiles/PPDBVALUEINDI.ttl";
		//TODO: substitute with actual tmp file please
		String stubTgt = "D:/workwork/ontoMatchFiles/jpspp.rdf";
		String stubSrc = "D:/workwork/ontoMatchFiles/tmpdbp.owl";
        JSONObject jo  = new JSONObject();
        jo.put("alignmentFileAddress", stubSavePath);
        jo.put("targetOntoIRI", stubTgt);
        jo.put("sourceOntoIRI", stubSrc);
        jo.put("matcherType", type);
        jo.put("matchingType", mt);


        Reader inputString = new StringReader(jo.toString());
        BufferedReader reader = new BufferedReader(inputString);
        
        when(request.getMethod()).thenReturn("POST");
        when(request.getReader()).thenReturn(reader);

        
        JSONObject result  = new ElementMatcher().processRequestParameters(jo, request);
        if(result.has("error")) {
        	System.out.println(result.getString("error"));
        }
        assertTrue(result.has("success"));

    }

    @Test
    public void testStringMatcher() throws Exception {
        HttpServletRequest request = mock(HttpServletRequest.class);       
        HttpServletResponse response = mock(HttpServletResponse.class);
		String type = "I_STRING";
		String mt = "INDIVIDUAL";
		String stubSavePath = "D:/workwork/ontoMatchFiles/PPDBSTRINGINDI.ttl";
		String stubTgt = "D:/workwork/ontoMatchFiles/jpspp.rdf";
		String stubSrc = "D:/workwork/ontoMatchFiles/tmpdbp.owl";
		//String stubTgt = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		//String stubSrc = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
		JSONObject jo  = new JSONObject();
        jo.put("alignmentFileAddress", stubSavePath);
        jo.put("targetOntoIRI", stubTgt);
        jo.put("sourceOntoIRI", stubSrc);
        jo.put("matcherType", type);
        jo.put("matchingType", mt);

        Reader inputString = new StringReader(jo.toString());
        BufferedReader reader = new BufferedReader(inputString);
        
        when(request.getMethod()).thenReturn("POST");
        when(request.getReader()).thenReturn(reader);

        JSONObject result  = new ElementMatcher().processRequestParameters(jo, request);
        if(result.has("error")) {
        	System.out.println(result.getString("error"));
        }
        assertTrue(result.has("success"));
    }
	
    @Test
    public void testWordMatcher() throws Exception {
        HttpServletRequest request = mock(HttpServletRequest.class);       
        HttpServletResponse response = mock(HttpServletResponse.class);
		String type = "I_WORD";
		String mt = "INDIVIDUAL";
		String stubSavePath = "D:/workwork/ontoMatchFiles/PPDBWORDINDI.ttl";
		String stubTgt = "D:/workwork/ontoMatchFiles/jpspp.rdf";
		String stubSrc = "D:/workwork/ontoMatchFiles/tmpdbp.owl";
		//String stubTgt = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		//String stubSrc = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
		JSONObject jo  = new JSONObject();
        jo.put("alignmentFileAddress", stubSavePath);
        jo.put("targetOntoIRI", stubTgt);
        jo.put("sourceOntoIRI", stubSrc);
        jo.put("matcherType", type);
        jo.put("matchingType", mt);

        Reader inputString = new StringReader(jo.toString());
        BufferedReader reader = new BufferedReader(inputString);
        
        when(request.getMethod()).thenReturn("POST");
        when(request.getReader()).thenReturn(reader);

        
        JSONObject result  = new ElementMatcher().processRequestParameters(jo, request);
        assertTrue(result.has("success"));
    }

	    @Test
	    public void testDomainMatcher() throws Exception {
	        HttpServletRequest request = mock(HttpServletRequest.class);       
	        HttpServletResponse response = mock(HttpServletResponse.class);
			String type = "DOMAIN";
			String mt = "TERM";
			String stubSavePath = "file:///D:/workwork/ontoMatchFiles/PPDBDOMAIN.owl";
			String stubTgt = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
			String stubSrc = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl";
			String stubModelPath = "D:/workwork/ontoMatchData/simMatch/model/modellevel2/model30t5p5a.gensim";
			String stubDictPath = "D:/workwork/ontoMatchData/simMatch/model/modellevel2/dictionarylevel2.gensim";
	        JSONObject jo  = new JSONObject();
	        jo.put("alignmentFileAddress", stubSavePath);
	        jo.put("targetOntoIRI", stubTgt);
	        jo.put("sourceOntoIRI", stubSrc);
	        jo.put("matcherType", type);
	        jo.put("matchingType", mt);
	        jo.put("modelAddress", stubModelPath);
	        jo.put("dictAddress", stubDictPath);
	        Reader inputString = new StringReader(jo.toString());
	        BufferedReader reader = new BufferedReader(inputString);
	        
	        when(request.getMethod()).thenReturn("POST");
	        when(request.getReader()).thenReturn(reader);

	        
	        JSONObject result  = new ElementMatcher().processRequestParameters(jo, request);
	        assertTrue(result.has("success"));
	    }
	
	
	
}
