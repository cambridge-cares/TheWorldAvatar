package uk.ac.cam.cares.jps.ontomatch.test;

import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.ontomatch.ElementMatcher;
import uk.ac.cam.cares.jps.ontomatch.MatchAggregator;
import uk.ac.cam.cares.jps.ontomatch.alignment.AlignmentIOHelper;

public class TestMatchAggregator extends Mockito {

	class MatchAggregator4Test extends MatchAggregator {

		public void testPenalizing(String classAlignmentIRI, double sameClassThreshold, double pFactor)
				throws Exception {
			super.penalizing(classAlignmentIRI, sameClassThreshold, pFactor);
		}

		public List<Map> checkFinalScoreList() {
			return super.batchScoreList;
		}

		public void setFinalScoreList(List<Map> list) {
			super.batchScoreList = list;
		}

		public void setSrcTgt(String srcOnto, String tgtOnto) {
			super.srcOnto = srcOnto;
			super.tgtOnto = tgtOnto;
		}
	}

	@Ignore
	@Test
	public void testParseValue() {
		String valuestr = "\"0.587\"^^http://www.w3.org/2001/XMLSchema#float";
				
		MatchAggregator4Test m = new MatchAggregator4Test();
		double v = m.getDoubleSparqlResult(valuestr);
		System.out.println(v);

	}
	@Ignore
	@Test
	public void checkListSorting() {
		List<String[]> a = new ArrayList<String[]>();
		String[] a1 = {"b","c","0"};
		String[] a2 = {"b","b","0"};
		String[] a3 = {"a","c","0"};
		a.add(a1);a.add(a2);a.add(a3);
		List<String[]> sorted = new MatchAggregator4Test().sortAlignmentListByEntityName(a);
	//TODO
		for(String[] item:sorted) {
		for(String i: item) {
			System.out.println(i);
		}
	}
	}

	
	
	@Ignore
	@Test
	public void testFunctionPenalizing() throws Exception {
		MatchAggregator4Test m = new MatchAggregator4Test();
		List testIndividualList = new ArrayList<HashMap>();
		Map cell1 = new HashMap();
		cell1.put("entity1", "www.test1.com/i1");
		cell1.put("entity2", "www.test2.com/i2");
		cell1.put("measure", 0.7);
		testIndividualList.add(cell1);
		Map cell2 = new HashMap();
		cell2.put("entity1", "www.test1.com/i11");
		cell2.put("entity2", "www.test2.com/i22");
		cell2.put("measure", 0.6);
		testIndividualList.add(cell2);
		m.setSrcTgt("D:/workwork/testFiles/test2.owl", "D:/workwork/testFiles/test1.owl");
		m.setFinalScoreList(testIndividualList);
		String classAlignmentIRI = "D:/workwork/testFiles/testA.owl";
		m.testPenalizing(classAlignmentIRI, 0.6, 0.6);
		List newList = m.checkFinalScoreList();
		Map newCell1 = (Map) newList.get(0);
		double newScore = (double) newCell1.get("measure");

	}

	@Ignore
	@Test
	public void testAggregateWithMiniCase() {
		double threshold = 0.6;
		List<Double> weights = new ArrayList<Double>();
		weights.add(0.5);
		weights.add(0.5);
		String onto1 = "D:/workwork/testFiles/ontologies/testOnto1.owl",
				onto2 = "D:/workwork/testFiles/ontologies/testOnto2.owl";
		List<String> stubAlignments = new ArrayList<String>();
		stubAlignments.add("file:///D:/workwork/testFiles/alignments/PPDBSTRING.owl");

		stubAlignments.add( "file:///D:/workwork/testFiles/alignments/PPDBWORD.owl" );



		String addr = "http://www.theworldavatar.com/finalTestAggregaotr.owl";

		JSONObject jo = new JSONObject();
		jo.put("threshold", 0.6);
		jo.put("srcOnto", onto1);
		jo.put("tgtOnto", onto2);
		jo.put("addr", addr);
		jo.put("weights", weights);
		jo.put("alignments", stubAlignments);
		Reader inputString = new StringReader(jo.toString());
		BufferedReader reader = new BufferedReader(inputString);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getMethod()).thenReturn("POST");
		try {
			when(request.getReader()).thenReturn(reader);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		JSONObject result = new MatchAggregator().processRequestParameters(jo, request);
		assertTrue(result.has("success"));

	}
	
	@Test
	public void testAggregateIndi() {
		double threshold = 0.5;
		List<Double> weights = new ArrayList<Double>();
		weights.add(0.5);
		weights.add(0.4);
		weights.add(0.1);

		String onto1 = "D:/workwork/ontoMatchFiles/tmpdbpAllT.owl",
				onto2 = "D:/workwork/ontoMatchFiles/jpsppcombine.rdf";
		List<String> stubAlignments = new ArrayList<String>();
		stubAlignments.add("D:/workwork/ontoMatchFiles/PPDBSTRINGINDI.ttl");
		stubAlignments.add( "D:/workwork/ontoMatchFiles/PPDBWORDINDI.ttl" );
		stubAlignments.add( "D:/workwork/ontoMatchFiles/PPDBVALUEINDI.ttl" );


		String addr = "http://www.theworldavatar.com/powerplantsAlignmentAll1019.owl";
		List<String> choices = new ArrayList<String>();
         choices.add("PENALIZING");
         choices.add("CARDINALITY");
		JSONObject jo = new JSONObject();
		jo.put("threshold", 0.55);
		jo.put("srcOnto", onto1);
		jo.put("tgtOnto", onto2);
		jo.put("addr", addr);
		jo.put("weights", weights);
		jo.put("alignments", stubAlignments);
		jo.put("choices", choices);
		jo.put("classAlign", "D:/workwork/ontoMatchFiles/finalTestAggregaotr.owl");
		jo.put("pFactor", 0.5);
		jo.put("sameClassThreshold", 0.6);

		
		Reader inputString = new StringReader(jo.toString());
		BufferedReader reader = new BufferedReader(inputString);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getMethod()).thenReturn("POST");
		try {
			when(request.getReader()).thenReturn(reader);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		JSONObject result = new MatchAggregator().processRequestParameters(jo, request);
		assertTrue(result.has("success"));

	}
	
	@Ignore
	@Test
	public void testDomain() {
		List<Double> weights = new ArrayList<Double>();
		weights.add(1.0);
		String onto1 = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl",
				onto2 = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		List<String> stubAlignments = new ArrayList<String>();
		stubAlignments.add("file:///D:/workwork/ontoMatchFiles/PowerPlantdbpedia_2014DOMAIN.owl");

		String addr = "http://www.theworldavatar.com/testDomain.owl";

		JSONObject jo = new JSONObject();
		jo.put("threshold", 0.6);
		jo.put("srcOnto", onto1);
		jo.put("tgtOnto", onto2);
		jo.put("addr", addr);
		jo.put("weights", weights);
		jo.put("alignments", stubAlignments);
		Reader inputString = new StringReader(jo.toString());
		BufferedReader reader = new BufferedReader(inputString);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getMethod()).thenReturn("POST");
		try {
			when(request.getReader()).thenReturn(reader);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		JSONObject result = new MatchAggregator().processRequestParameters(jo, request);
		assertTrue(result.has("success"));

	}
	
	@Ignore
	@Test
	public void testAggregateWithNoChoice() {
		List<Double> weights = new ArrayList<Double>();
		weights.add(0.4);
		weights.add(0.4);
		weights.add(0.2);

		String onto1 = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl",
				onto2 = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		List<String> stubAlignments = new ArrayList<String>();
		stubAlignments.add("file:///D:/workwork/ontoMatchFiles/PowerPlantdbpedia_2014WORD.owl");
		stubAlignments.add("file:///D:/workwork/ontoMatchFiles/PowerPlantdbpedia_2014STRING.owl");
		stubAlignments.add("file:///D:/workwork/ontoMatchFiles/PowerPlantdbpedia_2014DOMAIN.owl");

		String addr = "http://www.theworldavatar.com/finalTest.owl";

		JSONObject jo = new JSONObject();
		jo.put("threshold", 0.6);
		jo.put("srcOnto", onto1);
		jo.put("tgtOnto", onto2);
		jo.put("addr", addr);
		jo.put("weights", weights);
		jo.put("alignments", stubAlignments);
		Reader inputString = new StringReader(jo.toString());
		BufferedReader reader = new BufferedReader(inputString);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getMethod()).thenReturn("POST");
		try {
			when(request.getReader()).thenReturn(reader);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		JSONObject result = new MatchAggregator().processRequestParameters(jo, request);
		assertTrue(result.has("success"));

	}
	
	@Ignore
	@Test
	public void testCardinalityFiltering() {
		String onto1 = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl",
				onto2 = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		String thisAlignmentIRI = "C:/Users/morta/WebstormProjects/MatchAgentVisual/public/powerplantsAlignmentAll1004.owl";
		try {
			
			MatchAggregator m = new MatchAggregator();
			m.one2oneCardinalityFiltering(thisAlignmentIRI);
			for(Map i : m.batchScoreList) {
				System.out.println(m.toString());
			}
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
	
	@Ignore
	@Test
	public void testPenalizing() throws Exception {
		String srcOnto = "D:/workwork/ontoMatchFiles/tmpdbpAllT.owl";
		String tgtOnto = "D:/workwork/ontoMatchFiles/jpsppcombine.rdf";
		MatchAggregator4Test m = new MatchAggregator4Test();
		List<Map> test = new ArrayList<Map>();
		HashMap line1 = new HashMap();
		line1.put("entity1","http://dbpedia.org/resource/Schwarze_Pumpe_power_station");
		line1.put("entity2","http://www.theworldavatar.com/kb/powerplants/Schwarze_Pumpe_Coal_Power_Plant_Germany.owl#Schwarze_Pumpe_Coal_Power_Plant_Germany");
		line1.put("measure", 0.7);
		HashMap line2 = new HashMap();
		line2.put("entity1","controlgroup1");
		line2.put("entity2","controlgroup2");
		line2.put("measure", 0.7);
        test.add(line1);
        test.add(line2);
		m.setFinalScoreList(test);
		m.setSrcTgt(srcOnto, tgtOnto);
		m.penalizing("D:/workwork/ontoMatchFiles/finalTestAggregaotr.owl", 0.6, 0.1);
         List<Map> result = m.checkFinalScoreList();
		System.out.println("end");
		
	}
	
}
