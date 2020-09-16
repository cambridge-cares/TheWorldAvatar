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

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.ontomatch.AlignmentIOHelper;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher;
import uk.ac.cam.cares.jps.ontomatch.MatchAggregator;

public class TestMatchAggregator extends Mockito {

	class MatchAggregator4Test extends MatchAggregator {

		public void testPenalizing(String classAlignmentIRI, double sameClassThreshold, double pFactor)
				throws Exception {
			super.penalizing(classAlignmentIRI, sameClassThreshold, pFactor);
		}

		public List<Map> checkFinalScoreList() {
			return super.finalScoreList;
		}

		public void setFinalScoreList(List<Map> list) {
			super.finalScoreList = list;
		}

		public void setSrcTgt(String srcOnto, String tgtOnto) {
			super.srcOnto = srcOnto;
			super.tgtOnto = tgtOnto;
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

	@Test
	public void testAggregateWithNoChoice() {
		double threshold = 0.6;
		List<Double> weights = new ArrayList<Double>();
		weights.add(0.4);
		weights.add(0.4);
		weights.add(0.2);
		String onto1 = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl",
				onto2 = "D:/workwork/testFiles/ontologies/PowerPlant.owl";
		String[] stubAlignments = { "file:///D:/workwork/testFiles/alignments/PPDBDOMAIN.owl",
				"file:///D:/workwork/testFiles/alignments/PPDBSTRING.owl",
				"file:///D:/workwork/testFiles/alignments/PPDBWORD.owl" };
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

		JSONObject result = new ElementMatcher().processRequestParameters(jo, request);
		assertTrue(result.has("success"));

	}
}
