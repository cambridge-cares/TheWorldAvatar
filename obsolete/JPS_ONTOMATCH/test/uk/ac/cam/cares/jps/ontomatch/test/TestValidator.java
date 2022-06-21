package uk.ac.cam.cares.jps.ontomatch.test;

import java.util.HashMap;
import java.util.Map;

import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper.CUSTOMVALUETYPE;

public class TestValidator extends TestCase {
	
	@Test
	public void testParamsGroup() {
		JSONObject jo = new JSONObject();
		Map params = new HashMap<String, CUSTOMVALUETYPE>();
		jo.put("path", "D:/workwork/ontoMatchFiles/jpspp.rdf");
		params.put("path", CUSTOMVALUETYPE.PATH);
		jo.put("threshold", 0.6);
		params.put("threshold",CUSTOMVALUETYPE.THRESHOLD);
		boolean result = ParamsValidateHelper.validateALLParams(jo, params);
	assertTrue(result);
	}

	@Test
	public void testWrongParamsGroup() {
		JSONObject jo = new JSONObject();
		Map params = new HashMap<String, CUSTOMVALUETYPE>();
		jo.put("path", "D:/workwork/ontoMatchFiles/jpspp.rdf");
		params.put("path", CUSTOMVALUETYPE.URL);
		jo.put("threshold", 0.6);
		params.put("threshold",CUSTOMVALUETYPE.THRESHOLD);
		boolean result = ParamsValidateHelper.validateALLParams(jo, params);
	assertTrue(!result);
	}
	
	@Ignore
	@Test
	public void testValidWeight() {
		Double w[]  = {0.1,0.5,0.4};
		Double n[]  = {0.1,0.5};

		boolean result = ParamsValidateHelper.isValidWeights(w);
		boolean resultFalse = ParamsValidateHelper.isValidWeights(n);
		assertTrue(result);
		assertTrue(!resultFalse);
	}

	@Test
	public void testValidThreshold() {
		double t1 = 0.5;
		double t2 = 0.0;
		double t3 = 1.5;

		boolean result1 = ParamsValidateHelper.isValidThreshold(t1);
		boolean result2 = ParamsValidateHelper.isValidThreshold(t2);
		boolean result3 = ParamsValidateHelper.isValidThreshold(t3);

		assertTrue(result1);
		assertTrue(result2);

		assertTrue(!result3);
	}
}
