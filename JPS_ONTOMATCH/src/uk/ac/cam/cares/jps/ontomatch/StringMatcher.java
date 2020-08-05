package uk.ac.cam.cares.jps.ontomatch;

import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

public class StringMatcher extends JPSHttpServlet{
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Topic model agent");
		JSONObject jo = requestParams;
		try {
			//TODO:get parameter to get location from the target and source pkl 
		} catch (Exception e1) {
			e1.printStackTrace();
		}
        JSONObject result = new JSONObject();
		JSONArray resArr = new JSONArray();
		//TODO: call python, (python return matrix) then save the matrix to csv
		try {
			result.put("success", 1);
			System.out.print("read parameter result: ");
			System.out.println(result.toString());
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return result;
	}
}
