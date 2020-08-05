package uk.ac.cam.cares.jps.ontomatch;

import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

/**
 * basic class of element matcher, input: S T object, ouput:score matrix
 * @author zsc
 *
 */
public class BasicMatcher extends JPSHttpServlet{
	//TODO:input: parameters, training documents location, both should be put in settings?
//TODO: describe parameters and document location in owl?
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Topic model agent");
		JSONObject jo = requestParams;

        
        //TODO:check if agent locator can find this script
		try {
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
        JSONObject result = new JSONObject();
		JSONArray resArr = new JSONArray();
		try {
			result.put("success", 1);
			System.out.print("read parameter result: ");
			System.out.println(result.toString());
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;

	}

}
	
		
