package uk.ac.cares.jps.composition.compositionagent;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONObject;

public class OptimizationAgent {

	// At the moment, let the optimization agent only eliminate the two weather
	// agents with lower scores.

	public JSONObject optimize_composition_result(JSONObject composition_result) {

		ArrayList<String> result = new ArrayList<String>();
		result.add("http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service");
		result.add("http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service");
		composition_result.put("elimination_list", new JSONArray(result));
		return composition_result;

	}

}
