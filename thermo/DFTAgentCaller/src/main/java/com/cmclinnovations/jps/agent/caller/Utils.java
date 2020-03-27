package com.cmclinnovations.jps.agent.caller;

import java.util.ArrayList;
import java.util.List;

/**
 * Contains methods that can be used in any class within the project.
 * 
 * @author msff2
 *
 */
public class Utils {
	/**
	 * Splits comma separated SPARQL Endpoints and converts them to a list.
	 * 
	 * @param endpoints
	 * @return
	 */
	public static List<String> getEndpoints(String endpoints){
		List<String> endpointURLsToReturn = new ArrayList<String>(); 
		if(endpoints != null && !endpoints.trim().isEmpty()){
			// In the property file, SPARQL Endpoints are split using a comma (,)
			String[] endpointURLs = endpoints.split(",");
			for(String endpointURL:endpointURLs){
				if(endpointURL.trim().startsWith("http://") || endpointURL.trim().startsWith("https://")){
					endpointURLsToReturn.add(endpointURL);
				}
			}
		}
		if(endpointURLsToReturn.size()>0){
			return endpointURLsToReturn;
		}
		return null;
	}
}
