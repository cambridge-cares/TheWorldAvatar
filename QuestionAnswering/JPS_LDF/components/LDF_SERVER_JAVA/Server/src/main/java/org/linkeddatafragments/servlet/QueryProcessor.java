package org.linkeddatafragments.servlet;

import java.util.Iterator;

import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class QueryProcessor {

 
	// check whether the request is a equation query 
	public static boolean isOntokinEquationQuery(HttpServletRequest request) {
		return true; 
	}
	
	
	public static boolean isOntoCompChemSpeciesQuery(HttpServletRequest request) {
		
		String queryString = request.getQueryString();
	    String all_names = "";
		Iterator<String> paraNames = request.getParameterNames().asIterator();
	    while (paraNames.hasNext()) {
			String name = paraNames.next();
			// the condition is: it comes with a predicate: hasEquation, it comes with either the reactants or the products 
			all_names = all_names + name;
		}
	    
	    if(!(queryString.contains("hasName") && all_names.contains("predicate"))) {
	    	return false;
	    }
	    
	    if(!(all_names.contains("reactants") || all_names.contains("products"))) {
	    	return false;
	    }

		
		return true; 
	}
		
	public static JSONObject separateParameters(HttpServletRequest request) throws JSONException {
		// return the predicate string and the products/reactants separately
        JSONObject result = new JSONObject();
        String ontology = request.getPathInfo().replace("/","");
        result.put("ontology", ontology);
		// String hasEquationQuery = "predicate=" + request.getParameter("predicate");
		String hasEquationQuery = "predicate=http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasEquation";

		if(request.getParameterMap().containsKey("page")) {
			hasEquationQuery = hasEquationQuery + "&page=" + request.getParameter("page");
			result.put("page",  request.getParameter("page"));
		}
		

        result.put("hasEquationQuery", hasEquationQuery);
        if (request.getParameter("products") != null) {
            String[] products = request.getParameterValues("products");
            result.put("products", new JSONArray(products)); 
        }
        else {
        	result.put("products", new JSONArray());
        }
        
        if (request.getParameter("reactants") != null) {
	        String[] reactants = request.getParameterValues("reactants");
            result.put("reactants", new JSONArray(reactants)); 
        }else {
        	result.put("reactants", new JSONArray());
        }
 		
		return result;
	}
	
	
}
