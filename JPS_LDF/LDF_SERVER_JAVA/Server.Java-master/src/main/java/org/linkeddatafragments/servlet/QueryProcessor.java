package org.linkeddatafragments.servlet;

import java.util.Enumeration;
import java.util.Iterator;

import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class QueryProcessor {

 
	// check whether the request is a equation query 
	public static boolean isEquationQuery(HttpServletRequest request) {
		
		String queryString = request.getQueryString();
	    String all_names = "";
		Iterator<String> paraNames = request.getParameterNames().asIterator();
	    while (paraNames.hasNext()) {
			String name = paraNames.next();
			// the condition is: it comes with a predicate: hasEquation, it comes with either the reactants or the products 
			all_names = all_names + name;
		}
	    
	    if(!(queryString.contains("hasEquation") && all_names.contains("predicate"))) {
	    	return false;
	    }
	    
	    if(!(all_names.contains("reactants") || all_names.contains("products"))) {
	    	return false;
	    }
	    
//	    if(all_names.contains("subject") || all_names.contains("object")) {
//	    	return false;
//	    }
	    
		
		return true; 
	}
		
	public static JSONObject separateParameters(HttpServletRequest request) throws JSONException {
		// return the predicate string and the products/reactants separately
        JSONObject result = new JSONObject();

		String hasEquationQuery = "predicate=" + request.getParameter("predicate");
		System.out.println("========== query string in separator =========");
		System.out.println(request.getQueryString());
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
