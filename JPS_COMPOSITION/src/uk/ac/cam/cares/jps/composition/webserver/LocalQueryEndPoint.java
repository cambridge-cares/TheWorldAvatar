package uk.ac.cam.cares.jps.composition.webserver;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


@WebServlet("/LocalQueryEndPoint")
public class LocalQueryEndPoint extends HttpServlet {
	private static final long serialVersionUID = 1L;
       

    public LocalQueryEndPoint() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		// Takes two set of values, 1. The Serialized rdf model . 2. The query 3. The heads + types of parameters 

		String rdfModelString = request.getParameter("rdfModel").replace("$", "#").replace("@", "#");		
		Model rdfModel = ModelFactory.createDefaultModel();
		String headerString = request.getParameter("headers");

		
		System.out.println("=========== rdfModel =============");
		System.out.println(rdfModelString);
		System.out.println("==================================");
		RDFDataMgr.read(rdfModel, new ByteArrayInputStream(rdfModelString.getBytes("UTF-8")), Lang.RDFJSON);
		
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, rdfModel, Lang.RDFXML);
		
		try {
			JSONObject result = new JSONObject();
			
			JSONArray headersInJSON = new JSONArray(headerString);
			for(int i = 0; i < headersInJSON.length(); i++) {
				JSONObject header = headersInJSON.getJSONObject(i);
				String type = header.getString("type");
				JSONArray names = header.getJSONArray("names");
				String query = header.getString("query");
				String key = header.getString("key");
				
				String r = doQuery(rdfModel, type, names, query);
				result.put(key, r);
				
				
				
			}
			response.getWriter().write(result.toString());


		} catch (JSONException e) {
			e.printStackTrace();
		}
	}
	
	public String doQuery(Model model, String type, JSONArray names, String queryString) {
		
		String result = null; 
 		
		String key = null;
		try {
			key = names.getString(0);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		if(type.equalsIgnoreCase("array")) { // The buildingList scenario 
			
			JSONArray resultArray = new JSONArray();
			Query query = QueryFactory.create(queryString);
			QueryExecution qe = QueryExecutionFactory.create(query, model);
			ResultSet results = qe.execSelect();

			while(results.hasNext()) {
				QuerySolution nextSolution = results.nextSolution();
 				String singleResult = nextSolution.get(key).toString();
 				resultArray.put(singleResult);
			}
			result = resultArray.toString();

		}

		if(type.equalsIgnoreCase("string"))
		{
			Query query = QueryFactory.create(queryString);
			QueryExecution qe = QueryExecutionFactory.create(query, model);
			ResultSet results = qe.execSelect();

			while(results.hasNext()) {
				QuerySolution nextSolution = results.nextSolution();
 				String singleResult = nextSolution.get(key).toString();
 				result = (singleResult);
			}
		}
		
		if(type.equalsIgnoreCase("map"))
		{
			JSONObject map = new JSONObject();
			Query query = QueryFactory.create(queryString);
			QueryExecution qe = QueryExecutionFactory.create(query, model);
			ResultSet results = qe.execSelect();
			while(results.hasNext()) {
				QuerySolution nextSolution = results.nextSolution();
				for(int i = 0; i < names.length(); i++) {
					try {
						String currentKey = names.getString(i);
		 				String singleResult = nextSolution.get(currentKey).toString();
		 				map.put(currentKey, singleResult);

					} catch (JSONException e) {
						e.printStackTrace();
					}
				}
			}
				result = map.toString();

		}		

		return result.toString();
	}
	


	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
