package uk.ac.cam.cares.co2emissions.querypowerplants;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

@WebServlet("/QueryPowerPlants")
public class QueryPowerPlants extends HttpServlet {
	
	private static final long serialVersionUID = 1L;

	ArrayList <String> datalist = new ArrayList <String>();
	OntModel jenaOwlModel = ModelFactory.createOntologyModel();
	
	public static synchronized ResultSet query(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		System.out.println("get the result?== "+queryExec);
		ResultSet rs = queryExec.execSelect();  
		System.out.println("get the result== "+rs);
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		ResultSetFormatter.out(System.out, results, query);
		return results;
	}
	
	
	public static synchronized ResultSet queryFromFusekiServer(String serviceURI, String query) {
		
		QueryExecution q = QueryExecutionFactory.sparqlService(serviceURI,query);
		ResultSet results = q.execSelect();	

		return results;
	}
		
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");

		String sparqlstring = request.getParameter("query");

		System.out.println("sparql= " + sparqlstring);

		ArrayList<String> outputvalue = queryPowerplantProperty(sparqlstring);

		JSONObject dataSet = new JSONObject();

		try {
			JSONArray array = new JSONArray();

			for (int w = 0; w < outputvalue.size(); w += 7) {
				JSONObject item = new JSONObject();
				item.put("plant", outputvalue.get(w + 0));
				item.put("generation", outputvalue.get(w + 1));
				item.put("capacity", outputvalue.get(w + 2));
				item.put("emission", outputvalue.get(w + 3));
				item.put("year", outputvalue.get(w + 4));
				item.put("technology", outputvalue.get(w + 5));
				item.put("fuel", outputvalue.get(w + 6));

				array.put(item);
			}

			dataSet.put("results", array);

		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		String message = dataSet.toString();

		System.out.println("message= " + message);

		AgentCaller.writeJsonParameter(response, dataSet);

		outputvalue.clear();

	}
			
	
	
	
	public ArrayList<String> queryPowerplantProperty(String sparqlstring) {

		ResultSet rs_plant = QueryPowerPlants.queryFromFusekiServer(
				"http://www.theworldavatar.com:80/damecoolquestion/worldpowerplantsng/query", sparqlstring);

		while (rs_plant.hasNext()) {
			QuerySolution soln = rs_plant.nextSolution();
			// assumes that you have an "?g" in your query
			System.out.println("-----------------------------------------------------");

			Iterator<String> varNames = soln.varNames();
			for (int i = 0; varNames.hasNext(); i++) {

				String varName = varNames.next();

				System.out.println("hello result= " + varName);

				try {

					String entity = soln.getResource(varName).toString();
					System.out.println(entity);
					datalist.add(entity);
					
				} catch (Exception e) {
					String entity = soln.getLiteral(varName).getString();
					System.out.println(entity);
					datalist.add(entity);
				}
			}
		}

		return datalist;
	}
	
	
	
	
	
	
	
	
	
	
}
