package uk.ac.cam.cares.jps.co2emissions.querypowerplants;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

@WebServlet("/QueryPowerPlants")
public class QueryPowerPlants extends HttpServlet {
	
	private static final long serialVersionUID = 1L;
	Logger logger = LoggerFactory.getLogger(QueryPowerPlants.class);
	ArrayList <String> datalist = new ArrayList <String>();	
	
	public static synchronized ResultSet queryFromFusekiServer(String serviceURI, String query) {

		QueryExecution q = QueryExecutionFactory.sparqlService(serviceURI, query);
		ResultSet results = q.execSelect();

		return results;
	}
		
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");

		String sparqlstring = request.getParameter("query");

		logger.info("sparql= " + sparqlstring);

		ResultSet rs_plant = QueryPowerPlants.queryFromFusekiServer(
				"http://www.theworldavatar.com:80/damecoolquestion/worldpowerplantsng/query", sparqlstring);
//		ResultSet rs_plant = QueryPowerPlants.queryFromFusekiServer(
//				"http://www.theworldavatar.com:80/damecoolquestion/worldpowerplantsinmemory/query", sparqlstring);

		JSONObject dataSet = new JSONObject();
		JSONArray array = new JSONArray();

		while (rs_plant.hasNext()) {

			QuerySolution soln = rs_plant.nextSolution();

			Iterator<String> varNames = soln.varNames();

			JSONObject item = new JSONObject();
			for (; varNames.hasNext();) {

				String varName = varNames.next();

				try {

					String entity = soln.getResource(varName).toString();
					logger.debug(entity);
					datalist.add(entity);
					item.put(varName, entity);

				} catch (Exception e) {
					String entity = soln.getLiteral(varName).getString();
					logger.debug(entity);
					datalist.add(entity);
					try {
						item.put(varName, entity);
					} catch (JSONException e1) {
						logger.error(e1.getMessage(), e1);
					}

				}

			}
			array.put(item);
		}
		try {
			dataSet.put("results", array);
		} catch (JSONException e) {

			logger.error(e.getMessage(), e);
		}
		String message = dataSet.toString();

		logger.info("message= " + message);

		AgentCaller.writeJsonParameter(response, dataSet);

		datalist.clear();

	}

}
