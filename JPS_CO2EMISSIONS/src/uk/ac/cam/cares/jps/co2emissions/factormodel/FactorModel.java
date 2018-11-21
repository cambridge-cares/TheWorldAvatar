package uk.ac.cam.cares.jps.co2emissions.factormodel;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


@WebServlet("/FactorModel")
public class FactorModel extends HttpServlet {

	private static final long serialVersionUID = 1L;

	Logger logger = LoggerFactory.getLogger(FactorModel.class);

	public static synchronized ResultSet queryFromFusekiServer(String serviceURI, String query) {
		
		QueryExecution q = QueryExecutionFactory.sparqlService(serviceURI,query);
		ResultSet results = q.execSelect();	

		return results;
	}
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");

		JSONObject jo = AgentCaller.readJsonParameter(request);

		String iri = null;
		try {
			iri = jo.getString("plant");
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
			
		}

		// we get the iri plant

		Double outputvalue = queryPowerplantProperty(iri);

		// update to fuseki in java

		String plantupdate = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
				+ "WITH <" + iri + ">" + "DELETE { ?valueemission j2:numericalValue ?vemission .} "
				+ "INSERT { ?valueemission j2:numericalValue " + outputvalue + " .} "
				+ "WHERE { ?generation   j5:hasEmission ?emission ." + "?emission   j2:hasValue ?valueemission . "
				+ "?valueemission   j2:numericalValue ?vemission ." + "}";

		UpdateProcessor upp = UpdateExecutionFactory.createRemote(UpdateFactory.create(plantupdate),
				"http://www.theworldavatar.com:80/damecoolquestion/worldpowerplantsng/update");
		upp.execute();

		// **************************************************************************************************
		JSONObject dataSet = new JSONObject();
		
		JSONWriter jsonOutput = null;
		try {
			jsonOutput = new JSONStringer().object().
						key("hasEmission").object()
							.key("hasValue").object()
								.key("numericalValue").value(outputvalue).endObject()
							.endObject()
						.endObject();
			
		} catch (JSONException e1) {
			logger.error(e1.getMessage(), e1);
		} 
		
		logger.info("jsonOutput=\n" + jsonOutput);

		String jsonobject=jsonOutput.toString();
		try {
			dataSet=convertstringtojsonobject(jsonobject);
		} catch (Exception e) {
		logger.error(e.getMessage(), e);
					
		}
		String message = dataSet.toString();
		logger.info("message= " + message);
		
		AgentCaller.writeJsonParameter(response, dataSet);
	}

	private JSONObject convertstringtojsonobject(String a) throws Exception {
		
		return new JSONObject(a);	
	}

	public Double queryPowerplantProperty(String iri) {
		
		String value1 = null;
		String capacity = null;
		Double outputvalue;
		String plantInfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
				+ "SELECT ?generation ?vcapa "
				+ "{graph "+"<"+iri+">"
				+ "{?entity  a  cp:PowerPlant  ." 
				+ "?entity   j3:realizes ?generation ."
				+ "?entity   j4:designCapacity ?capa  ."
				+ "?capa   j2:hasValue ?valuecapa ."
				+ "?valuecapa   j2:numericalValue ?vcapa ." 
				+ "}"
				+ "}";

		ResultSet rs_plant = FactorModel.queryFromFusekiServer("http://www.theworldavatar.com:80/damecoolquestion/worldpowerplantsng/query",plantInfo); 
		
		for (; rs_plant.hasNext();) {			
			QuerySolution qs_p = rs_plant.nextSolution();

			Resource cpiri = qs_p.getResource("generation");
			value1 = cpiri.toString();
			Literal cap = qs_p.getLiteral("vcapa"); // extract the name of the source
			capacity = cap.getString();
	
		}

		if (value1.contentEquals(
				"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CoalGeneration")) {
			outputvalue = Double.valueOf(capacity) * 1000 * 0.8 * 0.001;
		} else if (value1.contentEquals(
				"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OilGeneration")) {
			outputvalue = Double.valueOf(capacity) * 1000 * 0.5 * 0.001;
		} else if (value1.contentEquals(
				"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration")) {
			outputvalue = Double.valueOf(capacity) * 750 * 0.5 * 0.001;
		} else {
			throw new JPSRuntimeException("unknown generation type: " + value1);
		}
		return outputvalue;
	}



//	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//		// TODO Auto-generated method stub
//		doGet(request, response);
//	}

}
