package uk.ac.cam.cares.co2emissions.factormodel;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.DatatypeProperty;
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
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


@WebServlet("/FactorModel")
public class FactorModel extends HttpServlet {

	private static final long serialVersionUID = 1L;
    

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	
	private DatatypeProperty numval = null;

	Logger logger = LoggerFactory.getLogger(FactorModel.class);
	

	public static synchronized ResultSet query(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		ResultSetFormatter.out(System.out, results, query);
		return results;
	}
	
	public static synchronized ResultSet queryFromFusekiServer(String serviceURI, String query) {
		
		QueryExecution q = QueryExecutionFactory.sparqlService(serviceURI,query);
		ResultSet results = q.execSelect();	

		return results;
	}
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
				
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		
		JSONObject jo = AgentCaller.readJsonParameter(request);

		String iri=null;
		try {
			 iri = jo.getString("plant");
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		//we get the iri plant
		
		Double outputvalue = queryPowerplantProperty(iri);
		
		
//update to fuseki in java
		
		String plantupdate = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
				+ "WITH <"+iri+">"
                +"DELETE { ?valueemission j2:numericalValue ?vemission .} " 
                +"INSERT { ?valueemission j2:numericalValue "+outputvalue+" .} " 
                +"WHERE { ?generation   j5:hasEmission ?emission ."  
                +"?emission   j2:hasValue ?valueemission . "
                + "?valueemission   j2:numericalValue ?vemission ."
                + "}" ;
		

        UpdateProcessor upp = UpdateExecutionFactory.createRemote(UpdateFactory.create(plantupdate),"http://www.theworldavatar.com:80/damecoolquestion/worldpowerplantsng/update");
        upp.execute();
		
		
		
		
		
//**************************************************************************************************		
		JSONObject dataSet = new JSONObject();
		try {
			dataSet.put("emission", outputvalue) ;
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		String message = dataSet.toString();
		System.out.println ("message= "+message);
		
		AgentCaller.writeJsonParameter(response, dataSet);
		
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
			System.out.println ("value1= "+value1);
			System.out.println ("capacity= "+capacity);
			

			
		}
		
		if (value1.contentEquals("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CoalGeneration"))
		{
			outputvalue=Double.valueOf(capacity)*1000*0.8*0.001;
		}
		else if (value1.contentEquals("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OilGeneration"))
		{
			outputvalue=Double.valueOf(capacity)*1000*0.5*0.001;
		}
		else if (value1.contentEquals("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration"))
		{
			outputvalue=Double.valueOf(capacity)*750*0.5*0.001;
		}
		else
		{
			outputvalue=0.0;
			System.out.println("error");
	
		}
		return outputvalue;
	}



//	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//		// TODO Auto-generated method stub
//		doGet(request, response);
//	}

}
