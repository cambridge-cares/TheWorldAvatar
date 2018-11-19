package uk.ac.cam.cares.co2emissions.factormodel;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
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
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
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
	
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");
	

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public static synchronized ResultSet query(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		ResultSetFormatter.out(System.out, results, query);
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
		
		
		String filePath1 = iri; // the result of written owl file
		String filePath2=filePath1.replaceAll("http://www.theworldavatar.com/kb","C:/TOMCAT/webapps/ROOT/kb").split("#")[0];
		
		System.out.println("newfile path= "+filePath2);
		System.out.println("iri= "+iri);
	    			
		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(iri,null);
		
		Double outputvalue = queryPowerplantProperty(jenaOwlModel);
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");	    			
		
		String plantname=iri.split("#")[1];
		System.out.println("plantname= "+plantname);
		Individual emission = jenaOwlModel.getIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_CO2Emission_of_"+plantname);
		
		emission.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(outputvalue));
		
		
		/** save the updated model file */
		try {
		savefile(jenaOwlModel, filePath2);
		} catch (URISyntaxException e1) {
			e1.printStackTrace();
		}
		
		
		
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

	public Double queryPowerplantProperty(OntModel jenaOwlModel) {
		
		String value1 = null;
		String capacity = null;
		Double outputvalue;
		String plantInfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
				+ "SELECT ?generation ?vcapa "
				+ "WHERE {?entity  a  cp:PowerPlant  ." 
				+ "?entity   j3:realizes ?generation ."
				+ "?entity   j4:designCapacity ?capa  ."
				+ "?capa   j2:hasValue ?valuecapa ."
				+ "?valuecapa   j2:numericalValue ?vcapa ." 
				+ "}"
				+ "ORDER BY ?product DESC(?added)";

		ResultSet rs_plant = FactorModel.query(plantInfo,jenaOwlModel); 
		
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
			outputvalue=Double.valueOf(capacity)*750*0.5*0.001;
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
