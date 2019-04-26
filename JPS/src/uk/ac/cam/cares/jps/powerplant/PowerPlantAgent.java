package uk.ac.cam.cares.jps.powerplant;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


/**
 * Servlet implementation class PowerPlantWrapperAgent
 */
@WebServlet("/PowerPlant")
public class PowerPlantAgent extends HttpServlet {

	private static final long serialVersionUID = 2796334308068192311L;
	private Logger logger = LoggerFactory.getLogger(PowerPlantAgent.class);
	
	OntModel jenaOwlModel = null;
	private DatatypeProperty numval = null;
	private ObjectProperty hasProperty = null;
	private ObjectProperty hasValue = null;
	private ObjectProperty contains = null;
	private ObjectProperty has_density = null;
	private ObjectProperty has_length = null;
	private OntClass particleclass = null;
	private OntClass flowclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass moleculargroupclass = null;
	private OntClass diameterclass = null;
	private OntClass massfractionclass = null;
	private OntClass densityclass = null;
	
	static Individual gpers;
	static Individual m;
	static Individual kg_per_m3;
	
	
	private ObjectProperty hasunit = null;
	
	public static String baseURL = null;
	ArrayList<String> cpirilist = new ArrayList<String>();
	ArrayList<String> cpirilist2 = new ArrayList<String>();
	
    HashMap<String, String> hmap = new HashMap<String, String>();	
	
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);

		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");	
		particleclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#MolecularEntity");	
		flowclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");
		hasProperty=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		hasValue=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		 hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		 scalarvalueclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		 contains=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");
		 moleculargroupclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/molecular_structure.owl#MolecularGroup");
		 gpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");
		 diameterclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Diameter");
		 densityclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Density");
		 massfractionclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MassFraction");
		 has_density=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");
		 has_length=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");
		 m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		 kg_per_m3=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");
		 
	}
		
	
	public void doConversion(OntModel jenaOwlModel,String iri, JSONObject joSRMResult) throws JSONException, IOException {
	    /*Adding elements to HashMap*/
	    hmap.put("CO", "ChemSpecies_Carbon__monoxide");
	    hmap.put("CO2", "ChemSpecies_Carbon__dioxide");
	    hmap.put("NO2", "ChemSpecies_Nitrogen__dioxide");
	    hmap.put("HC", "PseudoComponent_Unburned_Hydrocarbon");
	    hmap.put("NOx", "PseudoComponent_Nitrogen__oxides");
	    
		for (int b = 0; b < hmap.size(); b++) {
			Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iri.split("#")[0] + "#V_" + hmap.get(hmap.keySet().toArray()[b]) + "_EmissionRate");
			valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double("0")));
		}

		
		//JSONObject jsonObject = parseJSONFile(outputfiledir); (used after the format of json file is fixed )
		Double molecularvalue = joSRMResult.getJSONObject("mixture").getJSONObject("molmass").getDouble("value")*1000;
		Double Cpvalue = joSRMResult.getJSONObject("mixture").getJSONObject("cp").getDouble("value");
		Double temperaturevalue = joSRMResult.getJSONObject("mixture").getJSONObject("temperature").getDouble("value")-273.15;
		Double massfluxvalue = joSRMResult.getJSONObject("mixture").getJSONObject("massflux").getDouble("value"); 
		Double densityvalue = joSRMResult.getJSONObject("mixture").getJSONObject("density").getDouble("value");
				
		Individual valueofmassflowrate = jenaOwlModel.getIndividual(iri.split("#")[0] +"#V_massF_WasteStream-001");
		valueofmassflowrate.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(massfluxvalue));
		
		Individual valueofdensityrate = jenaOwlModel.getIndividual(iri.split("#")[0] +"#V_Density_MaterialInWasteStream-001");
		valueofdensityrate.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(densityvalue));
		
		Individual valueoftemperature = jenaOwlModel.getIndividual(iri.split("#")[0] +"#V_Temperature_MaterialInWasteStream-001");
		valueoftemperature.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(temperaturevalue));
		
		Individual valueofcombinedmolecularmass = jenaOwlModel.getIndividual(iri.split("#")[0] +"#V_ChemSpecies_Combined_MolecularMass");
		valueofcombinedmolecularmass.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(molecularvalue));
		
		Individual valueofcombinedheatcapacity = jenaOwlModel.getIndividual(iri.split("#")[0] +"#V_Cp_MaterialInWasteStream-001");
		valueofcombinedheatcapacity.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(Cpvalue));
		
		Individual particulate = jenaOwlModel.getIndividual(iri.split("#")[0] +"#Particulate-001"); 
		//Individual particulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#Particulate-001",particleclass);//if it is not there
		Individual particulaterate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#Particulate-001_EmissionRate",flowclass);
		particulate.addProperty(hasProperty, particulaterate);
		Individual particleratevalue = jenaOwlModel.createIndividual(iri.split("#")[0] + "#V_Particulate-001_EmissionRate",scalarvalueclass);
		particulaterate.addProperty(hasValue, particleratevalue);
		particleratevalue.addProperty(hasunit, gpers);
		particleratevalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("1.5")); //temporary as the json file value is not exist
		
		int valueoftotalparticlesincluded = joSRMResult.getJSONArray("particle").length();
		for( int a=0; a<valueoftotalparticlesincluded;a++) {
			double valueofdiameter=joSRMResult.getJSONArray("particle").getJSONObject(a).getJSONObject("diameter").getDouble("value");
			String valueofmassfraction=String.valueOf(joSRMResult.getJSONArray("particle").getJSONObject(a).getJSONObject("mass_fraction").get("value"));
			int valueofdensity=joSRMResult.getJSONArray("particle").getJSONObject(a).getJSONObject("density").getInt("value");
			logger.info("mass fraction= "+valueofmassfraction);
			if(Double.valueOf(valueofmassfraction)>0) { //maybe later can be added the condition if diameter less than 1nm
				logger.info("the particle selected= "+a);
				Individual partialparticulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#Partial-"+a+"OfParticulate-001",moleculargroupclass);
				particulate.addProperty(contains, partialparticulate);
				
				Individual diameterpartialparticulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#Diameter_Partial-"+a+"OfParticulate-001",diameterclass);
				partialparticulate.addProperty(has_length,diameterpartialparticulate);
				Individual diametervaluepartialparticulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#V_Diameter_Partial-"+a+"OfParticulate-001",scalarvalueclass);
				diameterpartialparticulate.addProperty(hasValue, diametervaluepartialparticulate);
				diametervaluepartialparticulate.addProperty(hasunit, m);
				diametervaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofdiameter*0.000000001));
				
				Individual densitypartialparticulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#Density_Partial-"+a+"OfParticulate-001",densityclass);
				partialparticulate.addProperty(has_density,densitypartialparticulate);
				Individual densityvaluepartialparticulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#V_Density_Partial-"+a+"OfParticulate-001",scalarvalueclass);
				densitypartialparticulate.addProperty(hasValue, densityvaluepartialparticulate);
				densityvaluepartialparticulate.addProperty(hasunit, kg_per_m3);
				densityvaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofdensity));
				
				Individual massfractionpartialparticulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#MassFraction_Partial-"+a+"OfParticulate-001",massfractionclass);
				partialparticulate.addProperty(hasProperty,massfractionpartialparticulate);
				Individual massfractionvaluepartialparticulate = jenaOwlModel.createIndividual(iri.split("#")[0] +"#V_MassFraction_Partial-"+a+"OfParticulate-001",scalarvalueclass);
				massfractionpartialparticulate.addProperty(hasValue, massfractionvaluepartialparticulate);
				massfractionvaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofmassfraction));
			}
		}
		
		int valueoftotalpollutant = joSRMResult.getJSONArray("pollutants").length();
		for (int b = 0; b < valueoftotalpollutant; b++) {
			String parametername = joSRMResult.getJSONArray("pollutants").getJSONObject(b).getString("name");
			Double parametervalue = joSRMResult.getJSONArray("pollutants").getJSONObject(b).getDouble("value")*1000; 

			Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iri.split("#")[0] + "#V_" + hmap.get(parametername) + "_EmissionRate");
			valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(parametervalue));
		}
		

	}
		
	
    //temporarily unused
	public static JSONObject parseJSONFile(String filename) throws JSONException, IOException {
        String content = new String(Files.readAllBytes(Paths.get(filename)));
        JSONObject content2 = new JSONObject(content.trim());
        return content2;
    }
	
	public void startConversion(String iriOfPlant, JSONObject joSRMResult) throws Exception {
				
		initOWLClasses(jenaOwlModel);

		doConversion(jenaOwlModel,iriOfPlant, joSRMResult);

		
		String filePath2= iriOfPlant.replaceAll("http://www.theworldavatar.com/kb", "C:/TOMCAT/webapps/ROOT/kb").split("#")[0]; //update the file locally
		System.out.println("filepath2= "+filePath2);
		
		/** save the updated model file */
		savefile(jenaOwlModel, filePath2);

	}
	
	
	public static synchronized ResultSet query(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		//ResultSetFormatter.out(System.out, results, query); ?? don't know why this is needed to be commented
		return results;
	}
	

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	    logger.info("plant agent is called");
	    
		JSONObject joforrec = AgentCaller.readJsonParameter(request);
		System.out.println("json accepted= "+joforrec.toString());
		String iri = null;
		String iri2 = null;
		try {
			iri = joforrec.getString("reactionmechanism");
			iri2 = joforrec.getString("plant");

		} catch (JSONException e1) {
			logger.error(e1.getMessage(), e1);
			e1.printStackTrace();
		} 
		System.out.println("data got= "+iri+" and "+iri2);
		
		

			
		//try to query the owl file to get the waste stream inside it 
		String engineInfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "SELECT ?engine "
				+ "WHERE {?entity  a  cp:Plant  ." 
				+ "?entity   j2:hasSubsystem ?engine ."
				+ "MINUS { ?engine a cp:Pipe .}"
				+ "}";
		
		String wastestreamInfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "SELECT ?wastestream ?engine"
					+ "WHERE {?entity  a  cp:Plant  ." 
					+ "?entity   j2:hasSubsystem ?chimney ."
					+ "?chimney  j3:realizes  ?releaseprocess  ."
					+ "?releaseprocess   j4:hasOutput ?wastestream ."
					+ "}";
					
			jenaOwlModel = ModelFactory.createOntologyModel();	
			jenaOwlModel.read(iri2, null); // plant iri
				
			ResultSet rs_plant = PowerPlantAgent.query(wastestreamInfo,jenaOwlModel); // wastestream and engine iri
			
			ResultSet rs_plant2 = PowerPlantAgent.query(engineInfo,jenaOwlModel); // engine 
			
			for (; rs_plant.hasNext();) {			
				QuerySolution qs_p = rs_plant.nextSolution();

				Resource cpiri = qs_p.getResource("wastestream");
				String valueiri = cpiri.toString();
				System.out.println("cpirilistwastestream= "+valueiri);
				logger.info("query result1= "+valueiri);
				cpirilist.add(valueiri); // wastestream iri
			}
			
			for (; rs_plant2.hasNext();) {			
				QuerySolution qs_p = rs_plant2.nextSolution();

				Resource engineiri = qs_p.getResource("engine");
				String valueengineiri = engineiri.toString();
				logger.info("query result2= "+valueengineiri);
				cpirilist2.add(valueengineiri); // engine iri
			}

			
			try {
				JSONObject jo = new JSONObject().put("waste", cpirilist.get(0));
			
				logger.info("message to sent= "+jo.toString());
				response.getWriter().write(jo.toString()); // returns HTTP response with wastestream iri
			
				//send the info to SRM Engine Agent
				JSONObject dataSet = new JSONObject();
				dataSet.put("reactionmechanism",  iri) ;
				dataSet.put("engine",  cpirilist2.get(0)) ;
			
				String resultjson = AgentCaller.executeGet("JPS/SRMAgent", "query", dataSet.toString());
			    JSONObject joSrmResult = new JSONObject(resultjson).getJSONObject("results");
				
				//update the emission and other information into the plant owl file

				startConversion(iri2, joSrmResult);
			} catch (Exception e) {
				logger.error(e.getMessage(), e);
				throw new JPSRuntimeException(e.getMessage(), e);
			} //convert to update value

		    
		    cpirilist.clear();
		    cpirilist2.clear();
			
		
	}
	
	
}
