package uk.ac.cam.cares.jps.ship;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
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
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

/**
 * Servlet implementation class ShipAgent
 * 
 * 4/7 2019 : the iri of the engine and chimney are HARDCODED
 */
@WebServlet("/ShipAgent")
public class ShipAgent extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(ShipAgent.class);
	
	OntModel jenaOwlModel = null;
	private DatatypeProperty numval = null;
	private ObjectProperty hasProperty = null;
	private ObjectProperty hasValue = null;
	private ObjectProperty contains = null;
	private ObjectProperty has_density = null;
	private ObjectProperty has_length = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hasSubsystem=null;
	private OntClass particleclass = null;
	private OntClass flowclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass partialparticleclass = null;
	private OntClass diameterclass = null;
	private OntClass massfractionclass = null;
	private OntClass densityclass = null;
	private ObjectProperty  hasRepresentativeParticle = null;
	
	static Individual gpers;
	static Individual m;
	static Individual kg_per_m3;
	
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
		particleclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ParticulateMaterialAmount");	
		flowclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");
		hasProperty=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		hasValue=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		 hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		 scalarvalueclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		 contains=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");
		 partialparticleclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#SingleParticle");
		 gpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");
		 diameterclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Diameter");
		 densityclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Density");
		 massfractionclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MassFraction");
		 has_density=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");
		 has_length=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");
		 m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		 kg_per_m3=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");
		 hasSubsystem=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
		 hasRepresentativeParticle=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");
	}
		
	
	public void doConversion(OntModel jenaOwlModel,String iriofchimney,JSONObject jsonObject) throws JSONException, IOException
	{
	    /*Adding elements to HashMap*/
	    hmap.put("CO", "ChemSpecies_Carbon__monoxide");
	    hmap.put("CO2", "ChemSpecies_Carbon__dioxide");
	    hmap.put("NO2", "ChemSpecies_Nitrogen__dioxide");
	    hmap.put("HC", "PseudoComponent_Unburned_Hydrocarbon");
	    hmap.put("NOx", "PseudoComponent_Nitrogen__oxides");
	    hmap.put("SO2", "ChemSpecies_Sulfur__dioxide");
	    hmap.put("O3", "ChemSpecies_Ozone");
	    
	    
	    //remove the subtree first for particle 1-8-19 undo it:	    
	    //JenaModelWrapper c= new JenaModelWrapper(jenaOwlModel, null);
	    //logger.info("removed subtree= "+iriofchimney.split("#")[0]+"#GeneralizedAmount_WasteStreamOfChimney-1");
	    //c.removeSubtree(iriofchimney.split("#")[0]+"#GeneralizedAmount_WasteStreamOfChimney-1", Prefixes.OCPSYST, "contains");
	    
	    //reset all the emission rate to be zero
		for (int b = 0; b < hmap.size(); b++) {
			Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(hmap.keySet().toArray()[b]) + "_EmissionRate");
			valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double("0")));
		}
		
		Individual particleratevalue1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Particulate-001_EmissionRate");
		//Individual particleratevalue2 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Particulate-002_EmissionRate");
		if(particleratevalue1!=null) {
			particleratevalue1.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double("0")));
		}
//		if(particleratevalue2!=null) {
//			particleratevalue2.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double("0")));
//		}
		
		
		//JSONObject jsonObject = parseJSONFile(outputfiledir); (used after the format of json file is fixed )
		Double molecularvalue = jsonObject.getJSONObject("mixture").getJSONObject("molmass").getDouble("value")*1000;
		Double Cpvalue = jsonObject.getJSONObject("mixture").getJSONObject("cp").getDouble("value");
		Double temperaturevalue = jsonObject.getJSONObject("mixture").getJSONObject("temperature").getDouble("value")-273.15;
		Double massfluxvalue = jsonObject.getJSONObject("mixture").getJSONObject("massflux").getDouble("value"); //(multiplied by 100 temporarily to make it visible)
		Double densityvalue = jsonObject.getJSONObject("mixture").getJSONObject("density").getDouble("value");
		
		int valueoftotalpollutant = jsonObject.getJSONArray("pollutants").length();
		
		
		System.out.println("value= "+massfluxvalue);
		Individual valueofmassflowrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_massF_WasteStream-001");
		valueofmassflowrate.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(massfluxvalue));

		
		Individual valueofdensityrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_Density_MaterialInWasteStream-001");
		valueofdensityrate.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(densityvalue));
		
		Individual valueoftemperature = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_Temperature_MaterialInWasteStream-001");
		valueoftemperature.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(temperaturevalue));
		
		Individual valueofcombinedmolecularmass = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_ChemSpecies_Combined_MolecularMass");
		valueofcombinedmolecularmass.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(molecularvalue));
		
		Individual valueofcombinedheatcapacity = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_Cp_MaterialInWasteStream-001");
		valueofcombinedheatcapacity.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(Cpvalue));
		
		
		Individual particulate1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#Particulate-001"); 
		
		if(particulate1==null)
		{
			Individual material2 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#GeneralizedAmount_WasteStreamOfChimney-1");
//			particulate1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#Particulate-001",particleclass);//if it is not there	
			particulate1 = jenaOwlModel.createIndividual("http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Particulate-001",particleclass);//temp solution 2-8-19
			material2.addProperty(contains, particulate1);
		}
		Individual particulaterate1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#Particulate-001_EmissionRate");

		if(particulaterate1==null)
		{
			particulaterate1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#Particulate-001_EmissionRate",flowclass);	
			particleratevalue1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_Particulate-001_EmissionRate",scalarvalueclass);
		}
		
		particulate1.addProperty(hasProperty, particulaterate1);
		
		
		particulaterate1.addProperty(hasValue, particleratevalue1);
		particleratevalue1.addProperty(hasunit, gpers);
			
		int valueoftotalparticlesincluded = jsonObject.getJSONArray("particle").length();
		
		//------------------------------this part for seeing the correct format and version of json----------------------------------------
		double totalparticleemission=0.0;
		for( int a=0; a<valueoftotalparticlesincluded;a++) {
			if(jsonObject.getJSONArray("particle").getJSONObject(a).has("emission_rate")) {
				double valueofparticlerate=jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("emission_rate").getDouble("value");
				totalparticleemission=totalparticleemission+valueofparticlerate;
			}
//			else
//			{
//				totalparticleemission=0.0;
//			}
		}
		particleratevalue1.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(totalparticleemission)); 
		logger.info("total mass of particle="+totalparticleemission);
		//---------------------------------------------------------------------------------------------------------------------------------
		
		for( int a=0; a<valueoftotalparticlesincluded;a++) {
		String valueofmassfraction=null;
			if(jsonObject.getJSONArray("particle").getJSONObject(a).has("mass_fraction")) {
				valueofmassfraction=String.valueOf(jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("mass_fraction").get("value"));
			}
			else {
				double valueofparticlerate=jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("emission_rate").getDouble("value");
				 valueofmassfraction=String.valueOf(valueofparticlerate/totalparticleemission);
			}
			//logger.info("mass fraction= "+valueofmassfraction);
			double valueofdiameter=jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("diameter").getDouble("value");
			int valueofdensity=jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("density").getInt("value");

			
			if(Double.valueOf(valueofmassfraction)>0) { //maybe later changed to emission rate 
				//logger.info("the particle selected= "+a);
				Individual partialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#Partial-"+a+"OfParticulate-001");
				Individual diameterpartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#Diameter_Partial-"+a+"OfParticulate-001");
				Individual diametervaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_Diameter_Partial-"+a+"OfParticulate-001");
				Individual densitypartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#Density_Partial-"+a+"OfParticulate-001");
				Individual densityvaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_Density_Partial-"+a+"OfParticulate-001");
				Individual massfractionpartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#MassFraction_Partial-"+a+"OfParticulate-001");
				Individual massfractionvaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] +"#V_MassFraction_Partial-"+a+"OfParticulate-001");
				if(partialparticulate==null) {
					partialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#Partial-"+a+"OfParticulate-001",partialparticleclass);
					diameterpartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#Diameter_Partial-"+a+"OfParticulate-001",diameterclass);
					diametervaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#V_Diameter_Partial-"+a+"OfParticulate-001",scalarvalueclass);
					densitypartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#Density_Partial-"+a+"OfParticulate-001",densityclass);
					densityvaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#V_Density_Partial-"+a+"OfParticulate-001",scalarvalueclass);
					massfractionpartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#MassFraction_Partial-"+a+"OfParticulate-001",massfractionclass);
					massfractionvaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#V_MassFraction_Partial-"+a+"OfParticulate-001",scalarvalueclass);
				}
				particulate1.addProperty(hasRepresentativeParticle, partialparticulate);

				
					
					partialparticulate.addProperty(has_length,diameterpartialparticulate);
					
					diameterpartialparticulate.addProperty(hasValue, diametervaluepartialparticulate);
					diametervaluepartialparticulate.addProperty(hasunit, m);
					diametervaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofdiameter*0.000000001));
				
				
				partialparticulate.addProperty(has_density,densitypartialparticulate);
				
				densitypartialparticulate.addProperty(hasValue, densityvaluepartialparticulate);
				densityvaluepartialparticulate.addProperty(hasunit, kg_per_m3);
				densityvaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofdensity));
				
				
				partialparticulate.addProperty(hasProperty,massfractionpartialparticulate);
				
				massfractionpartialparticulate.addProperty(hasValue, massfractionvaluepartialparticulate);
				massfractionvaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofmassfraction));
				//logger.info("mass fraction= "+valueofmassfraction);
			}
		}
		double NO2value=0;
		double NOvalue=0;
		for (int b = 0; b < valueoftotalpollutant; b++) {
			String parametername = jsonObject.getJSONArray("pollutants").getJSONObject(b).getString("name");
			Double parametervalue = jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value")*1000; //(multiplied by 100 temporarily to make it visible)
			
			if(hmap.get(parametername) != null) {
				System.out.println("iri needed= "+iriofchimney.split("#")[0] + "#V_" + hmap.get(parametername) + "_EmissionRate");
				Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(parametername) + "_EmissionRate");
				valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(parametervalue));
				if(parametername.contentEquals("NO2")) {
					NO2value=jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value")*1000;
				}
			}
			else if(parametername.contentEquals("NO")) {
				NOvalue=jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value")*1000;
			}

		}
		Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + "_EmissionRate");
		valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(NO2value+NOvalue));

	}
			
	public void startConversion(String iriOfChimney,JSONObject jsonresultstring) throws Exception {
				
		jenaOwlModel = ModelFactory.createOntologyModel();	
		jenaOwlModel.read(iriOfChimney);
		
		initOWLClasses(jenaOwlModel);

		doConversion(jenaOwlModel,iriOfChimney,jsonresultstring);

		
		String filePath2= iriOfChimney.replaceAll("http://www.theworldavatar.com/kb", "C:/TOMCAT/webapps/ROOT/kb").split("#")[0]; //update the file locally
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
	    logger.info("ship agent is called");
	    
		JSONObject joforrec = AgentCaller.readJsonParameter(request);
		System.out.println("json accepted= "+joforrec.toString());
		String iri = null;
		String mmsi = null;
		try {
			iri = joforrec.optString("reactionmechanism");
			mmsi=joforrec.getJSONObject("ship").get("mmsi").toString(); //only get the mmsi instead of full iri
			//mmsi = joforrec.getString("ship");

		} catch (JSONException e1) {
			logger.error(e1.getMessage(), e1);
			e1.printStackTrace();
		} 
				
		jenaOwlModel = ModelFactory.createOntologyModel();	
			
		File stockDir = new File("C:/TOMCAT/webapps/ROOT/kb/ships/" + mmsi);
		stockDir.mkdir();
		File source = new File("C:/TOMCAT/webapps/ROOT/kb/ships/Chimney-1-temp.owl");
		File destiny = new File("C:/TOMCAT/webapps/ROOT/kb/ships/" + mmsi + "/Chimney-1.owl");
		Charset charset = StandardCharsets.UTF_8;
		String content = new String(Files.readAllBytes(source.toPath()), charset);
		content = content.replaceAll("http://www.theworldavatar.com/kb/ships/Chimney-1.owl",
				"http://www.theworldavatar.com/kb/ships/" + mmsi + "/Chimney-1.owl");
		Files.write(destiny.toPath(), content.getBytes(charset));
				//finished create owl file based on this iri()
		
		
		cpirilist2.add("http://www.theworldavatar.com/kb/ships/Engine-001.owl#Engine-001");

		jenaOwlModel.read("http://www.theworldavatar.com/kb/ships/"+mmsi+"/Chimney-1.owl#Chimney-1");
		
		
		String wasteStreamInfo = "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> " + 
				"PREFIX j7:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> " + 
				"SELECT ?wasteStream \r\n" + 
				"WHERE " + 
				"{ ?chimney j7:hasOutput ?wasteStream ." + 
				"  ?wasteStream a j4:NonReusableWasteProduct ." + 
				"}";
		
		ResultSet rs_chimney = ShipAgent.query(wasteStreamInfo, jenaOwlModel);
		String wasteStreamIRI = null;
		for (; rs_chimney.hasNext();) {			
			QuerySolution qs_p = rs_chimney.nextSolution();

			Resource cpiri = qs_p.getResource("wasteStream");
			wasteStreamIRI = cpiri.toString();
			System.out.println("waste stream iri = " + wasteStreamIRI);
			logger.info("query result3 = " + wasteStreamIRI);
		}
		
		JSONObject jo=null;
		
		try {
			jo = new JSONObject().put("waste", wasteStreamIRI); // waste stream iri
		} catch (JSONException e) {
			e.printStackTrace();
		}
			
		
	    //send the info to SRM Engine Agent

		logger.info("iri gotten= "+iri);
		JSONObject dataSet = new JSONObject();
		try {
			dataSet.put("reactionmechanism",  iri) ;
			dataSet.put("engine", cpirilist2.get(0)) ;	
			dataSet.put("source", "ship") ;
			String resultjson="";
			JSONObject jsonsrmresult=null;
			
			// if there is no reaction mechanism
			if(!iri.contains("theworldavatar")) {
				JSONObject jo2 = new JSONObject();
//				jo2.put("speed",8.0); 
//				jo2.put("type","passenger");
				jo2.put("speed",joforrec.getJSONObject("ship").getDouble("ss"));
				jo2.put("type",joforrec.getJSONObject("ship").get("type").toString().replace("+", " "));
				logger.info("type of ship= "+joforrec.getJSONObject("ship").getString("type").replace("+", " "));
				resultjson = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SLMAgent", jo2.toString());
				jsonsrmresult=new JSONObject(resultjson);
			}
			else {
				resultjson = AgentCaller.executeGet("JPS/SRMAgent", "query", dataSet.toString());
				jsonsrmresult = new JSONObject(resultjson).getJSONObject("results");
			}
	
//			startConversion(cpirilist.get(0),jsonsrmresult);
			startConversion("http://www.theworldavatar.com/kb/ships/"+mmsi+"/Chimney-1.owl#Chimney-1",jsonsrmresult);
			
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		} //convert to update value

		
		logger.info("message to sent back from shipagent = " + jo.toString());
		response.getWriter().write(jo.toString());
	    
		
	    cpirilist2.clear();	
	    
	    
	}
}
