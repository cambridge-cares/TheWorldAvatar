package uk.ac.cam.cares.jps.srm;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Scanner;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

@WebServlet("/SRMAgent")
public class SRMAgent extends HttpServlet  {

	private static final long serialVersionUID = 2796334308068192311L;
	private Logger logger = LoggerFactory.getLogger(SRMAgent.class);
	ArrayList<String> cpirilist = new ArrayList<String>();
	String rs_mechanism;
	OntModel jenaOwlModel = null;
	
	
	private void startSRM(String SRMFolderlocation) {
		String startSRMCommand = "C:/Program Files/Kinetics and SRM Engine Suite/x64_SRMDriver.exe -w \"C:\\JPS_DATA\\workingdir\\JPS\\SRM\\\"";
		CommandHelper.executeSingleCommand(SRMFolderlocation, startSRMCommand);
	}
	
	private void startbinaryconverter(String batchFolderlocation,String iri) {
		//system.out.println("starting the binary converter");
		String startSRMCommand = "C:/JPS_DATA/workingdir/JPS/SRM/ontokinConvertOwlToBin.bat "+iri;
		CommandHelper.executeSingleCommand(batchFolderlocation, startSRMCommand);
	}
	

	
	
	public static String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
	
	
	public static String queryFromRDF4JServer(String mechanismquery,String iriresult) throws JSONException {

		String myHost = "www.theworldavatar.com" ;
		int myPort = 80;
		String myPath = "/RDF4J_SPARQL_GUI/SPARQLEndpointProxy";
		// This specific endpoint loads kb of two plants. 
				
		URIBuilder builder;
		builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPath)
				//.setUserInfo("feroz", "password")
				.setParameter("queryString	", mechanismquery);
				//.setParameter("output", "json");
		
		String result = executeGet(builder);
		ArrayList <String> urimech= new ArrayList<String>();
		
		JSONArray ja = new JSONArray();
		for (int x=1;x<result.split("\\{").length;x++)
		{

		JSONObject resultobj = new JSONObject();
		resultobj.put("x",result.split("\\{")[x].split("\\}")[0].split("\"\\:")[1].replaceAll("\n", "").replaceAll("\"", "").trim());
		ja.put(resultobj);
		}		

		JSONObject mainObj = new JSONObject();
		mainObj.put("bindings", ja);
		
		//system.out.println("result query= "+mainObj);

		JSONArray bindings = mainObj.getJSONArray("bindings");
		String X = "null"; 
		for(int i = 0; i < bindings.length();i++) {
			
			urimech.add(String.valueOf(bindings.get(i)));

		}
		
		int sizeofar= urimech.size();
		for(int index=0;index<sizeofar;index++) {
		//system.out.println(urimech.get(index));
		//system.out.println("iriresult= "+iriresult);
		
			if (urimech.get(index).contains(iriresult))
		{
			//system.out.println("it goes to query the ontokin");
			X=iriresult;
		}

		}
		urimech.clear();
		return X;
		
	}
	
	public static synchronized ResultSet queryFromOWLFile(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    

		return results;
	}
	

	
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		
 		System.out.println(request.getParameter("query"));
		JSONObject joforrec = AgentCaller.readJsonParameter(request);
		
		String iri = null;
		String iriofengine = null;
		String source="";

		try {
			iri = joforrec.getString("reactionmechanism");
			iriofengine = joforrec.getString("engine");
			source=joforrec.optString("source", "none");

		} catch (JSONException e1) {
			logger.error(e1.getMessage(), e1);
			e1.printStackTrace();
			
			
		}
		//system.out.println("data got for reaction mechanism= " + iri);
		//system.out.println("data got for engine iri= " + iriofengine);

		/** PREPARE ALL THE INPUT FILE*/
		// for PRODUCTION
		cleanDirectory();		
		
		
		if(iri.contains("particle")){
			//String jsonFiledir = AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/OutputCase00001Cyc0001ADMS-valid_v2.json";
			String jsonFiledir = AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/OutputCase00001Cyc0001ADMS-NOx-SOx-O3-PM.json";
			JSONObject json = dojsonmodif(jsonFiledir);
			AgentCaller.writeJsonParameter(response, json);
		
		}
		else {
			//--------------------------------------------------------------------------------
			
			
		    //First, query from ontokin to get the specific reaction mechanism in the format of owl file iri  
			try {
				String mechanismquery = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " 
						+ "PREFIX ontochem: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#> "
						+ "SELECT ?x "
						+ "WHERE {?x  rdf:type  ontochem:ReactionMechanism ." 
						+ "}";
				//rs_mechanism = SRMAgent.queryFromRDF4JServer(mechanismquery,iri); not neccessary at moment
				
			} catch (JSONException e1) {

				logger.error(e1.getMessage());

			}

			
			
			//second, run using command prompt the owl file iri using the batch file to produce new bin
			// for PRODUCTION
//			startbinaryconverter(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM",rs_mechanism); try change not using query on 10/5
			startbinaryconverter(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM",iri);
			
			//-------------------------------------------------------------------------------
			
			//prepareXMLInput(iriofengine); temporarily use the direct input file given
			
				
				
				/** This part put run to the SRM Engine simulation and take the output */

				// for PRODUCTION
				startSRM("C:/Program Files/Kinetics and SRM Engine Suite");
				
				String jsonFiledir = AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/OutputCase00001Cyc0001ADMS.json";
				JSONObject json = dojsonmodif(jsonFiledir);
				AgentCaller.writeJsonParameter(response, json);
		
		
	}

		logger.info("finished successfully");
		
	}

	private void prepareXMLInput(String iriofengine) throws TransformerFactoryConfigurationError {
		//prepare the input engine file from the kb data
		String engineInfo = "PREFIX eng:<http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?No_cyl ?No_exh_val ?No_int_val ?Strokes ?Bore ?CR ?Wrist_pin_offset ?Eng_displ_vol ?Con_rod ?extEGR ?Stroke ?intEGR ?C_to_H ?RON ?RPM ?Int_dia ?Exh_dia ?Int_event_height ?Exh_event_height ?EVO ?EVC ?IVO ?IVC ?AFRstoich ?IniFuelAirEquivRatio ?AFR ?Tman ?Pman ?amfr ?Tex ?Pex "
				+ "WHERE {?entity  a  eng:CompressionIgnitionEngine  ." 
				+ "?entity   eng:numberOfCylinder ?No_cyl ."
				+ "?entity   eng:numberOfExhaustValve ?No_exh_val ."
				+ "?entity   eng:numberOfStroke ?Strokes ."
				+ "?entity   eng:numberOfIntakeValve ?No_int_val ."
				+ "?entity   eng:hasEngineSpeed ?ES ."
				+ "?ES   j2:hasValue ?vES ."
				+ "?vES   j2:numericalValue ?RPM ."
				
				+ "?entity   eng:hasFuel ?fuel ."
				+ "?fuel   eng:hasCtoHRatio ?CHRat ."
				+ "?CHRat   j2:hasValue ?vCHRat ."
				+ "?vCHRat   j2:numericalValue ?C_to_H ."
				+ "?fuel   eng:hasRON ?fuRON ."
				+ "?fuRON   j2:hasValue ?vfuRON ."
				+ "?vfuRON   j2:numericalValue ?RON ."
				
				+ "?entity   j3:realizes ?comb ."
				+ "?comb   eng:hasStoichiometricAirFuelRatio ?SAFR ."
				+ "?SAFR   j2:hasValue ?vSAFR ."
				+ "?vSAFR   j2:numericalValue ?AFRstoich ."
				+ "?comb   eng:hasInitialFuelAirRatio ?IFAR ."
				+ "?IFAR   j2:hasValue ?vIFAR ."
				+ "?vIFAR   j2:numericalValue ?IniFuelAirEquivRatio ."
				+ "?comb   eng:hasAirFuelRatio ?OvAFR ."
				+ "?OvAFR   j2:hasValue ?vOvAFR ."
				+ "?vOvAFR   j2:numericalValue ?AFR ."
				
				+ "?comb   j4:hasInput ?airstream ."
				+ "?airstream   j5:refersToGeneralizedAmount ?genamountairstream ."
				+ "?genamountairstream   j2:hasSubsystem ?matamountairstream ."
				+ "?matamountairstream   j6:refersToMaterial ?matairstream ."
				+ "?matairstream   j7:thermodynamicBehavior ?phaseairstream ."
				+ "?phaseairstream   j8:has_pressure ?pressureairstream ."
				+ "?pressureairstream   j2:hasValue ?vpressureairstream ."
				+ "?vpressureairstream   j2:numericalValue ?Pman ."
				+ "?phaseairstream   j8:has_temperature ?temperatureairstream ."
				+ "?temperatureairstream   j2:hasValue ?vtemperatureairstream ."
				+ "?vtemperatureairstream   j2:numericalValue ?Tman ."
				+ "?matamountairstream   j2:hasProperty ?amfrairstream ."
				+ "?amfrairstream  j2:hasValue ?vamfrairstream ."
				+ "?vamfrairstream   j2:numericalValue ?amfr ."
				
				+ "?comb   j4:hasOutput ?wastestream ."
				+ "?wastestream   j5:refersToGeneralizedAmount ?genamountwastestream ."
				+ "?genamountwastestream   j2:hasSubsystem ?matamountwastestream ."
				+ "?matamountwastestream   j6:refersToMaterial ?matwastestream ."
				+ "?matwastestream   j7:thermodynamicBehavior ?phasewastestream ."
				+ "?phasewastestream   j8:has_pressure ?pressurewastestream ."
				+ "?pressurewastestream   j2:hasValue ?vpressurewastestream ."
				+ "?vpressurewastestream   j2:numericalValue ?Pex ."
				+ "?phasewastestream   j8:has_temperature ?temperaturewastestream ."
				+ "?temperaturewastestream   j2:hasValue ?vtemperaturewastestream ."
				+ "?vtemperaturewastestream   j2:numericalValue ?Tex ."
				
				+ "?entity   j2:hasSubsystem ?cyl ."
				+ "?cyl   eng:hasWristPinOffset ?WPoff ."
				+ "?WPoff   j2:hasValue ?vWPoff ."
				+ "?vWPoff   j2:numericalValue ?Wrist_pin_offset ."
				+ "?cyl   eng:hasBore ?borecyl ."
				+ "?borecyl   j2:hasValue ?vborecyl ."
				+ "?vborecyl   j2:numericalValue ?Bore ."
				+ "?cyl   eng:hasInternalEGR ?interegr ."
				+ "?interegr   j2:hasValue ?vinteregr ."
				+ "?vinteregr   j2:numericalValue ?intEGR ."
				+ "?cyl   eng:hasExternalEGR ?exteregr ."
				+ "?exteregr   j2:hasValue ?vexteregr ."
				+ "?vexteregr   j2:numericalValue ?extEGR ."
				+ "?cyl   eng:hasDisplacementVolume ?dispvolume ."
				+ "?dispvolume   j2:hasValue ?vdispvolume ."
				+ "?vdispvolume   j2:numericalValue ?Eng_displ_vol ."
				+ "?cyl   eng:hasConnectingRodLength ?crlength ."
				+ "?crlength   j2:hasValue ?vcrlength ."
				+ "?vcrlength   j2:numericalValue ?Con_rod ."
				+ "?cyl   eng:hasStrokeDistance ?SD ."
				+ "?SD   j2:hasValue ?vSD ."
				+ "?vSD   j2:numericalValue ?Stroke ."
				+ "?cyl   eng:hasCompressionRatio ?CompR ."
				+ "?CompR   j2:hasValue ?vCompR ."
				+ "?vCompR   j2:numericalValue ?CR ."

				
				+ "?entity   j2:hasSubsystem ?exv ."
				+ "?exv   eng:hasOpeningConditionAngle ?opexv ."
				+ "?opexv   j2:hasValue ?vopexv ."
				+ "?vopexv   j2:numericalValue ?EVO ."
				+ "?exv   eng:hasClosingConditionAngle ?clexv ."
				+ "?clexv   j2:hasValue ?vclexv ."
				+ "?vclexv   j2:numericalValue ?EVC ."
				+ "?exv   eng:hasExhaustValveDiameter ?Dexv ."
				+ "?Dexv   j2:hasValue ?vDexv ."
				+ "?vDexv   j2:numericalValue ?Exh_dia ."
				+ "?exv   eng:hasExhaustEventHeight ?Hexv ."
				+ "?Hexv   j2:hasValue ?vHexv ."
				+ "?vHexv   j2:numericalValue ?Exh_event_height ."
				
				+ "?entity   j2:hasSubsystem ?inv ."
				+ "?inv   eng:hasOpeningConditionAngle ?opinv ."
				+ "?opinv   j2:hasValue ?vopinv ."
				+ "?vopinv   j2:numericalValue ?IVO ."
				+ "?inv   eng:hasClosingConditionAngle ?clinv ."
				+ "?clinv   j2:hasValue ?vclinv ."
				+ "?vclinv   j2:numericalValue ?IVC ."
				+ "?inv   eng:hasIntakeValveDiameter ?Dinv ."
				+ "?Dinv   j2:hasValue ?vDinv ."
				+ "?vDinv   j2:numericalValue ?Int_dia ."
				+ "?inv   eng:hasIntakeEventHeight ?Hinv ."
				+ "?Hinv   j2:hasValue ?vHinv ."
				+ "?vHinv   j2:numericalValue ?Int_event_height ."
				
				
				+ "}";
		
		
		
		String engineInfoclass = "PREFIX eng:<http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?OpMode "
				+ "WHERE {?entity   eng:numberOfCylinder ?No_cyl ."
				+ "?entity  a ?OpMode ."
				+"FILTER regex(STRBEFORE(STR(?OpMode),\"#\"), \"http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl\") ."
				+ "}";
		
		jenaOwlModel = ModelFactory.createOntologyModel();	
		jenaOwlModel.read(iriofengine, null);
		
ResultSet rs_engine = SRMAgent.queryFromOWLFile(engineInfo,jenaOwlModel); 

String []a= {"No_cyl","No_exh_val","No_int_val","Strokes","C_to_H","RON","RPM","Int_dia","Exh_dia","Int_event_height","Exh_event_height","EVO","EVC","IVO","IVC","AFRstoich","IniFuelAirEquivRatio","AFR","Tman","Pman","Tex","Pex","Wrist_pin_offset","Bore","intEGR","extEGR","Eng_displ_vol","Con_rod","Stroke","CR","amfr"};
int sizea=a.length;
//system.out.println("size of a= "+sizea);
String valueiri=null;
SRMAgent xmlreader = new SRMAgent();
for (; rs_engine.hasNext();) {			
		QuerySolution qs_p = rs_engine.nextSolution();
		for (int b=0;b<sizea;b++)
		{

				Literal cpiri = qs_p.getLiteral(a[b]); // extract the name of the source
				 valueiri = cpiri.toString();

		//system.out.println(a[b]+" = "+valueiri);
		try {

			xmlreader.editXMLForengine(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml",a[b],valueiri);
		} catch (TransformerFactoryConfigurationError | TransformerException e) {
						
			logger.error(e.getMessage());
		}

		logger.info("query result1= "+valueiri);

		}
}

ResultSet rs_engineclass = SRMAgent.queryFromOWLFile(engineInfoclass,jenaOwlModel); 
String valueiri2=null;
String valuetype2=null;
for (; rs_engineclass.hasNext();) {			
		QuerySolution qs_p = rs_engineclass.nextSolution();

		
			Resource cpiri = qs_p.getResource("OpMode"); // extract the name of the source
			valueiri2 = cpiri.toString();
			if(valueiri2.contains("http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl#")&&!valueiri2.contains("#Engine")){
				valuetype2=("CI");
				//system.out.println("query result1= "+valuetype2);
				try {
					xmlreader.editXMLForengine(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml","OpMode",valuetype2);
					} 
				catch (TransformerFactoryConfigurationError | TransformerException e) {
					logger.error(e.getMessage());
				}
		
				}	

		}
// edit the input params file

			editinputparamXML(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml","mechanism.bin");
	}

	private void cleanDirectory() {
		//	clear the folder SRM first
		File folder = new File("C:/JPS_DATA/workingdir/JPS/SRM");
				
		File[] listOfFiles = folder.listFiles();

		if (listOfFiles != null) {
			for (int i = 0; i < listOfFiles.length; i++) {
				if (listOfFiles[i].isFile() && !listOfFiles[i].getName().equals("ontokin.jar")
						&& !listOfFiles[i].getName().equals("InputParams.xml")
						&& !listOfFiles[i].getName().equals("InputEngineML.xml")
						&& !listOfFiles[i].getName().equals("OutputCase00001Cyc0001ADMS-valid_v2.json")
						&& !listOfFiles[i].getName().equals("OutputCase00001Cyc0001ADMS-NOx-SOx-O3-PM.json")
						&& !listOfFiles[i].getName().equals("convert.exe")
						&& !listOfFiles[i].getName().equals("ontokinConvertOwlToBin.bat")) {

					listOfFiles[i].delete();
				}
			}
		}
	}

	public void editinputparamXML(String filepath1, String filepath2, String valueofmech) throws TransformerFactoryConfigurationError {
		try {

			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(filepath1);
			
			NodeList propgroup=doc.getElementsByTagName("property_group");
			
			for (int b = 0; b < propgroup.getLength(); b++) {
				if (propgroup.item(b).getNodeType() == Node.ELEMENT_NODE) {
					Node node = propgroup.item(b);
					Element eElement = (Element) node;

					String a = eElement.getAttribute("ref");

					if (a.equals("general")) {

						updateChemistryBlock(doc, b,"ADMSOutput","1");
						updateChemistryBlock(doc, b,"MomentsPostProcessOutput","1"); //(only for sooth later)
					}

					if (a.equals("Chemistry")) {

						updateChemistryBlock(doc, b,"MechFile",valueofmech);
						updateChemistryBlock(doc, b,"ChemModel","5"); //based on user input mechanism
					}
					if(a.equals("soot")) {
						updateChemistryBlock(doc, b,"PopBalModel","2"); //(only for sooth later)
					}
					if (a.equals("NanoModelPostProcess")) {
						updateChemistryBlock(doc, b,"Number_KDE_point","20"); //(only for sooth later)
					}
					if (a.equals("NanoModel")) {
						updateChemistryBlock(doc, b,"NanoModelFlag","1"); //(only for sooth later)
					}

					if (a.equals("fuel")) {

						//updateFuelBlock(doc, b,"InjSpeciesIndices","NC7H16","IC8H18");
						
					}
				}
			}
			
			// write the content into xml file
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Transformer transformer = transformerFactory.newTransformer();
			DOMSource source = new DOMSource(doc);
			StreamResult result = new StreamResult(new File(filepath2));
			transformer.transform(source, result);

			////system.out.println("Done");

		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		} catch (TransformerException tfe) {
			tfe.printStackTrace();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		} catch (SAXException sae) {
			sae.printStackTrace();
		}
	}

	public void updateChemistryBlock(Document doc, int b, String nameofparameter, String valueofparameter) {
		NodeList list = doc.getElementsByTagName("property_group").item(b).getChildNodes(); // for
																							// chemistry

		for (int i = 0; i < list.getLength(); i++) {
			if (list.item(i).getNodeType() == Node.ELEMENT_NODE) {

				Node node2 = list.item(i);
				Element eElement2 = (Element) node2;

				String a2 = eElement2.getAttribute("ref");

				if (a2.equals(nameofparameter)) {

					Node value = eElement2.getChildNodes().item(1);
					value.setTextContent(valueofparameter);
				}
			}
		}
	}

	
	public void editXMLForengine(String filepath1,String filepath2,String propname ,String datavalue) throws TransformerFactoryConfigurationError, TransformerException {
		try {

			// String filepath = "/JPS/workingdir/ADMS/InputParams.xml";
			// String filepath2 = AgentLocator.getPathToJpsWorkingDir() +
			// "/JPS/SRM/InputParams.xml";
			

			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(filepath1);
			
			NodeList propgroup=doc.getElementsByTagName("property");
						
			for (int b = 0; b < propgroup.getLength(); b++) {
				if (propgroup.item(b).getNodeType() == Node.ELEMENT_NODE) {
					Node node = propgroup.item(b);
					Element eElement = (Element) node;

					String a = eElement.getAttribute("xsi:type");

					////system.out.println("a= "+a);

						Node value = eElement.getChildNodes().item(1);
						if (a.contentEquals(propname))
						{
							//system.out.println(a+" is updated!!!!");
						value.setTextContent(datavalue);
						}
				}
			}
			
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Transformer transformer = transformerFactory.newTransformer();
			DOMSource source = new DOMSource(doc);
			StreamResult result = new StreamResult(new File(filepath2));
			transformer.transform(source, result);

		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		}  catch (IOException ioe) {
			ioe.printStackTrace();
		} catch (SAXException sae) {
			sae.printStackTrace();
		}

	}
	
	
	private String readFile(String pathname) throws IOException {

	    File file = new File(pathname);
	    StringBuilder fileContents = new StringBuilder((int)file.length());        

	    try (Scanner scanner = new Scanner(file)) {
	        while(scanner.hasNextLine()) {
	            fileContents.append(scanner.nextLine() + System.lineSeparator());
	        }
	        return fileContents.toString();
	    }
	}
		
	//to convert the file of adms output in srm to the string of corrected json format
	public JSONObject dojsonmodif(String outputfiledir) throws IOException {	
		//the flow is fixed and cannot be changed
	    
		ArrayList <String> newstring = new ArrayList <String> ();
	    String newjsonfile = readFile(outputfiledir);
	    
	    
	    newjsonfile=newjsonfile.replace("/*","a_a");
	    newjsonfile=newjsonfile.replace("*/","a_a");
	    
	    
	    //remove the "mixture" part error except the last part
	    newjsonfile=newjsonfile.replace("\",\r\n" + 
	    		"        }","\"\r\n" + 
	    				"        },");
	  //remove the "pollutant" part error except the last part
	    newjsonfile =newjsonfile.replaceAll("\",\r\n" + 
	    		"            }","\"\r\n" + 
	    	    		"            },");
	     
	    //move the comma to the last } for pollutant
	      newjsonfile=newjsonfile.replace("},\r\n" + 
	    		"        }\r\n" + 
	    		"        {","}\r\n" + 
	    				"        },\r\n" + 
	    				"        {");
	    
	    //move the comma to the last } for mixture
	    newjsonfile=newjsonfile.replace("},\r\n" + 
	    		"    }","}\r\n" + 
	    				"    },");
	    
	    
	    //delete the comma for the last } in json
	    newjsonfile=newjsonfile.replace("},\r\n" + 
	    		"        }\r\n" + 
	    		"    ]","}\r\n" + 
	    				"        }\r\n" + 
	    				"    ]");
	    
	    
	    //make comma after ] in json (but not the last)
	    newjsonfile=newjsonfile.replace("]\r\n" + 
	    		"    \"", "],\r\n" + 
	    				"    \"");

	    int x=newjsonfile.split("a_a").length;
	    for(int a=0;a<x;a+=2)
	    {
	    	newstring.add(newjsonfile.split("a_a")[a]);
	    }
	    
	    StringBuilder sb = new StringBuilder();
	    for (String s : newstring)
	    {
	        sb.append(s);
	    }

	    //System.out.println("result after modification= "+sb.toString());
	    logger.info("result after modification in logger= "+sb.toString());
	    
	    
		JSONObject dataSet = new JSONObject(sb.toString());
		
		
		JSONObject result = new JSONObject();
		result.put("results", dataSet);
		return result;
	}
}
