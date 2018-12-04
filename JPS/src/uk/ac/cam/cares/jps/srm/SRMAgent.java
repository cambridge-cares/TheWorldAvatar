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
	
	private void startSRM(String SRMFolderlocation) {
		String startSRMCommand = "C:/Program Files/Kinetics and SRM Engine Suite/x64_SRMDriver.exe -w \"C:\\JPS_DATA\\workingdir\\JPS\\SRM\\\"";
		CommandHelper.executeSingleCommand(SRMFolderlocation, startSRMCommand);
	}
	
	private void startbinaryconverter(String batchFolderlocation,String iri) {
		String startSRMCommand = "C:/Program Files/JPSDeliverables/BatchFiles/ontochemConvertOwlToBin.bat "+iri;
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
	
	
	public static String queryFromRDF4JServer(String mechanismquery) throws JSONException {

		String myHost = "www.theworldavatar.com" ;
		int myPort = 80;
		String myPath = "/sparqlendpoint/query";
		// This specific endpoint loads kb of two plants. 
				
		URIBuilder builder;
		builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPath)
				.setUserInfo("feroz", "password")
				.setParameter("query", mechanismquery)
				.setParameter("output", "json");
		
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
		
		System.out.println("result query= "+mainObj);

		JSONArray bindings = mainObj.getJSONArray("bindings");
		String X = "null"; 
		for(int i = 0; i < bindings.length();i++) {
			
			urimech.add(bindings.getString(i));
		}
			X=urimech.get(9); //(assume it is queried the specific mechanism)
		return X;	
	}
	
	
	

	
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		//System.out.println("joforrec= "+AgentCaller.readJsonParameter(request));
		JSONObject joforrec = AgentCaller.readJsonParameter(request);
		
		String iri = null;

		try {
			//iri = joforrec.getString("reactionmechanism");
			
			//meanwhile changed until zxc change value to query
			String temp= joforrec.getString("value");
			System.out.println("json accepted2= "+temp.toString());
			
			 JSONObject content2 = new JSONObject(temp);
		        
			iri = content2.getString("reactionmechanism");

		} catch (JSONException e1) {
			//logger.error(e1.getMessage(), e1);
			//e1.printStackTrace();
			
			//meanwhile changed until zxc change value to query
			try {
				iri = joforrec.getString("reactionmechanism");
			} catch (JSONException e) {

				logger.error(e.getMessage());
			}
		}
		System.out.println("data got for reaction mechanism= " + iri);

		/** PREPARE ALL THE INPUT FILE*/


		//query from the ontokin based on "iri" to convert the mechanism for creating the binary file
		//-------------------------------------------------------------------------------
		String mechanismfile=null;
		String filename=null;
		if (iri.contains("marinov")) {
			 mechanismfile= "C:/Program Files/Kinetics and SRM Engine Suite/mechanisms/Ethanol Mechanism.xml";
			 filename="chemical_mechanism.bin";
		System.out.println ("SRM is called and mechanism= "+mechanismfile);
		}
		else
		{
			filename="chemical_mechanism2.bin";
		}
		
		//--------------------------------------------------------------------------------
		
		
	    //First, query from ontokin to get the specific reaction mechanism in the format of owl file iri  
		
		//http://www.theworldavatar.com/sparqlendpoint/query?query=PREFIX+rdf%3A+%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E+%0APREFIX+ontochem%3A+%3Chttps%3A%2F%2Fcomo.cheng.cam.ac.uk%2Fkb%2Fontochem.owl%23%3E+%0ASELECT+%3Fx+%0AWHERE+%7B+%0A++++%3Fx+rdf%3Atype+ontochem%3AReactionMechanism+.+%0A%09%7D
		String mechanismquery = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " 
				+ "PREFIX ontochem: <https://como.cheng.cam.ac.uk/kb/ontochem.owl#> "
				+ "SELECT ?x "
				+ "WHERE {?x  rdf:type  ontochem:ReactionMechanism ." 
				+ "}";
		
		//ResultSet rs_mechanism = SRMAgent.queryFromRDF4JServer("http://www.theworldavatar.com:80/sparqlendpoint/query", mechanismquery); 
	
		try {
			rs_mechanism = SRMAgent.queryFromRDF4JServer(mechanismquery);
			System.out.println("result of the total query= "+rs_mechanism);
		} catch (JSONException e1) {

			logger.error(e1.getMessage());

		}

		
		
		//second, run using command prompt the owl file iri using the batch file to produce new bin
		
		//startbinaryconverter("C:/Program Files/JPSDeliverables/BatchFiles",rs_mechanism);
		
		
		//-------------------------------------------------------------------------------
		
		
		// edit the input file
		editXML(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml",null);
		//editXML("D:/JPS/JParkSimulator-git/JPS/workingdir/SRM/InputParams.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml");

		editXML(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml",filename);
		
		
		
		
		/** This part put run to the SRM Engine simulation and take the output */

		startSRM("C:/Program Files/Kinetics and SRM Engine Suite");

		
		String jsonFiledir = AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/OutputCase00001Cyc0001ADMS.json";

		JSONObject dataSet = new JSONObject();
		try {

			dataSet.put("file", dojsonmodif(jsonFiledir));
			response.getWriter().write(dataSet.toString());
		} catch (JSONException e) {
			logger.error(e.getMessage());
		}
	}

	public void editXML(String filepath1, String filepath2, String valueofmech) throws TransformerFactoryConfigurationError {
		try {

			// String filepath = "/JPS/workingdir/ADMS/InputParams.xml";
			// String filepath2 = AgentLocator.getPathToJpsWorkingDir() +
			// "/JPS/SRM/InputParams.xml";
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
					}

					if (a.equals("Chemistry")) {

						updateChemistryBlock(doc, b,"MechFile",valueofmech);
						updateChemistryBlock(doc, b,"ChemModel","5"); //based on user input mechanism
					}
					if (a.equals("fuel")) {

						updateFuelBlock(doc, b,"FuelSpeciesIndices","NC7H16","IC8H18");
						
					}
					if (a.equals("InjectionFuel")) {

						updateFuelBlock(doc, b,"InjSpeciesIndices","NC7H16","IC8H18");
						
					}
				}
			}
			
			// write the content into xml file
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Transformer transformer = transformerFactory.newTransformer();
			DOMSource source = new DOMSource(doc);
			StreamResult result = new StreamResult(new File(filepath2));
			transformer.transform(source, result);

			//System.out.println("Done");

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

	
	public void updateFuelBlock(Document doc, int b, String nameofparameter, String valueofparameter,String valueofparameter2) {
		NodeList list = doc.getElementsByTagName("property_group").item(b).getChildNodes(); // for
																							// general

		for (int i = 0; i < list.getLength(); i++) {
			if (list.item(i).getNodeType() == Node.ELEMENT_NODE) {

				Node node2 = list.item(i);
				Element eElement2 = (Element) node2;

				String a2 = eElement2.getAttribute("ref");

				if (a2.equals(nameofparameter)) {

					Node value = eElement2.getChildNodes().item(1);
					value.setTextContent(valueofparameter);
					Node value2 = eElement2.getChildNodes().item(2);
					value2.setTextContent(valueofparameter2);
				}
			}
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
		public String dojsonmodif(String outputfiledir) throws IOException
		{	
			//the flow is fixed and cannot be changed
		    
			ArrayList <String> newstring = new ArrayList <String> ();
		    String newjsonfile = readFile(outputfiledir);
		    newjsonfile=newjsonfile.replace("/*","_");
		    newjsonfile=newjsonfile.replace("*/","_");
		    
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

		    int x=newjsonfile.split("_").length;
		    System.out.println("size of the array= "+x);
		    for(int a=0;a<x;a+=2)
		    {
		    	newstring.add(newjsonfile.split("_")[a]);
		    }
		    
		    StringBuilder sb = new StringBuilder();
		    for (String s : newstring)
		    {
		        sb.append(s);
		    }

		    System.out.println(sb.toString());
			
			return sb.toString().trim();
		}
}
