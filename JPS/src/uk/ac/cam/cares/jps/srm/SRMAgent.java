package uk.ac.cam.cares.jps.srm;

import java.io.File;
import java.io.IOException;
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
import uk.ac.cam.cares.jps.base.util.CommandHelper;

@WebServlet("/SRMAgent")
public class SRMAgent extends HttpServlet  {

	private static final long serialVersionUID = 2796334308068192311L;
	private Logger logger = LoggerFactory.getLogger(SRMAgent.class);
	
	private void startSRM(String SRMFolderlocation) {
		String startSRMCommand = "C:/Program Files/Kinetics and SRM Engine Suite/x64_SRMDriver.exe -w \"C:\\JPS_DATA\\workingdir\\JPS\\SRM\\\"";
		CommandHelper.executeSingleCommand(SRMFolderlocation, startSRMCommand);
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
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		System.out.println("data got for reaction mechanism= " + iri);

		/** PREPARE ALL THE INPUT FILE*/
		
		// edit the input file
		editXML(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputEngineML.xml");
		//editXML("D:/JPS/JParkSimulator-git/JPS/workingdir/SRM/InputParams.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml");

		editXML(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml",AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/InputParams.xml");

		//query from the ontokin based on "iri" to convert the mechanism for creating the binary file
		//-------------------------------------------------------------------------------
		String mechanismfile=null;
		if (iri.contains("marinov")) {
			 mechanismfile= "C:/Program Files/Kinetics and SRM Engine Suite/mechanisms/Ethanol Mechanism.xml";
		System.out.println ("SRM is called and mechanism= "+mechanismfile);
		}
		
		//--------------------------------------------------------------------------------
		
		
	    //First, query from ontokin to get the specific reaction mechanism in the format of owl file iri  
		//http://www.theworldavatar.com/sparqlendpoint/query?query=PREFIX+rdf%3A+%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E+%0APREFIX+ontochem%3A+%3Chttps%3A%2F%2Fcomo.cheng.cam.ac.uk%2Fkb%2Fontochem.owl%23%3E+%0ASELECT+%3Fx+%0AWHERE+%7B+%0A++++%3Fx+rdf%3Atype+ontochem%3AReactionMechanism+.+%0A%09%7D
		//second, run using command prompt the owl file iri using the batch file to produce new bat
		
		//replace the old bat with the new one
		
		//-------------------------------------------------------------------------------
		
		
		/** This part put run to the SRM Engine simulation and take the output */

		startSRM("C:/Program Files/Kinetics and SRM Engine Suite");

		
		String jsonFiledir = AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/OutputCase00001Cyc0001ADMS.json";

		JSONObject dataSet = new JSONObject();
		try {

			dataSet.put("file", dojsonmodif(jsonFiledir));
			response.getWriter().write(dataSet.toString());
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void editXML(String filepath1, String filepath2) throws TransformerFactoryConfigurationError {
		try {

			// String filepath = "/JPS/workingdir/ADMS/InputParams.xml";
			// String filepath2 = AgentLocator.getPathToJpsWorkingDir() +
			// "/JPS/SRM/InputParams.xml";
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(filepath1);

			// Get the root element

			// Get the staff element , it may not working if tag has spaces, or
			// whatever weird characters in front...it's better to use
			// getElementsByTagName() to get it directly.
			// Node staff = company.getFirstChild();

			// Get the staff element by tag name directly
			//Node propgroup = doc.getElementsByTagName("property_group").item(0);
			
/*			NodeList list = doc.getElementsByTagName("property_group").item(0).getChildNodes(); //for general

			for (int i = 0; i < list.getLength(); i++) {
				if (list.item(i).getNodeType() == Node.ELEMENT_NODE) {

					Node node = list.item(i);
					Element eElement = (Element) node;

					String a = eElement.getAttribute("ref");

					if (a.equals("ADMSOutput")) {

						Node value = eElement.getChildNodes().item(1);
						value.setTextContent("1");
					}
				}
			}*/
			
/*			NodeList list2 = doc.getElementsByTagName("property_group").item(3).getChildNodes(); //for chemistry

			for (int i = 0; i < list2.getLength(); i++) {
				if (list2.item(i).getNodeType() == Node.ELEMENT_NODE) {

					Node node = list2.item(i);
					Element eElement = (Element) node;

					String a = eElement.getAttribute("ref");

					if (a.equals("MechFile")) {

						Node value = eElement.getChildNodes().item(1);
						value.setTextContent("chemical_mechanism.bin");
					}
				}
			}*/
			
			NodeList propgroup=doc.getElementsByTagName("property_group");
			
			for (int b = 0; b < propgroup.getLength(); b++) {
				if (propgroup.item(b).getNodeType() == Node.ELEMENT_NODE) {
					Node node = propgroup.item(b);
					Element eElement = (Element) node;

					String a = eElement.getAttribute("ref");

					if (a.equals("general")) {

						updateGeneralBlock(doc, b,"ADMSOutput","1");
					}

					if (a.equals("Chemistry")) {

						updateChemistryBlock(doc, b,"MechFile","chemical_mechanism.bin");
					}
				}
			}
			
//			Node admstag = propgroup.getChildNodes().item(38);
//			System.out.println("node admstag= "+admstag.getNodeName());
//			Node value = admstag.getChildNodes().item(0);
//			//value.getAttributes().
//			value.setTextContent("1");

			// append a new node to staff
			/*
			 * Element age = doc.createElement("age");
			 * age.appendChild(doc.createTextNode("28")); staff.appendChild(age);
			 */

			// loop the staff child node
			/*
			 * NodeList list = staff.getChildNodes();
			 * 
			 * for (int i = 0; i < list.getLength(); i++) {
			 * 
			 * Node node = list.item(i);
			 * 
			 * // get the salary element, and update the value if
			 * ("salary".equals(node.getNodeName())) { node.setTextContent("2000000"); }
			 * 
			 * //remove firstname if ("firstname".equals(node.getNodeName())) {
			 * staff.removeChild(node); }
			 * 
			 * }
			 */

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

	public void updateGeneralBlock(Document doc, int b, String nameofparameter, String valueofparameter) {
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
