package PWServlet_OWL;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


public class OWLUpdater {
	String filename= "C:\\BiodieselPlant3.owl";
	 
	
	public void updateData(String target, String newValue, String layername, int ObjectID) throws Exception
	{
		 String[]modif= target.split("_");
			//String lastone = modif[modif.length-1];
		for (int y=0; y<modif.length ;y++)
		{
		 if(modif[y].contentEquals("LoadPoint"))
		{
			filename="C:\\updated electrical network.owl";
		}
		 if(target.contains("1-")|| target.contains("-10"))
			{
				filename="C:\\BiodieselPlant1WOWHR.owl";
			}
		 if(target.contains("2-")|| target.contains("-20"))
			{
				filename="C:\\BiodieselPlant2WWHR.owl";
			}
		}	
	
   
   File inputFile = new File(filename);
   DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
   DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
   Document doc = dBuilder.parse(inputFile);
   doc.getDocumentElement().normalize();
   Element root = doc.getDocumentElement();
   NodeList individuals = root.getElementsByTagName("owl:NamedIndividual");
   
   for(int i = 0 ; i < individuals.getLength() ; i ++)
   {
	   String name = individuals.item(i).getAttributes().item(0).getNodeValue();
	   name = name.substring(name.indexOf("#") + 1);
	    
	   if(name.contentEquals(target))
	   {
	
		   NodeList children = individuals.item(i).getChildNodes();
		   for(int j = 0 ; j < children.getLength(); j++)
		   {
			   if(children.item(j).getNodeName().contentEquals("system:numericalValue"))
			   {
				   children.item(j).setTextContent(newValue);
 			   }

		   }
		  
		  
	   }
 
	   
	   
   }
   
   
   
   
   
   
	TransformerFactory transformerFactory = TransformerFactory.newInstance();

	Transformer transformer = transformerFactory.newTransformer();
	DOMSource domSource = new DOMSource(doc);

	StreamResult streamResult = new StreamResult(new File(filename));
	transformer.transform(domSource, streamResult);
		
	String result_string = readData(target);
	

	  ArrayList<String[]> result = new ArrayList<String[]>();
	  //InputStream input = new URL( "http://172.25.182.41/" + filename ).openStream();
	  
	  String processed_target = target.replace("-", "_");
	       
	   	 Map<Integer, String> oldNewAttriNameDictionary = new HashMap<Integer, String>();
		    	 oldNewAttriNameDictionary.put(22, processed_target);
		    	 
	    

	      String[] pair2 = {layername,processed_target,result_string};
	      result.add(pair2);
	       
	      FeatureServiceUpdater mUpdater = new FeatureServiceUpdater("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/jpsimulator/FeatureServer");
	      String[] entityNameList= {layername};
	      Map<Integer,Map<String, String>> nameValueListPerLayer = new HashMap<Integer, Map<String, String>>();
	     
	      HashMap<String, String>   parameters301 = new HashMap<String, String>();
	      HashMap<String, String>   parameters302 = new HashMap<String, String>();

	      
	      System.out.println(result_string);
	      parameters302.put(oldNewAttriNameDictionary.get(22),result_string);
	      nameValueListPerLayer.put(0, parameters302);

	      
	       mUpdater.updateFeaturesInTable(entityNameList, nameValueListPerLayer,ObjectID);
	
		
	}
	
	
	public String readData(String target) throws IOException, Exception
	{
		 String[]modif= target.split("_");
			//String lastone = modif[modif.length-1];
		for (int y=0; y<modif.length ;y++)
		{
		 if(modif[y].equals("LoadPoint"))
		{
			filename="C:\\updated electrical network.owl";
		}
		 if(target.contains("1-")|| target.contains("-10"))
			{
				filename="C:\\BiodieselPlant1WOWHR.owl";
			}
		 if(target.contains("2-")|| target.contains("-20"))
			{
				filename="C:\\BiodieselPlant2WWHR.owl";
			}
		}	
		return OWLFileReader.read_owl_file(filename, target);
	}
	

}
