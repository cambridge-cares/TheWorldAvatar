package PWServlet_OWL;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
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
	 
	public static String PrAPPWOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPPWOUT.CSV"); // output CSV file from the pr aspen plus model
	
	public void updateData(String target, String newValue) throws SAXException, IOException, ParserConfigurationException, TransformerException
	{
		
	String filename = "C://apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant3.owl";
	String filename2 = "C://apache-tomcat-8.0.24/webapps/ROOT/updated electrical network.owl";
   
   
   
   String []parameter =target.split("_");
   
      
   System.out.println("parameter length= "+parameter.length);
   
   if (parameter.length>1)
   {
   if (target.split("_")[1].equals("Angle")||target.split("_")[1].equals("actualVoltage"))
	   
   {
	    filename = filename2;
   
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
		 System.out.println("ok1");
		 
		 NodeList propertylist = individuals.item(i).getChildNodes();
		  
		
		 for(int t = 0 ; t < propertylist.getLength() ; t ++)
		 {
			 
		 if("system:numericalValue".equals(propertylist.item(t).getNodeName()))
			 
			 {propertylist.item(t).setTextContent(newValue);
			 System.out.println("ok3");
			 
				 
			 }
		  
		  
	   }
 
   }
	   
   }
   
	TransformerFactory transformerFactory = TransformerFactory.newInstance();

	Transformer transformer = transformerFactory.newTransformer();
	DOMSource domSource = new DOMSource(doc);

	StreamResult streamResult = new StreamResult(new File(filename));
	transformer.transform(domSource, streamResult);
		
		
		
	}
	
	

	}

