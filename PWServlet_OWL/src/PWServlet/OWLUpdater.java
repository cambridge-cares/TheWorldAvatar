package PWServlet;

import java.io.File;
import java.io.IOException;

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
	 
	
	public void updateData(String target, String newValue) throws SAXException, IOException, ParserConfigurationException, TransformerException
	{
		
	String filename = "BiodieselPlant3.owl";
   
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
		 
		 
		  individuals.item(i).setTextContent(newValue);
		  
	   }
 
   }
	TransformerFactory transformerFactory = TransformerFactory.newInstance();

	Transformer transformer = transformerFactory.newTransformer();
	DOMSource domSource = new DOMSource(doc);

	StreamResult streamResult = new StreamResult(new File(filename));
	transformer.transform(domSource, streamResult);
		
		
		
	}
	
	

}
