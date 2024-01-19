package cam.dev.zhouxiaochi;

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
	
	String filename;
	 
	
	public void updateData(String target, String newValue, int filecode) throws SAXException, IOException, ParserConfigurationException, TransformerException
	{
		if (filecode==1)
		{
          filename = "owl/storagetankcomplete.owl";
		}
		
		else if (filecode==2)
		{
			filename = "owl/buildingmodif2.owl";
		}
		
		else
		{
			filename = "owl/BiodieselPlant3.owl";
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
