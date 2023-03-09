package PWServlet;


import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class OWLFileReader {
	public static String name;
	public static NodeList individuals;
	public static Node targetNode;
	public static String result; 
	public static String read_owl_file (String filename , String target) throws Exception, IOException {
		
	if(filename == null)
	{
		filename = "C:\\BiodieselPlant3.owl";
	} 
	
   File inputFile = new File(filename);
   DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
   DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
   Document doc = dBuilder.parse(inputFile);
   doc.getDocumentElement().normalize();
   Element root = doc.getDocumentElement();
   individuals = root.getElementsByTagName("owl:NamedIndividual");
   
   for(int i = 0 ; i < individuals.getLength() ; i ++)
   {
	   String name = individuals.item(i).getAttributes().item(0).getNodeValue();
	   name = name.substring(name.indexOf("#") + 1);
	   System.out.println(name);
	   if(name.contentEquals(target))
	   {
		   targetNode = individuals.item(i);
	   }
   }
   
   NodeList children = targetNode.getChildNodes();
   for(int j = 0 ; j < children.getLength(); j++)
   {
	   if(children.item(j).getNodeName().contentEquals("system:numericalValue"))
	   {
		   result = children.item(j).getTextContent();
		   System.out.println(result);
	   }
 
	    
   }
return result;
   
   
	}

}
