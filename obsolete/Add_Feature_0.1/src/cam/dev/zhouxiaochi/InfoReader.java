package cam.dev.zhouxiaochi;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class InfoReader {
	public static double center_x ;
	public static double center_y;
	
	 
	public static double scale;
	
	
	
	public static void readXML(String type, String name) throws ParserConfigurationException, SAXException, IOException
	{
		   File inputFile = new File("ARCGIS/"+  type + ".xml");
	       DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
	       DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
	       Document doc = dBuilder.parse(inputFile);
	       doc.getDocumentElement().normalize(); 
	       Element root = doc.getDocumentElement();
	       NodeList featureList = root.getElementsByTagName("mFeature");
	        
 
	   
	    
	    		 
		    		  String center = featureList.item(0).getChildNodes().item(9).getTextContent();
		    		  String c_x = center.split(",")[0];
		    		  String c_y = center.split(",")[1];
		    		  center_x = Double.parseDouble(c_x)	;
		    		  center_y = Double.parseDouble(c_y);
		    		  
		    		
		    		  
		    		  
		    		  
		 
		
	}
	
}
