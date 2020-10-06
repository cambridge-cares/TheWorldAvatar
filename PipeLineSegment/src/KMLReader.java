

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.json.JSONException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.esri.core.geometry.CoordinateConversion;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.FeatureEditResult;
import com.esri.core.map.Graphic;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.map.ArcGISFeatureLayer;

public class KMLReader {
	
	public static ArrayList<ArrayList<Point>> coordinates = new ArrayList<ArrayList<Point>>();
	public static ArrayList<String> buildingList = new ArrayList<String>();
	public static ArrayList<Point[]> pipeLineCoordinates = new ArrayList<Point[]>();
	public static String[] nameList;

	
	
	
	
	public void readkml(ArcGISFeatureLayer buildinglayer) throws Exception
	{
		
		   pipeLineCoordinates.clear();
		  
		 	File inputFile = new File("Material_Line.kml");
			
	        DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
	        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
	        Document doc = dBuilder.parse(inputFile);
	        doc.getDocumentElement().normalize(); 
	        Element root = doc.getDocumentElement();
	        NodeList featureList = root.getElementsByTagName("coordinates");
	        
	        for(int i = 0; i < featureList.getLength(); i++)
	        {
 	        	String value = featureList.item(i).getTextContent();
 
 	            
	        	int length = value.split(",100\n").length;
	        	String[] string_array = new String[length];
	        	string_array = value.split(",100\n");
	        	
	        	ArrayList<Point> temp = new ArrayList<Point>();
	        	
	        	for(String point : string_array)
	        	{
	        		String x_string =  point.split(",")[0].trim();
	        		String y_string =  point.split(",")[1].trim();
	        		
	        		Point thisPoint = CoordinateConversion.decimalDegreesToPoint(y_string+ "N " + x_string + "E", buildinglayer.getDefaultSpatialReference());
	        		temp.add(thisPoint);
	        	}
	        	
	        	coordinates.add(temp);
	        	
	        	
	        	
	        	
	        	String start_string = string_array[0];
	        	String end_string = string_array[length - 1];
	  
	        	/**
	        	System.out.println("----------Start & End ---------");
	        	System.out.println(start_string);
	        	System.out.println(end_string);
	        	**/
	        	
	        	
	        	String x_string =  start_string.split(",")[0].trim();
        		String y_string =  start_string.split(",")[1].trim();
        		
        		
        		
        		Point startpoint = CoordinateConversion.decimalDegreesToPoint(y_string+ "N " + x_string + "E", buildinglayer.getDefaultSpatialReference());
        	
        		/**
        		System.out.println(x_string);
        		System.out.println(y_string);
        		**/
        		
        	  	x_string =  start_string.split(",")[0].trim();
        		y_string =  end_string.split(",")[1].trim();
        		Point endpoint = CoordinateConversion.decimalDegreesToPoint(y_string+ "N " + x_string + "E", buildinglayer.getDefaultSpatialReference());
        	
        		Point[] start_end = new Point[2];
        		
        		
        		
        		start_end[0] = startpoint;
        		start_end[1] = endpoint;
        		
        		/**
        		System.out.println("--" + startpoint.getX());
        		System.out.println("--" + startpoint.getY());
        		System.out.println("--" + endpoint.getX());
        		System.out.println("--" + endpoint.getY());
        		**/
        		pipeLineCoordinates.add(start_end);
        		 
	        }
	}
}
