
import com.esri.runtime.ArcGISRuntime;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.esri.core.geometry.CoordinateConversion;
import com.esri.core.geometry.CoordinateConversion.UTMConversionMode;
import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.Geometry;
import com.esri.core.geometry.GeometryEngine;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
import com.esri.core.geometry.SpatialReference;
import com.esri.core.geometry.Transformation2D;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.FeatureSet;
import com.esri.core.map.Graphic;
import com.esri.core.renderer.SimpleRenderer;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.core.tasks.ags.query.Query;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.ArcGISFeatureLayer.SELECTION_METHOD;
import com.esri.map.ArcGISTiledMapServiceLayer;
import com.esri.map.JMap;
import com.esri.map.LayerList;
import com.esri.map.MapOptions;
import com.esri.map.MapOptions.MapType;
import com.esri.map.MapOverlay;
import com.esri.map.QueryMode;



public class FeatureWriter {

	public static double delta_x;
	public static double delta_y;
	
	public static double _x;
	public static double _y;
	
	public static double center_x;
	public static double center_y;
	
	public static String name;
	public static int angle = 45;
	
	
	public static double x_min;
	public static double x_max;
 
	
	public static double y_min;
	public static double y_max;
	
	
	
 
	public static double length1 ;
	public static double length2;
	public static double beta1;
	public static double beta2;
	 
 
	public static Point p_left_button = new Point (11541440.87421663,140088.23280191634);
 	
	public static double bioplant_length = 0;
	public static double bioplant_width  = 0;
	
 	public static ArrayList<Point> CoordinateList = new ArrayList<Point>();
	
	
	/***
	 * Create a feature basing on the coordinates, type and spatial reference
	 * @param type  type of the feature e.g. Blower
	 * @param x		the longitude of the feature (decimal coordinate)
	 * @param y		the latitude    of the feature (decimal coordinate)
	 * @param ref    the spatial reference of the layer
	 *  @return Graphic[] the array of features to be added
	 * @throws Exception 
	 */
	public static void createFeature(String type , double x , double y,SpatialReference ref, String input_name ) throws Exception
	{
		CoordinateList.clear();
		
		bioplant_length = 35;
		bioplant_width  =  30;
		
		 
 
	 	InfoReader.readXML(type, input_name); // read the XML file to get the center coordinate of the according device, type: e.g. Pump , input_name: e.g. P-301
 
	 	
	 	
	 	center_x = InfoReader.center_x;
	 	center_y = InfoReader.center_y;
  
		 // ----------------------------------- read the prefab xml , get the coordinates and add delta to make the new geometry
		File inputFile = new File("Prefab/"+ type + ".xml");  
		
        DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(inputFile);
        doc.getDocumentElement().normalize(); 
        Element root = doc.getDocumentElement();
        Node coordinates = root.getChildNodes().item(1);
       
        
        String coordinate_string = coordinates.getTextContent();
        int length_2 = coordinate_string.split(" ").length;
        String[] temp = new String[length_2];
        temp = coordinate_string.split(" ");
       
        // in order to understand this part of code please read the 
        
        int counter = 0;
        for(String point : temp)
        {
         	 point = point.trim();
        	 if(point.contains(","))
  
          	 {
        	String[] temp2 = new String[3];
        	temp2 = point.split(",");
 
               _x = Double.parseDouble(temp2[0]);
        	   _y = Double.parseDouble(temp2[1]);
 
        	   
            if(counter == 0)
            {
             delta_x = x - center_x;  // calculate out the distance between the center of the prefab and the center of the device
             delta_y = y - center_y; // y center
             
            }
            
        	Point newPoint = new Point(_x + delta_x,_y + delta_y); // for the rest of the coordinates of the prefab, their new value should be itself plus delta...
        	CoordinateList.add(newPoint);
           
            }
        	 counter++;
        }
        
        // ----------------------------------- read the prefab xml , get the coordinates and add delta to make the new geometry 
 
		 
	}
	
	
	 
	
 
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	 
}
