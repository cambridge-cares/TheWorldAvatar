import java.awt.Color;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable.Status;
import com.esri.core.geometry.CoordinateConversion;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
import com.esri.core.geometry.SpatialReference;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Field;
import com.esri.core.map.Graphic;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleFillSymbol.Style;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.FeatureLayer;
import com.esri.map.JMap;
import com.esri.map.MapEvent;
import com.esri.map.MapEventListenerAdapter;

public class App {

	
	public static ArcGISFeatureLayer testLayer;
	public static ArcGISFeatureLayer referenceLayer;
	public static String[] name_array;
	public static String[] coordinate_array;
	
	private static JMap map;
 
	//http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/test004/FeatureServer
	
	public static void main(String[] args) throws SAXException, IOException, Exception {


		UserCredentials user = new UserCredentials();
	    user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		
	    testLayer =new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/test004/FeatureServer/0", user);	   
	    testLayer.initializeAsync();
	    initiate_layer(testLayer);

	    referenceLayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/water_line/FeatureServer/0",user);
	    referenceLayer.initializeAsync();
	    initiate_layer(referenceLayer);
		 read_from_kml();
 	    System.out.println(testLayer);
	    System.out.println(testLayer.isAllowGeometryUpdates());
	 
	    String[] fields = {"Name"};
	    testLayer.setOutFields(fields);
 
	    
	    for(int i = 0 ; i < name_array.length ; i++)
	    
	    {
	    Polygon polygon = new Polygon();
	     
	    	int size = coordinate_array[i].split("#").length;
	    	String[] point = new String[size];
	    	point = coordinate_array[i].split("#");
	    	
	    	 double _x = 	CoordinateConversion.decimalDegreesToPoint(point[0], testLayer.getDefaultSpatialReference()).getX();
			 double _y = 	CoordinateConversion.decimalDegreesToPoint(point[0], testLayer.getDefaultSpatialReference()).getY();
        	polygon.startPath(_x + 0 ,_y + 300 );
	    	for(int counter = 1 ; counter<size; counter++)
	    	{
	    	 
	    		double x = 	CoordinateConversion.decimalDegreesToPoint(point[counter], testLayer.getDefaultSpatialReference()).getX();
	    		 double y = 	CoordinateConversion.decimalDegreesToPoint(point[counter], testLayer.getDefaultSpatialReference()).getY();

	    		 System.out.println("X is " + x  + 100);
	    		 System.out.println("Y is " + y  + 100 );
	    		 System.out.println("---------------------------------");
	    	 polygon.lineTo(x +  0,y + 300);
	    	 // polygon.lineTo(x,y);
	    	 System.out.println("Point Number" + polygon.getPointCount());
	    	}
	    
	    
	    polygon.closePathWithLine();
	     
	    SimpleLineSymbol outline = new SimpleLineSymbol(new Color(0, 150, 0), 300);
	    SimpleFillSymbol symbol2 = new SimpleFillSymbol(new Color(0, 240, 0, 180), outline);
	    Map<String,Object> attributes = new HashMap<String,Object>();
	    attributes.put("Name", name_array[i]);
		
	    Graphic polygonGraphic = new Graphic(polygon, symbol2,attributes);
	    Graphic[] adds = {polygonGraphic};
	    testLayer.applyEdits(adds, null, null, null);

	    System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++");
	    
	    }
   
	System.out.println("Finished");
	}
	
	public static void initiate_layer(ArcGISFeatureLayer Layer)
	{
		 if(Layer.isAllowGeometryUpdates() == false)
		    {
		    try {
		        Thread.sleep(1000);       
		        System.out.println("Tick 1 ");
		        initiate_layer(Layer);//1000 milliseconds is one second.
		    } catch(InterruptedException ex) {
		        Thread.currentThread().interrupt();
		    }
		    }
		    else
		    {
		    	return;
		    }
	}
 
	public static void read_from_kml() throws SAXException, IOException, Exception
	{
		File inputFile = new File("020.kml");
        DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(inputFile);
        doc.getDocumentElement().normalize();
        
        Element kml = doc.getDocumentElement();
        NodeList placemarks = kml.getElementsByTagName("Placemark");
        
        
        name_array = new String[placemarks.getLength()];
         
         
         
         
         
         
        for(int i = 0; i < placemarks.getLength();i++)
        { 
        	Node placemark = placemarks.item(i);
        	Node name = placemark.getChildNodes().item(1);
        	System.out.println("No." + i + " Name" + name.getTextContent());
        	name_array[i] = name.getTextContent();	 
        }
        
        NodeList polygon = kml.getElementsByTagName("Polygon");
        
        
         coordinate_array = new String[placemarks.getLength()];
         
        
        
        
        
        for(int i = 0; i<placemarks.getLength();i++)
        {
        	 Node coordinate = polygon.item(i).getChildNodes().item(5).getChildNodes().item(1).getChildNodes().item(1);
             String coordinate_string = "";
        	 for(int j = 0; j < coordinate.getTextContent().split(" ").length; j ++)
        	 {
        		 String point = coordinate.getTextContent().split(" ")[j];
        		 point = point.trim();
 
              	 if(point.contains(","))
              		 
              	 {
              		 String Long  =  point.split(",")[0]+ "E";
              		 String Lat	    =  point.split(",")[1]+ "N";
              		coordinate_string = Lat+ " " + Long + "#" + coordinate_string ;
 
              	 }
              	
        	 }
        	  
        	 coordinate_array[i] = coordinate_string ;
        }
		
	}
 
}
