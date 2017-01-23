package cam.dev.zhouxiaochi;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

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
	
	public static ArrayList<Point> coordinates = new ArrayList<Point>();
	
	public void readkml(ArcGISFeatureLayer buildinglayer) throws ParserConfigurationException, SAXException, IOException, InterruptedException
	{
		 SimpleLineSymbol building_outline = new SimpleLineSymbol(new Color(255, 234, 0), 300);
		 SimpleFillSymbol symbol = new SimpleFillSymbol(new Color(163, 23, 0, 180), building_outline);
		 
		 
		 buildinglayer.initializeAsync();
		 try {
			while(!buildinglayer.isAllowGeometryUpdates())
			 {
				 Thread.sleep(500);
				 System.out.println("Loading");
			 }
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		 
		 
		// for each building polygons, generate a Graphic and commit the polygon. 
		 
		 
		  
		 	File inputFile = new File("Buildings.kml");
			
	        DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
	        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
	        Document doc = dBuilder.parse(inputFile);
	        doc.getDocumentElement().normalize(); 
	        Element root = doc.getDocumentElement();
	        NodeList featureList = root.getElementsByTagName("coordinates");
	        
	        Graphic[] adds =  new Graphic[featureList.getLength()];
	        
	        
	        
	        
	        for(int i = 0; i < featureList.getLength();i++)
	        {
	        	
	        	Polygon building_polygon = new Polygon();
	        	String value = featureList.item(i).getTextContent();
	        	Point point = new Point();
	        	int counter = 0;
	        	
	        	
	        	for(String coordinate_string : value.split(",100\n"))
	        	{
	        		String x_string =  coordinate_string.split(",")[0].trim();
	        		String y_string =  coordinate_string.split(",")[1].trim();
	    
	        		// 01.80N 000.90E
	        		point = CoordinateConversion.decimalDegreesToPoint(y_string+ "N " + x_string + "E", buildinglayer.getDefaultSpatialReference());
	        //		System.out.println(point);
	        		 
	        		if(counter == 0)
	        		{
	        				building_polygon.startPath(point);
	        		}
	        		else
	        		{
	        				building_polygon.lineTo(point);
	        		}

	        		counter++;
	        	}
	        	building_polygon.closePathWithLine();
	        	
	        	
	        	
	        	
	        	
        		
        		Graphic building_graphic = new Graphic(building_polygon,symbol,null);
        		adds[i] = building_graphic;
        		
	        }
	        
	        CallbackListener<FeatureEditResult[][]> callback = null;
	        
    		buildinglayer.applyEdits(adds, null, null, callback  );
    		
    	 	System.out.println(callback);
	        
	        
	        
	        
	        
	        
	        
	        
	        
	        
	        
	}
}
