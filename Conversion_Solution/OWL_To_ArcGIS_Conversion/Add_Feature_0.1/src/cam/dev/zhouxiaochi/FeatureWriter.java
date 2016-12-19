package cam.dev.zhouxiaochi;
import com.esri.runtime.ArcGISRuntime;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

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
	

	/***
	 * Create a feature basing on the coordinates, type and spatial reference
	 * @param type  type of the feature e.g. Blower
	 * @param x		the longitude of the feature (decimal coordinate)
	 * @param y		the latitude    of the feature (decimal coordinate)
	 * @param ref    the spatial reference of the layer
	 *  @return Graphic[] the array of features to be added
	 */
	public Graphic[] createFeature(String type , double x , double y,SpatialReference ref ) throws SAXException, IOException, ParserConfigurationException
	{ 
        String x1  = null;
        String y1  = null;
    
		  Polygon polygon = new Polygon();
		 
		  SimpleLineSymbol outline = new SimpleLineSymbol(new Color(0, 150, 0), 300);
		  SimpleFillSymbol symbol2 = new SimpleFillSymbol(new Color(0, 240, 0, 180), outline);
		  
		
	 
 		  double value = 1.521513125;
		 Map<String,Object> attributes = new HashMap<String,Object>();
		
		
		 int length = OWLReader.name_list.size();
		
 
	 for(int i = 0 ; i < length ; i++)
	 {
		 attributes.put(i+OWLReader.name_list.get(i), OWLReader.value_list.get(i));
	 }
			 
			 
	 
		 
		File inputFile = new File(type + ".xml");
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
        
        int counter = 0;
        for(String point : temp)
        {
         	 point = point.trim();
        	 if(point.contains(","))
 
          	 {
        	String[] temp2 = new String[3];
        	temp2 = point.split(",");
            x1 = temp2[0] + "E";
            y1 = temp2[1]  + "N";
       //      String input = y1 + " " + x1;
         //   double _x = 	CoordinateConversion.decimalDegreesToPoint(input,ref).getX();
           // double _y = 	CoordinateConversion.decimalDegreesToPoint(input, ref).getY();
               _x = Double.parseDouble(temp2[0]);
        	   _y = Double.parseDouble(temp2[1]);
            	
            
            if(counter == 0)
            {
             delta_x = x - _x;
             delta_y = y - _y;
             polygon.startPath(x,y);
            }
            else
            {
            	polygon.lineTo(_x + delta_x , _y+delta_y);
            }
             
          	 }
        	 
        	 counter++;
        }
        
        polygon.closePathWithLine();
        
        double angleRad = Math.toRadians(-45);
        Transformation2D rotateTx = new Transformation2D();
        
        rotateTx.rotate(Math.cos(angleRad), Math.sin(angleRad), new Point(_x,_y));

        Transformation2D scale = new Transformation2D();
        scale.scale(0.5, 0.5);
        
        // apply transformation on the point
        polygon.applyTransformation(rotateTx);
        polygon.applyTransformation(scale);
        
        
 	    Graphic polygonGraphic = new Graphic(polygon, symbol2,attributes);
	    Graphic[] adds = {polygonGraphic};
		return adds;
	}
	
	 
}
