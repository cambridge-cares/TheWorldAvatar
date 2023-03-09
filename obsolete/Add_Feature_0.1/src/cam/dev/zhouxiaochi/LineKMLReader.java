package cam.dev.zhouxiaochi;


import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
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
/***
 * Read line attributes and coordinates from xml generated from shp.
 * @author Shaocong
 *
 */
public class LineKMLReader {
	

	
	public  ArrayList<ArrayList<Point>> coordinates = new ArrayList<ArrayList<Point>>();
	public  ArrayList<Point[]> pipeLineCoordinates = new ArrayList<Point[]>();
    public ArrayList<Map<String, Object>> dataTables = new ArrayList<Map<String, Object>>();
	public ArrayList<String> attriNameList = new ArrayList<String>();

	
    private static String[] fileLocations = {"kml/WaterLine.xml",
    		                                 "kml/GasLine.xml",
    		                                 "kml/Material_Line.xml",
    		                                 "kml/AirLine.xml"
                                            };
	private static LineKMLReader[] instanceList = new LineKMLReader[App.LineType.values().length];
    
	public static LineKMLReader getInstance(App.LineType lineType) throws Exception{
		System.out.println("length of line type:"+ App.LineType.values().length);
		int typeId = lineType.getId();
		if( instanceList[lineType.getId()] == null){//if this instance is not created yet
			instanceList[lineType.getId()] = new LineKMLReader(fileLocations[typeId]);
		}
		return instanceList[lineType.getId()];
	}
	
	private LineKMLReader( String fileLocation) throws Exception{
		readkml(fileLocation);
	}
	
	
	public  void readkml(String fileLocation) throws Exception
	{
		
		   pipeLineCoordinates.clear();
		  
		 	File inputFile = new File(fileLocation);
			
	        DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
	        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
	        Document doc = dBuilder.parse(inputFile);
	        doc.getDocumentElement().normalize(); 
	        Element root = doc.getDocumentElement();
	        NodeList coordList = root.getElementsByTagName("Coordinates");
	        NodeList attriLists = root.getElementsByTagName("AttributeList");
	      	
	        boolean firstTime = true;
        	for(int idxAttriList = 0; idxAttriList < attriLists.getLength(); idxAttriList++)
	        {
        		NodeList   attris = attriLists.item(idxAttriList).getChildNodes();
        	  Map<String, Object> attriMap = new  LinkedHashMap<String, Object>();
        		for(int idxAttri = 0; idxAttri < attris.getLength(); idxAttri++){
        			Node attriNode = attris.item(idxAttri);

                    if (attriNode.getNodeType() == Node.ELEMENT_NODE) {

        			Element attriEle = (Element)attriNode;

        			Node attriNameNode = attriEle.getElementsByTagName("name").item(0);
        			Node attriValueNode = attriEle.getElementsByTagName("value").item(0);

        			
        			 if ( attriNameNode != null){
                     System.out.println("++++++++"+attriNameNode.getTextContent());
        				 attriMap.put(attriNameNode.getTextContent(), attriValueNode.getTextContent());
        			if(firstTime){
        				attriNameList.add(attriNameNode.getTextContent());
        			}
        			 }
        		}
        		}
        		dataTables.add(attriMap);
				firstTime = false;

	        }
        	
	        for(int i = 0; i < coordList.getLength(); i++)
	        {
 	        	String value = coordList.item(i).getTextContent();
 
 	            
	        	int length = value.split(",100\n").length;
	        	String[] string_array = new String[length];
	        	string_array = value.split(",100\n");
	        	
	        	ArrayList<Point> temp = new ArrayList<Point>();
	        	
	        	for(String point : string_array)
	        	{
	        		String x_string =  point.split(",")[0].trim();
	        		String y_string =  point.split(",")[1].trim();
	        		//Point thisPoint = CoordinateConversion.decimalDegreesToPoint(y_string+ "N " + x_string + "E", buildinglayer.getDefaultSpatialReference());
	        		Point thisPoint = new Point(Double.parseDouble(x_string), Double.parseDouble(y_string));
	        		temp.add(thisPoint);
	        	}
	        	
	        	coordinates.add(temp);
	        	
	              	
	        	
        		 
	        }
	System.out.println("end for debugging");
	}
}
