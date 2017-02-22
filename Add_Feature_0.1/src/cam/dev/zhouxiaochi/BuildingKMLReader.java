package cam.dev.zhouxiaochi;

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

public class BuildingKMLReader {
	
	public static ArrayList<Point> coordinates = new ArrayList<Point>();
	public static ArrayList<String> buildingList = new ArrayList<String>();
	
	public static String[] nameList;

	
	
	
	
	public void readkml(ArcGISFeatureLayer buildinglayer) throws Exception
	{
		

		
		buildingList.clear();
		coordinates.clear();
		buildingList = Filter_Building_Names(OWLReader.read_owl_file("owl/buildingmodif2.owl", null));
  
  
	     CreateBuildingLayer();
 
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
		 
		 SimpleLineSymbol building_outline = new SimpleLineSymbol(new Color(255, 234, 0), 300);
		 SimpleFillSymbol symbol = new SimpleFillSymbol(new Color(163, 23, 0, 180), building_outline);
		 

		 
		 
		// for each building polygons, generate a Graphic and commit the polygon. 
		 
		 
		  
		 	File inputFile = new File("kml/Buildings.kml");
			
	        DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
	        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
	        Document doc = dBuilder.parse(inputFile);
	        doc.getDocumentElement().normalize(); 
	        Element root = doc.getDocumentElement();
	        NodeList featureList = root.getElementsByTagName("coordinates");
	        
	        Graphic[] adds =  new Graphic[featureList.getLength()];
	        
	        
	        
	        
	        for(int i = 0; i < buildingList.size();i++)
	        {
	        	
	        	Polygon building_polygon = new Polygon();
	        	String value = featureList.item(i).getTextContent();
	        	Point point = new Point();
	        	int counter = 0;
	        	String buildingID = buildingList.get(i);
	        	
	        	
	        	
	        	
	        	OWLReader.read_owl_file("owl/buildingmodif2.owl", buildingID);
	        	
	   		 Map<String,Object> attributes = new HashMap<String,Object>();
	 		
			 for(int k = 0 ; k < OWLReader.name_list.size(); k++)
			 {
				 attributes.put(nameList[k], OWLReader.value_list.get(k));
			 }
	        	
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
	        	
        		Graphic building_graphic = new Graphic(building_polygon,symbol,attributes);
        		adds[i] = building_graphic;
        		
	        }
	        
	        CallbackListener<FeatureEditResult[][]> callback = null;
		
	        

  

	        
	        
	      buildinglayer.applyEdits(adds, null, null, callback  );
    		 
	        
	}
	
	public ArrayList<String> Filter_Building_Names(ArrayList<String> nodelist)
	{
		ArrayList<String> result = new ArrayList<String>();
		
		for(String nodename : nodelist)
		{
			if(nodename.contains("_"))
			{
				if(nodename.split("_")[0].matches("^[buildngBUILDNG]*$")    && nodename.split("_")[1].matches("^[0-9]*$"))
				{
					result.add(nodename);
				}
			}
		}
		
		
		return result;
	}
	
	
	public void CreateBuildingLayer() throws IOException, Exception
	{ 		
	OWLReader.read_owl_file("owl/buildingmodif2.owl", buildingList.get(1));
	int length = OWLReader.name_list.size();
	System.out.println( "ID of the Building " + buildingList.get(1));
	String[] typeList = new String[length];
	nameList = new String[length];
 
	
	for(int i = 0; i < length; i++)
	{
		nameList[i] =  OWLReader.name_list.get(i);
		String name = nameList[i];
	
			name = LayerFactory.deleteID(name);
		
		
		
		System.out.println(nameList[i]);
	}
	
	
	
	
	
	
	// _BuildingID

	for(int i = 0; i < length; i++)
	{
		typeList[i] = "esriFieldTypeString";
	}
        Map<String, String[]> attrLists = new HashMap<String, String[]>();
		attrLists.put("name", nameList);
		attrLists.put("type", typeList);
		attrLists.put("alias", nameList);
		int lengthOfEachList = nameList.length;
		FeatureServiceUpdater updater = new FeatureServiceUpdater("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST017/FeatureServer");
		updater.generateLayer(lengthOfEachList,FeatureServiceUpdater.LayerType.POLYGON, attrLists, "Buildings");
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
