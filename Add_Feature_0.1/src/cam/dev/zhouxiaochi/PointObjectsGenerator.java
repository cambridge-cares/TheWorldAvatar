package cam.dev.zhouxiaochi;

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.json.JSONException;

import com.esri.core.geometry.Point;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Graphic;
import com.esri.core.symbol.SimpleMarkerSymbol;
import com.esri.map.ArcGISFeatureLayer;

import cam.dev.zhouxiaochi.OWLReader.Device;

public class PointObjectsGenerator {

	public static String[] files;
	
	public static void main(String[] args) throws Exception {
	 
		files = new String[2];
		files[0] = "updated electrical network.owl";
		files[1] = "buildingmodif2.owl";
		layer_factory(0,"Load",null,"Load_Point",true); // Load Points
//		layer_factory(1,"Building",null,"Buildings",false); 
//		layer_factory(0,"Transformer",null,"Transformer",false);
//		layer_factory(0,"PowerGenerator",null,"PowerGenerator",false);
	}

	public static void layer_factory(int file_index, String device_class, String filter_regex, String LayerName, Boolean filter_on) throws Exception
	{
		if(filter_regex==null)
		{
			filter_regex = "\\d+";
		}
		
		ArrayList<Device> device_list = OWLReader.BatchOperationForPoint(files[file_index],device_class,filter_on,filter_regex); // This is for Load Points 
		 
		System.out.println("We are here " + device_list.size());
		String[] fields = new String[device_list.get(0).Name_list.length];
		for(int i = 0; i < fields.length ; i++)
		{
			fields[i] = device_list.get(0).Name_list[i].replaceAll(filter_regex, "");
			fields[i] = fields[i].replaceAll("-", "");
		}
	 
		
		generate_a_layer(fields,LayerName);
		

		
		for(Device device : device_list)
		{
			System.out.println(device.Id);
		
			for(int i = 0; i < device.Name_list.length;i++)
			{
				device.Name_list[i] = device.Name_list[i].replaceAll(filter_regex, "");
				device.Name_list[i] =device.Name_list[i].replaceAll("-", "");
			}
			draw_a_point(device.Point,device.Name_list,device.Value_list,device.Id,LayerName);
		}	
	}
	 
	public static void generate_a_layer(String[] name_list,String LayerName) throws JSONException
	{
		      
	 	Map<String, String[]> attrLists = new HashMap<String, String[]>();
		
	 	String[] typeList  = new String[name_list.length];
	 	  	
	 	
	 	
	 	for(int i = 0 ; i < name_list.length; i++)
	 	{
	 		typeList[i] = "esriFieldTypeString";
	 	}
	 	 
	    attrLists.put("name", name_list);
		attrLists.put("type", typeList);
		attrLists.put("alias", name_list);
		int lengthOfEachList = name_list.length;
		FeatureServiceUpdater updater = new FeatureServiceUpdater("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Point/FeatureServer");
	    updater.generateLayer(lengthOfEachList,FeatureServiceUpdater.LayerType.POINT, attrLists, LayerName);

		
	}
	 


	public static void draw_a_point(Point point, String[] name_list, String[] value_list, String Id, String LayerName  ) throws IOException, JSONException, InterruptedException
	{
		 

		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		ArcGISFeatureLayer Point_test_layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Point/FeatureServer/" + LayerName,user);
		Point_test_layer.initializeAsync();
		while(!Point_test_layer.isAllowGeometryUpdates())
		{
			Thread.sleep(500);
		}
		
		Map<String,Object> attributes = new HashMap<String,Object>();
		

		
		for(int i = 0; i < name_list.length;i++)
		{
		attributes.put(name_list[i], value_list[i]);
		}
		 
	    SimpleMarkerSymbol symbol = new SimpleMarkerSymbol(Color.red, 15, SimpleMarkerSymbol.Style.CROSS);
	    Graphic graphic;
	    graphic = new Graphic(point, symbol,attributes);
		Graphic[] adds = {graphic};
		Point_test_layer.applyEdits(adds, null, null, null);	

	}

	
	
	
}
