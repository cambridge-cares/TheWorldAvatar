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

	public static String[] layers = {"Load_Point","Bus_Coupler","EHT_Station","UHT_Station","HT_Station","LT_Station"};
	public static String Url_Base = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST024/FeatureServer/";
	
	public static String[] files;
	
	public static void main(String[] args) throws Exception {
	 

 

        layer_factory(0,"Load",null,"Load_Point",true); // Load Points
        layer_factory(0,"Coupler",null,"Bus_Coupler",false); // Load Points
        layer_factory(0,"Transformer","^.*EHT.*$","EHT_Station",true);
        layer_factory(0,"Transformer","^.*UHT.*$","UHT_Station",true);
        layer_factory(0,"Transformer","^.*HT.*$","HT_Station",true);
        layer_factory(0,"Transformer","^.*LT.*$","LT_Station",true);        

		
	}

	public static void layer_factory(int file_index, String device_class, String filter_regex, String LayerName, Boolean filter_on) throws Exception
	{
		
		files = new String[2];
		files[0] = "owl/updated electrical network.owl";
		files[1] = "owl/buildingmodif2.owl";
		if(filter_regex==null)
		{
			filter_regex = "\\d+";
		}
		
		System.out.println(files[file_index]);
		ArrayList<Device> device_list = OWLReader.BatchOperationForPoint(files[file_index],device_class,filter_on,filter_regex); // This is for Load Points 
		 
		System.out.println("We are here " + device_list.size());
		String[] fields = new String[device_list.get(0).Name_list.length];
		for(int i = 0; i < fields.length ; i++)
		{
			fields[i] = device_list.get(0).Name_list[i].replaceAll("\\d+", "");
			fields[i] = fields[i].replaceAll("-", "");
		}
	 
		
		generate_a_layer(fields,LayerName);
		

		
		for(Device device : device_list)
		{
			System.out.println(device.Id);
		
			for(int i = 0; i < device.Name_list.length;i++)
			{
				device.Name_list[i] = device.Name_list[i].replaceAll("\\d+", "");
				device.Name_list[i] =device.Name_list[i].replaceAll("-", "");
			}
			draw_a_point(device.Point,device.Name_list,device.Value_list,device.Id,LayerName);
		}	
	}
	 
	/****
	 * The function generates a layer in ArcGIS server basing on the field names detected from the owl file
	 * @param name_list --> the list of field names, which are derived from the owl file
	 * @param LayerName   --> the name of the layer.
	 */
	
	
	
	public static void generate_a_layer(String[] name_list,String LayerName) throws JSONException 
	{
		      
	 	Map<String, String[]> attrLists = new HashMap<String, String[]>();
		
	 	String[] typeList  = new String[name_list.length];
	 	  	
	 	
	 	
	 	for(int i = 0 ; i < name_list.length; i++)
	 	{
	 		typeList[i] = "esriFieldTypeString";		// set the 
	 	}
	 	 
	    attrLists.put("name", name_list);
		attrLists.put("type", typeList);
		attrLists.put("alias", name_list);
		int lengthOfEachList = name_list.length;
		FeatureServiceUpdater updater = new FeatureServiceUpdater(Url_Base);
	    updater.generateLayer(lengthOfEachList,FeatureServiceUpdater.LayerType.POINT, attrLists, LayerName);

		
	}
	 
	
	/****
	 * The function draws point objects and write data to ArcGISiu              online server
	 * @param point --> the coordinate of the point object
	 * @param name_list   --> the list of attributes names 
	 * @param value_list  --> the list of attributes values
	 * @param Id    --> the Id of the device
	 * @param LayerName    --> the name of the layer that stores the point object
	 */
	

	public static void draw_a_point(Point point, String[] name_list, String[] value_list, String Id, String LayerName  ) throws IOException, JSONException, InterruptedException
	{
		 

		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		ArcGISFeatureLayer Point_test_layer = new ArcGISFeatureLayer(Url_Base + LayerName,user);
		Point_test_layer.initializeAsync();
		
		while(!Point_test_layer.isAllowGeometryUpdates()) // wait till the target layer is initiated
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
