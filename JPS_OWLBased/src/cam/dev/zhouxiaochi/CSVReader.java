package cam.dev.zhouxiaochi;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.json.JSONException;

import com.esri.core.io.UserCredentials;
import com.esri.map.ArcGISFeatureLayer;

 

public class CSVReader {
 
	public static String filename;

	    public static ArrayList<String[]> readCSV(String ButtonPressed) throws IOException, JSONException {

	    	 switch(ButtonPressed)
	    	 {
	    	 case  "PrAPPW":
	    	 filename = "PrAPPWOUT.CSV";
	    	 break;
	    	 }
	    	
	    	 Map<Integer, String> oldNewAttriNameDictionary = new HashMap<Integer, String>();
	    	 oldNewAttriNameDictionary.put(25, "ValueOfHeatDutyOfR_301");
	    	 oldNewAttriNameDictionary.put(22, "ValueOfHeatDutyOfR_302");

	    	  ArrayList<String[]> result = new ArrayList<String[]>();
	    	  InputStream input = new URL( "http://172.25.182.41/" + filename ).openStream();
	    	    try {
	    	       String myString = IOUtils.toString(input);
  
	    	       int length = myString.split(",").length;
	    	       String name_string = myString.split("\n")[0];
	    	       String value_string = myString.split("\n")[1];	    	      
	    	       String[] names = new String[length/2];
	    	       String[] values = new String[length/2];
	    	       names = name_string.split(",");
	    	       values = value_string.split(",");
	    	       
	    	       
	    	       System.out.println("R-301 HeatDuty  " + names[25] + " Value is -->" + values[25]);
	    	       System.out.println("R-302 HeatDuty   " + names[22] + " Value is -->"+ values[22]);
	    	       String[] pair = {"R-301",names[25],values[25]};
	    	       result.add(pair);
	    	       String[] pair2 = {"R-302",names[22],values[22]};
	    	       result.add(pair2);
	    	       
	    	       FeatureServiceUpdater mUpdater = new FeatureServiceUpdater("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST017/FeatureServer");
	    	       String[] entityNameList= {"R-301", "R-302"};
	    	      Map<Integer,Map<String, String>> nameValueListPerLayer = new HashMap<Integer, Map<String, String>>();
	    	      
	    	      HashMap<String, String>   parameters301 = new HashMap<String, String>();
	    	      HashMap<String, String>   parameters302 = new HashMap<String, String>();

	    	      //TODO: if name is still to old convention, query will return null, so what to do really?
	    	      //ValueOfHeatDutyOfR_301 
	    	      
	    	      parameters301.put(oldNewAttriNameDictionary.get(25), values[25]);
	    	      parameters302.put(oldNewAttriNameDictionary.get(22), values[22]);
	    	      nameValueListPerLayer.put(0, parameters301);
	    	      nameValueListPerLayer.put(1, parameters302);

	    	       mUpdater.updateFeaturesInTable(entityNameList, nameValueListPerLayer);
	    	       
	    	    } finally {
	    	   
	    	    }
				return result;
	    }
 
}
