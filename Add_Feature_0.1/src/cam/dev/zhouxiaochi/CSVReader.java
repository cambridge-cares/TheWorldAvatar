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
	    //	 oldNewAttriNameDictionary.put(25, "ValueOfHeatDutyOfR_301");
	    	 oldNewAttriNameDictionary.put(22, "ValueOf_x_R_303");

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
	    	       
	    	       
	    	    
	
	    	      String[] pair2 = {"R-303","ValueOf_x_R_303",values[22]};
	    	      result.add(pair2);
	    	       
	    	      FeatureServiceUpdater mUpdater = new FeatureServiceUpdater("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST015/FeatureServer");
	    	      String[] entityNameList= {"R-303"};
	    	      Map<Integer,Map<String, String>> nameValueListPerLayer = new HashMap<Integer, Map<String, String>>();
	    	     
	    	      HashMap<String, String>   parameters301 = new HashMap<String, String>();
	    	      HashMap<String, String>   parameters302 = new HashMap<String, String>();

	    	      parameters302.put(oldNewAttriNameDictionary.get(22), "xxx");
	    	      nameValueListPerLayer.put(0, parameters302);

	    	      
	    	       mUpdater.updateFeaturesInTable(entityNameList, nameValueListPerLayer);
	    	       
	    	    } finally {
	    	   
	    	    }
				return result;
	    }
 
}
