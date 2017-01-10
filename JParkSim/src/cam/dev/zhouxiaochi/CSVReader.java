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

import org.apache.commons.io.IOUtils;

import com.esri.core.io.UserCredentials;
import com.esri.map.ArcGISFeatureLayer;

 

public class CSVReader {
 
	public static String filename;

	    public static void readCSV(String ButtonPressed) throws IOException {

	    	 switch(ButtonPressed)
	    	 {
	    	 case  "PrAPPW":
	    	 filename = "PrAPPWOUT.CSV";
	    	 break;
	    	 }
	    	
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

	    	       
	    	       
	    	    } finally {
	    	   
	    	    }
	    }
 
}
