package uk.ac.cam.cares.jps.agent.heat;

import org.json.JSONArray;
import org.json.JSONObject;
import java.util.Date;

import com.ibm.icu.text.DateFormat;
import com.ibm.icu.text.SimpleDateFormat;
import com.ibm.icu.util.Calendar;
import com.jayway.jsonpath.JsonPath;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import javax.ws.rs.BadRequestException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.update.UpdateRequest;
import org.jooq.exception.IOException;
import org.apache.jena.graph.NodeFactory;
import uk.ac.cam.cares.jps.base.http.Http;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.ParseException;

public class HeatTest {
	
	  private static SimpleDateFormat inSDF = new SimpleDateFormat("dd/mm/yyyy hh:ss");
	  private static SimpleDateFormat outSDF = new SimpleDateFormat("yyyy-mm-dd hh:ss:00");
	  public static final String KEY_CLIENTPROPERTIES = "clientProperties";

	public static void main(String[] args) throws ParseException { 
	//System.out.println("123"); 
		

		
	String TsData = "/Users/HXUE01/Documents/GitHub/TheWorldAvatar/Agents/DistrictHeatingNetworkAgent/bin/config/optimization_input_2020_testA.csv";
	
	String[] numericValue0 = ReadCol(0,TsData,",");
	Map<String, List<?>> data_new = new HashMap<String, List<?>>();
	List<String> Date_Utc = new ArrayList<String>();
	for (int i =1; i < numericValue0.length; i++)
	{//System.out.println(i);
		Date_Utc.add(formatDate(numericValue0[i]));
		}
	System.out.println("AA");
	data_new.put("obsTimeUtc", Date_Utc);
	System.out.println("BB");
	
	String requestInput = "1/1/2020 0:00";
	System.out.println(formatDate(requestInput));
	
	
	int column_length = 0;
	try {
		column_length = ColNum (TsData,",");
	} catch (java.io.IOException e) {
		e.printStackTrace();
	}
	
	System.out.println(column_length); 
	
	for (int i =1; i < column_length; i++) {
		//String[] numericValue = ReadCol(i,TsData,",");
		//data_new.put(numericValue[0], Arrays.asList(Arrays.copyOfRange(numericValue, 1, numericValue.length)));
	}
    System.out.println(data_new);
 	
 	
	//Map<String, List<?>> data = new HashMap<String, List<?>>();
 	//data.put("obsTimeUtc", Arrays.asList("2007-12-03T10:15:30", "2008-12-03T10:15:30", "2009-12-03T10:15:30","2010-12-03T10:15:30","2011-12-03T10:15:30","2012-12-03T10:15:30")); 
 	//data.put("sample1", Arrays.asList("1","2","3","33","44","443"));
 	//data.put("sample2", Arrays.asList("4","5","6","33","44","443"));
 	//data.put("sample3", Arrays.asList("7","8","9","33","44","443"));
 	//System.out.println(data);
	
	//for (int i =0; i < value1.length; i++)
	//{	System.out.println(value1[i]);
	//}
	
 	//String data_obsTimeTtc=formatDate("1/1/2020 0:00");
 	//System.out.println( data_obsTimeTtc);
	//for (int i =0; i < staticData.length; i++)
	//{	System.out.println(value1[i]);
	//}
}

	public static String[] ReadCol(int col, String filepath, String delimiter) {
    	String currentLine;
    	String data[];	
    	ArrayList<String> colData = new ArrayList<String>();
	
	try {
		FileReader fr = new FileReader(filepath);
		BufferedReader br = new BufferedReader(fr); 
		while((currentLine = br.readLine()) !=null)
		{
			data = currentLine.split(delimiter);
		    colData.add(data[col]);
		    
		}
	    } catch (Exception e) {
		    System.out.println(e);
		    return null;
	    } 
	    
	    
	    return colData.toArray(new String[0]);
    }
	
	public static int ColNum (String filepath, String delimiter) throws java.io.IOException {
		FileReader fr_col;
		String[] currentLine;
			fr_col = new FileReader(filepath);
			BufferedReader br_col = new BufferedReader(fr_col); 
			currentLine = br_col.readLine().split(",");
			int col_length = currentLine.length;
			return col_length;
	}

	  public static String formatDate(String inDate) {
		  
		    String outDate = "";
		    if (inDate != null) {
		        try {
		            Date date = inSDF.parse(inDate);
		            outDate = outSDF.format(date);
		        } catch (ParseException ex){ 
		        }
		    }
		    
		    //System.out.println(inDate);
		    String front = outDate.substring(0,10); 
		    //System.out.println(front); System.out.println(front+"AAAA");
		    String back = outDate.substring(11);
		   // System.out.println(back); System.out.println(front+"BBB");
		   // System.out.println(outDate+"CCC");
		 
		    return front+"T"+back;
		  }

}
