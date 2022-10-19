package uk.ac.cam.cares.jps.agent.heat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import java.time.OffsetDateTime;
import java.sql.SQLException;
import java.text.ParseException;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import com.ibm.icu.text.SimpleDateFormat;
import com.jayway.jsonpath.JsonPath;


public class HeatNetworkInputAgentLauncher {
	
	private static final Logger LOGGER = LogManager.getLogger(HeatNetworkInputAgentLauncher.class);
	private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
	private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
	
	private static SimpleDateFormat inSDF = new SimpleDateFormat("dd/mm/yyyy hh:ss");
	private static SimpleDateFormat outSDF = new SimpleDateFormat("yyyy-mm-dd hh:ss:00");


	 @SuppressWarnings("deprecation")  
	public static void main(String[] args) throws ParseException{  
		 
		 String TsData = "/Users/HXUE01/Documents/GitHub/TheWorldAvatar/Agents/DistrictHeatingNetworkAgent/bin/config/optimization_input_2020_testB.csv";
			
			String[] numericValue0 = ReadCol(0,TsData,",");
			Map<String, List<?>> data_TS = new HashMap<String, List<?>>();
			List<String> Date_Utc = new ArrayList<String>();
			for (int i =1; i < numericValue0.length; i++)
			{Date_Utc.add(formatDate(numericValue0[i]));}
			data_TS.put("obsTimeUtc", Date_Utc);
			
			int column_length = 0;
			try {
				column_length = ColNum (TsData,",");
			} catch (java.io.IOException e) {
				e.printStackTrace(); 
			}
			
			
			for (int i =1; i < column_length; i++) {
				String[] numericValue = ReadCol(i,TsData,",");
				data_TS.put(numericValue[0], Arrays.asList(Arrays.copyOfRange(numericValue, 1, numericValue.length)));
			}
		
	    	
	     String propertiesFile = "/Users/HXUE01/Documents/GitHub/TheWorldAvatar/Agents/DistrictHeatingNetworkAgent/bin/config/client.properties";
		 
	     HeatNetworkInputAgent agent = new HeatNetworkInputAgent();
	     
	     TimeSeriesClient<OffsetDateTime> tsClient;
	        try {
	            tsClient = new TimeSeriesClient<>(OffsetDateTime.class,propertiesFile);
	            agent.setTsClient(tsClient);
	        } catch (IOException | JPSRuntimeException e) {
	            LOGGER.error(TSCLIENT_ERROR_MSG, e);
	            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
	        }
	        LOGGER.info("Time series client object initialized.");
	     
	        try { 
	            agent.dataInstantiation();
	            agent.initializeTimeSeriesIfNotExist(); 
	            agent.updateTSData(data_TS);
	        }
	        catch (JPSRuntimeException e) {
	            LOGGER.error(INITIALIZE_ERROR_MSG,e);
	            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
	        }
		 
		 System.out.println("end"); 
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
			    
			    String front = outDate.substring(0,10); 
			    String back = outDate.substring(11);
			    
			    return front+"T"+back;
			  }
}
