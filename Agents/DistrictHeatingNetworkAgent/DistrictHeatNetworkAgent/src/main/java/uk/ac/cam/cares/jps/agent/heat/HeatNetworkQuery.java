package uk.ac.cam.cares.jps.agent.heat;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.IOException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import java.time.OffsetDateTime;
import java.sql.SQLException;
import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import com.jayway.jsonpath.JsonPath;

@Controller
@WebServlet(urlPatterns = {HeatNetworkQuery.URL_PATH})

public class HeatNetworkQuery extends JPSAgent {
	
	public static final String URL_PATH = "/performheatquery";
	
	private static final Logger LOGGER = LogManager.getLogger(HeatNetworkQuery.class);
	private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
	private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
	
	public String processRequestParameters() { 
		
		 Map<String, List<?>> data = new HashMap<String, List<?>>();
	    	data.put("obsTimeUtc", Arrays.asList("2007-12-03T10:15:30", "2008-12-03T10:15:30", "2009-12-03T10:15:30")); 
	    	data.put("sample1", Arrays.asList("1","2","3"));
	    	data.put("sample2", Arrays.asList("4","5","6"));
	    	data.put("sample3", Arrays.asList("7","8","9"));
	    	//System.out.println(data); 
	    	
	    	
	    	String propertiesFile = "/Users/HXUE01/Documents/GitHub/TheWorldAvatar/Agents/DistrictHeatingNetworkAgent/bin/config/client.properties";
	    	
	    	
		 //HeatNetworkAgent HeatNetworkA = new HeatNetworkAgent();
	     HeatNetworkAgent agent = new HeatNetworkAgent();
	     
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
	            //agent.initializeTimeSeriesIfNotExist(); System.out.println("testZ2");
	            //agent.updateTSData(data);
	        }
	        catch (JPSRuntimeException e) {
	            LOGGER.error(INITIALIZE_ERROR_MSG,e);
	            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
	        }
		 
		 
		 String done = "end";
		 return done;
		 //System.out.println("end"); 
	 }
}

