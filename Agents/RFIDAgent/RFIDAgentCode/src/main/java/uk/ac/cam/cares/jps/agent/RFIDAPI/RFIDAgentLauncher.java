package uk.ac.cam.cares.jps.agent.RFIDAPI;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.XML;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.InsertValuesStepN;
import org.jooq.Record;
import org.jooq.Result;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;

import javax.servlet.*;

/**
 * Class with a main method that is the entry point of the compiled war and puts all components together to retrieve
 * data from the API and write it into the database.
 * @author 
 */
@WebServlet(urlPatterns = {"/send"})
public class RFIDAgentLauncher extends JPSAgent{
	
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(RFIDAgentLauncher.class);
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;
    
    private String rdbURL = null; 
	private String rdbUser = null;
	private String rdbPassword = null;
	String tagNumber;
	private Connection conn = null;
	String[] tagID;
	String[] RSSI;
	String[] antennaNumber;
	private DSLContext context;
	private static final SQLDialect dialect = SQLDialect.POSTGRES;  
	private static final Field<Long> timestampColumn = DSL.field(DSL.name("timestamp"), Long.class);
	private static final Field<String> dateTimeWithOffsetColumn = DSL.field(DSL.name("datetime with offset"), String.class);
	private static final Field<String> statusColumn = DSL.field(DSL.name("status"), String.class);
	private static final Field<Integer> antennaNumberColumn = DSL.field(DSL.name("antenna number"), int.class);
	private static final Field<Integer> RSSIColumn = DSL.field(DSL.name("RSSI"), int.class);
	
	
	 @Override
	    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
	  
    	
    	long timestamp = System.currentTimeMillis();
    	Date date = new java.util.Date(timestamp);
    	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    	
    	Object ts = sdf.format(date);
    	
    	LocalDateTime localTime = LocalDateTime.parse(ts.toString());
        // Then add the zone id
        OffsetDateTime timestampWithOffset = OffsetDateTime.of(localTime, ZONE_OFFSET);
        System.out.println("The request is "+request.toString());
        System.out.println("The requestParams is " + requestParams);
        //The requestParams is {"acceptHeaders":"*/*","method":"POST","requestUrl":"http://localhost:1016/rfid-agent/","body":"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"> <s:Body> <GetRFIDData xmlns=\"http://tempuri.org/\"><value1>00000000000000A000009727,0,-85<\/value1><\/GetRFIDData><\/s:Body><\/s:Envelope>","contentType":"application/xml"}
    	String body = requestParams.get("body").toString();
    	//System.out.println("The requestParams.get(body) is " + body);
    	JSONObject a;
		JSONObject b;
		JSONObject c;
    	try {  
    		JSONObject json = XML.toJSONObject(body);
    	
    		a = json.getJSONObject("s:Envelope");
    		b = a.getJSONObject("s:Body");
    		c = b.getJSONObject("GetRFIDData");
    	    } catch (JSONException e) {
         	throw new JSONException ("The request parameters is in the wrong format.");
         	}
    		//System.out.println("a is " + a + " and b is " + b + " and c is " + c);
    		try {
    		//extract the tag numbers if there are multiples within one string
    		int num = c.length() - 1;
    		tagID = new String[num];
    		antennaNumber = new String[num];
    		RSSI = new String[num];
    		for (int e = 0; e < num; e++) {
    		Object f = c.get("value" + (e+1));
    		System.out.println(f.toString().split(",")[0]);
    		
    		tagID[e]=f.toString().split(",")[0];
    		antennaNumber[e] = f.toString().split(",")[1];
    		RSSI[e]=f.toString().split(",")[2];
    		}
    		}catch (NullPointerException e) {
    			throw new NullPointerException ("The tag ID, antenna Number and RSSI value cannot be found!");
    		}
    		
    		
    		
           
    		
    	   File file;
           try {
    			file = new File(System.getenv("RFID_PROPERTIES"));		
               } catch (NullPointerException e) {
    	        throw new NullPointerException("No properties file found at specified filepath");
               }
    			if (!file.exists()) {
    				throw new JPSRuntimeException("No properties file found at specified filepath");
    			}
    			
    			// Try-with-resource to ensure closure of input stream
    			try (InputStream input = new FileInputStream(file)) {
    			
    				// Load properties file from specified path
    		        Properties prop = new Properties();
    		        prop.load(input);
    		        
    		
    		        // Get the property values and assign
    		        if (prop.containsKey("db.url")) {
    		        	setRdbURL(prop.getProperty("db.url"));
    		        } else {
    		        	throw new JPSRuntimeException("Properties file is missing \"db.url=<rdb_url>\" ");
    		        }
    		        if (prop.containsKey("db.user")) {
    		        	setRdbUser(prop.getProperty("db.user"));
    		        } else {
    		        	throw new JPSRuntimeException("Properties file is missing \"db.user=<rdb_username>\" ");
    		        }
    		        if (prop.containsKey("db.password")) {
    		        	setRdbPassword(prop.getProperty("db.password"));
    		        } else {
    		        	throw new JPSRuntimeException("Properties file is missing \"db.password=<rdb_password>\" ");
    		        }
    			} catch (FileNotFoundException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
    			
    			
    			
    			for (int i = 0; i < tagID.length; i++) {
    				connect();
        		    String tagNumber = tagID[i];
        		    String antenna = antennaNumber[i];
        		    String rssiValue = RSSI[i];
        		    context.createTableIfNotExists(tagNumber).column(timestampColumn).column(dateTimeWithOffsetColumn).column(statusColumn).column(antennaNumberColumn).column(RSSIColumn).execute();
        		    Table<?> table = DSL.table(DSL.name(tagNumber));
        		    Result<? extends Record> queryResult = null;
        		    try {
        		    	queryResult = context.select(timestampColumn, statusColumn).from(table).where(statusColumn.isNotNull()).orderBy(timestampColumn.desc()).limit(1).fetch();
        		    	List<String> dataValues = queryResult.getValues(statusColumn);
        		    	String value = dataValues.get(0); // will throw an exception for new tags that does not have any previous entries in the database
        		    	System.out.println(value);
        		    	if (value.contains("Out") == true) {
        		    		populateTableIn (timestamp, timestampWithOffset, tagNumber, antenna, rssiValue);
            				System.out.println("The tag is currently in the cabinet");
        				}
        				else if (value.contains("In") == true) {
        					populateTableOut (timestamp, timestampWithOffset, tagNumber, antenna, rssiValue);
        					System.out.println("The tag is currently out of the cabinet");
        				} else {
        					throw new JPSRuntimeException ("There is something wrong with the data in the database!");
        				}
        			
        		    } catch (Exception e) { 
        		    	System.out.println(e.toString());
        		    	long firstTimestamp = timestamp - 5000; 
            			Date firstDate = new java.util.Date(firstTimestamp);
            	    	SimpleDateFormat sdf1 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            	    	sdf1.setTimeZone(TimeZone.getTimeZone("UTC"));
            	    	
            	    	Object ts1 = sdf1.format(firstDate);
            	    	
            	    	LocalDateTime firstLocalTime = LocalDateTime.parse(ts1.toString());
            	        // Then add the zone id
            	        OffsetDateTime firstTimestampWithOffset = OffsetDateTime.of(firstLocalTime, ZONE_OFFSET);
            			populateTableIn (firstTimestamp, firstTimestampWithOffset, tagNumber, antenna, rssiValue);
            			populateTableOut (timestamp, timestampWithOffset, tagNumber, antenna, rssiValue);
        		    }
        		    
        		    disconnect();
    			}
				return requestParams;
    }
    
    public void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
    	String url = req.getRequestURI();
    	Field<?> column = null;
    	
    	String variable = null;
    	int limit = 0;
    	String keys = null;
    	
    	File file;
        try {
 			file = new File(System.getenv("RFID_PROPERTIES"));		
            } catch (NullPointerException e) {
 	        throw new NullPointerException("No properties file found at specified filepath");
            }
 			if (!file.exists()) {
 				throw new JPSRuntimeException("No properties file found at specified filepath");
 			}
    	
    	try (InputStream input = new FileInputStream(file)) {
			
			// Load properties file from specified path
	        Properties prop = new Properties();
	        prop.load(input);
	        
	
	        // Get the property values and assign
	        if (prop.containsKey("db.url")) {
	        	setRdbURL(prop.getProperty("db.url"));
	        } else {
	        	throw new JPSRuntimeException("Properties file is missing \"db.url=<rdb_url>\" ");
	        }
	        if (prop.containsKey("db.user")) {
	        	setRdbUser(prop.getProperty("db.user"));
	        } else {
	        	throw new JPSRuntimeException("Properties file is missing \"db.user=<rdb_username>\" ");
	        }
	        if (prop.containsKey("db.password")) {
	        	setRdbPassword(prop.getProperty("db.password"));
	        } else {
	        	throw new JPSRuntimeException("Properties file is missing \"db.password=<rdb_password>\" ");
	        }
		}
		
		connect();
    	
    	// http://localhost:1016/rfid-agent/values=status/limit=3/keys=00000000000000A000009727
    	if (url.split("/")[2].split("=")[0].contains("values") == true) {
    	variable = url.split("/")[2].split("=")[1];
    	}
    	else {	
        	throw new JPSRuntimeException("The query parameters in the request URL is not written correctly.");
        	}
    	if (url.split("/")[3].split("=")[0].contains("limit") == true) {
    		
    	limit = Integer.valueOf(url.split("/")[3].split("=")[1]);
    	}
    	else {	
        	throw new JPSRuntimeException("The query parameters in the request URL is not written correctly.");
        	}
    	if (url.split("/")[4].split("=")[0].contains("keys") == true) {
    	keys = url.split("/")[4].split("=")[1];
    	}
    	else {	
        	throw new JPSRuntimeException("The query parameters in the request URL is not written correctly.");
        	}
    	
    	
    	JSONObject response = new JSONObject();
    	
    	
    	for (int i = 0; i < keys.split(",").length; i++) {
    		
    		JSONArray responseValue = new JSONArray();
    		
    		if (variable.contains("status") == true) {
    			column = DSL.field(DSL.name("status"), String.class);
    		} else if (variable.contains("antennanumber") == true) {
    			column = DSL.field(DSL.name("antenna number"), int.class);
    		} else if (variable.contains("RSSI") == true) {
    			column = DSL.field(DSL.name("RSSI"), int.class);
    		}
    		System.out.println(column.getName());
    		
    	System.out.println("The table name is " + keys.split(",")[i]);
    	String getTagNumber = keys.split(",")[i];
    	Table<?> table = DSL.table(DSL.name(getTagNumber));
	    Result<? extends Record> getQueryResult = null;
	    try {
	    	getQueryResult = context.select(timestampColumn, column).from(table).where(column.isNotNull()).orderBy(timestampColumn.desc()).limit(limit).fetch();
	    	List<?> dataValues = getQueryResult.getValues(column);
	    	// will throw an exception for new tags that does not have any previous entries in the database
	    	
	    	List<Long> timestampValues = getQueryResult.getValues(timestampColumn);
	    	
	    	if (dataValues.size() != timestampValues.size()) {
	    		throw new JPSRuntimeException ("The number of data values does not match the number of timestamp values");	
	    	}
	    	
	    	for (int j = 0; j < dataValues.size(); j++) {
	    		JSONObject a = new JSONObject();
	    		a.put("ts", timestampValues.get(j));
	    		a.put("value", dataValues.get(j));
	    		responseValue.put(a);
	    		
	    	}
	    	
	    	response.put("tag_"+keys.split(",")[i]+"_"+variable, responseValue);
	    } catch (Exception e) {
	    	System.out.println(e.toString());
	    }
    	}
	   
    	res.setContentType("application/json");
    	// Get the printwriter object from response to write the required json object to the output stream      
    	PrintWriter out = res.getWriter();
    	// Assuming your json object is **jsonObject**, perform the following, it will return your json object  
    	out.print(response);
    	out.flush();
    	LOGGER.info("The historical data has been retrieved.");
    	disconnect();
    	
    }
    
    public void setRdbURL(String rdbURL) {
		this.rdbURL = rdbURL;
	}
	
	public void setRdbUser(String user) {
		this.rdbUser = user;
	}
	
	public void setRdbPassword(String password) {
		this.rdbPassword = password;
	}
	
	protected void connect() {
		try {
			if (this.conn == null || this.conn.isClosed()) {
				// Load required driver
				Class.forName("org.postgresql.Driver");
				// Connect to DB (using static connection and context properties)
	        	this.conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
	        	this.context = DSL.using(this.conn, dialect); 
	        	System.out.println("Connecting successful: " + this.rdbURL); 
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			System.out.println("Connecting failed: " + this.rdbURL);
			throw new JPSRuntimeException("Establishing database connection failed");
		}
    }

	void disconnect() {
		try {
			conn.close();
			System.out.println("Disconnecting successful"); 
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			System.out.println("Disconnecting failed");
			throw new JPSRuntimeException("Closing database connection failed");
		}
	}
	
	private void populateTableOut (Object timestamp, Object dateTimeWithOffset, String tagID, Object antennaNum, Object RSSI) {	
		
		Table<?> table = DSL.table(DSL.name(tagID));
		InsertValuesStepN<?> insertValueStep = (InsertValuesStepN<?>) context.insertInto(table, timestampColumn, dateTimeWithOffsetColumn, statusColumn, antennaNumberColumn, RSSIColumn);
		insertValueStep = insertValueStep.values(timestamp, dateTimeWithOffset, "Out", antennaNum, RSSI);
		
		insertValueStep.execute();
	}
	
	private void populateTableIn (Object timestamp, Object dateTimeWithOffset, String tagID, Object antennaNum, Object RSSI) {	
		
		Table<?> table = DSL.table(DSL.name(tagID));
		InsertValuesStepN<?> insertValueStep = (InsertValuesStepN<?>) context.insertInto(table, timestampColumn, dateTimeWithOffsetColumn, statusColumn, antennaNumberColumn, RSSIColumn);
		insertValueStep = insertValueStep.values(timestamp, dateTimeWithOffset, "In", antennaNum, RSSI);
		
		insertValueStep.execute();
	}
	
    }

    



