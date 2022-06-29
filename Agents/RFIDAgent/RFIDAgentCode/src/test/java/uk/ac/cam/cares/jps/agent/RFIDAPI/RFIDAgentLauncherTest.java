package uk.ac.cam.cares.jps.agent.RFIDAPI;

import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import org.springframework.mock.web.MockHttpServletRequest;
import org.apache.log4j.AppenderSkeleton;
import org.apache.logging.log4j.LogManager;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;




@Ignore("Requires postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class RFIDAgentLauncherTest {

	
	private static final org.apache.logging.log4j.Logger LOGGER = LogManager.getLogger(RFIDAgentLauncher.class);
	
	 // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
	
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    
    private final String clientPropertiesFilename = "client.properties";
    File clientPropertyFile;
    JSONObject requestParams;

    
    @Before
    public void initializeAgent() throws IOException {
    	try {
            // Start postgreSQL container
            postgres.start();
        } catch (Exception e) {
            throw new AssertionError("Docker container startup failed. Please try running tests again");
        }
    	 clientPropertyFile= folder.newFile(clientPropertiesFilename);
    	 String propertiesFile = Paths.get(folder.getRoot().toString(), clientPropertiesFilename).toString();
         try (FileWriter writer = new FileWriter(propertiesFile, false)) {
             writer.write("db.url="+postgres.getJdbcUrl()+"\n");
             writer.write("db.user="+postgres.getUsername()+"\n");
             writer.write("db.password="+postgres.getPassword()+"\n");
         }
    }
    
 // Cleaning up containers after each test, otherwise unused containers will first be killed when all tests finished
    @After
    public void stopContainers() {
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }
    
	@Test
    public void testProcessRequestParametersSuccessful() {
    	RFIDAgentLauncher testLauncher = new RFIDAgentLauncher();
    	JSONObject correctRequestParams = new JSONObject();
    	correctRequestParams.put("acceptHeaders", "*/*");
    	correctRequestParams.put("method", "POST");
    	correctRequestParams.put("requestUrl", "http://localhost:1016/rfid-agent/");
    	correctRequestParams.put("body", "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"> <s:Body> <GetRFIDData xmlns=\"http://tempuri.org/\"><value1>00000000000000A000009727,0,-85<\\/value1><\\/GetRFIDData><\\/s:Body><\\/s:Envelope>");
    	correctRequestParams.put("contentType","application/xml");
    	HttpServletRequest request = null;
    	try {
        	SystemLambda.withEnvironmentVariable("RFID_PROPERTIES", clientPropertyFile.getCanonicalPath()).execute(() -> {
        		//a successful run will return back the request parameters where requestParams == correctRequestParams
        		requestParams = testLauncher.processRequestParameters(correctRequestParams, request);
        		Assert.assertTrue(requestParams.toString().contains(correctRequestParams.toString()));
        	 });
        }
        catch (Exception e) {
        } 
    	
    	//{"acceptHeaders":"*/*","method":"POST","requestUrl":"http://localhost:1016/rfid-agent/","body":"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"> <s:Body> <GetRFIDData xmlns=\"http://tempuri.org/\"><value1>00000000000000A000009727,0,-85<\/value1><\/GetRFIDData><\/s:Body><\/s:Envelope>","contentType":"application/xml"}
    }
	
	@Test
    public void testProcessRequestParametersFailed() {
    	RFIDAgentLauncher testLauncher = new RFIDAgentLauncher();
    	JSONObject correctRequestParams = new JSONObject();
    	correctRequestParams.put("acceptHeaders", "*/*");
    	correctRequestParams.put("method", "POST");
    	correctRequestParams.put("requestUrl", "http://localhost:1016/rfid-agent/");
    	correctRequestParams.put("body", "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"> <s:Body> <GetRFIDData xmlns=\"http://tempuri.org/\"><value1>00000000000000A000009727,0,-85<\\/value1><\\/GetRFIDData><\\/s:Body><\\/s:Envelope>");
    	correctRequestParams.put("contentType","application/xml");
    	HttpServletRequest request = null;
    	try {
    		//incorrect environment variable assigned to client.properties
        	SystemLambda.withEnvironmentVariable("RFID_PROPER", clientPropertyFile.getCanonicalPath()).execute(() -> {
        		requestParams = testLauncher.processRequestParameters(correctRequestParams, request);
        		
        	 });
        }
        catch (Exception e) {
        	Assert.assertEquals(NullPointerException.class, e.getClass());
        } 
    	
    	//{"acceptHeaders":"*/*","method":"POST","requestUrl":"http://localhost:1016/rfid-agent/","body":"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"> <s:Body> <GetRFIDData xmlns=\"http://tempuri.org/\"><value1>00000000000000A000009727,0,-85<\/value1><\/GetRFIDData><\/s:Body><\/s:Envelope>","contentType":"application/xml"}
    }
	
	@Test
	public void testDoGetSuccessful() {
		//Logger and appender to retrieve logger.info messages
		final TestAppender appender = new TestAppender();
	    final Logger logger = Logger.getLogger(RFIDAgentLauncher.class);
	    logger.addAppender(appender);
		
		String readingPath = String.join("/","http://localhost:1016", "rfid-agent", "values=status", "limit=3", "keys=00000000000000A000009727"); 
		MockHttpServletRequest readingRequest = new MockHttpServletRequest();
		//mock correct request URI
		readingRequest.setRequestURI(readingPath);
		
		//run processRequestParameters once to have data saved to the postgresql database
		 RFIDAgentLauncher testLauncher = new RFIDAgentLauncher();
		 JSONObject correctRequestParams = new JSONObject();
	    	correctRequestParams.put("acceptHeaders", "*/*");
	    	correctRequestParams.put("method", "POST");
	    	correctRequestParams.put("requestUrl", "http://localhost:1016/rfid-agent/");
	    	correctRequestParams.put("body", "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"> <s:Body> <GetRFIDData xmlns=\"http://tempuri.org/\"><value1>00000000000000A000009727,0,-85<\\/value1><\\/GetRFIDData><\\/s:Body><\\/s:Envelope>");
	    	correctRequestParams.put("contentType","application/xml");
	    	HttpServletRequest request = null;
	    	
	    	
		 try {
			 try {
		        
	        	SystemLambda.withEnvironmentVariable("RFID_PROPERTIES", clientPropertyFile.getCanonicalPath()).execute(() -> {
	        		requestParams = testLauncher.processRequestParameters(correctRequestParams, request);
	        		
	        		//run doGet with the mock request to retrieve the historical data
	        		testLauncher.doGet(readingRequest, null);
	        	
	        		//retrieve Logger.info message, a successful run of doGet should Log a .info message ("The historical data has been retrieved.")
	        		final List<LoggingEvent> log = appender.getLog();
	                final LoggingEvent firstLogEntry = log.get(0);
	                
	        		Assert.assertTrue(firstLogEntry.getMessage().toString().contains("The historical data has been retrieved."));
	        	 });
			 }
		        finally {
		            logger.removeAppender(appender);
		        }
	        }
	        catch (Exception e) {
	        } 
		 
	}
	
	@Test
	public void testDoGetFailed() {
		
		
		//Logger and appender to retrieve logger.info messages
		String readingPath = String.join("/","http://localhost:1016", "rfid-agent","valu=status", "limit=3", "keys=00000000000000A000009727"); 
		MockHttpServletRequest readingRequest = new MockHttpServletRequest();
		//mock correct request URI
		readingRequest.setRequestURI(readingPath);
		
		//run processRequestParameters once to have data saved to the postgresql database
		 RFIDAgentLauncher testLauncher = new RFIDAgentLauncher();
	    	
		 try {
		        
	        	SystemLambda.withEnvironmentVariable("RFID_PROPERTIES", clientPropertyFile.getCanonicalPath()).execute(() -> {
	        		//run doGet with the mock request to retrieve the historical data
	        		testLauncher.doGet(readingRequest, null);
	        		Assert.fail();
	        	 });
			 
	        }
	        catch (Exception e) {
	        	Assert.assertEquals("The query parameters in the request URL is not written correctly.", e.getMessage());
	        } 
	}
	 
	
	
	
    
}

//Appender class for retrieving logging messages and events
class TestAppender extends AppenderSkeleton {
    private final List<LoggingEvent> log = new ArrayList<LoggingEvent>();

    @Override
    public boolean requiresLayout() {
        return false;
    }

    @Override
    protected void append(final LoggingEvent loggingEvent) {
        log.add(loggingEvent);
    }

    @Override
    public void close() {
    }

    public List<LoggingEvent> getLog() {
        return new ArrayList<LoggingEvent>(log);
    }
}
