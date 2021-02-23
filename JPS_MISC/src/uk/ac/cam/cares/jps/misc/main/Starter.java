package uk.ac.cam.cares.jps.misc.main;

import java.io.IOException;
import java.sql.SQLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.misc.http.AdmsLoop;
import uk.ac.cam.cares.jps.misc.http.HttpGet;
import uk.ac.cam.cares.jps.misc.performance.TestBuildings;
import uk.ac.cam.cares.jps.misc.performance.TestHttp;
import uk.ac.cam.cares.jps.misc.performance.UploadFilesForRDF4J;
import uk.ac.cam.cares.jps.misc.powerplants.performance.TestPowerPlants;

public class Starter extends TestCase {
	/**
	 * Created an instance of Logger to keep record of any exceptional cases<br>
	 * that may happen in this class at run time.
	 */
	private static Logger logger = LoggerFactory.getLogger(Starter.class);
	public static void main(String[] args) throws IOException, SQLException {
				try {
					new Starter().start(args);
				} catch (IOException e) {
					logger.error("TestPowerPlants: An exception occured due to "+e.getMessage());
					System.out.println("TestPowerPlants: The reason for the exception is "+e.getMessage());
					throw new IOException(e.getMessage());
				} catch (SQLException e){
					logger.error("TestPowerPlants: An exception occured due to "+e.getMessage());
					System.out.println("TestPowerPlants: The reason for the exception is "+e.getMessage());
					throw new SQLException(e.getMessage());
				}
	}
	
	public void testMain() {
		//String[] args = new String[] {};
		//String[] args = new String[] {"TestHttp", "http://localhost:8080", "1000", "1000", "false", "true"};
		//String[] args = new String[] {"TestHttp", "http://www.theworldavatar.com", "1000", "1000", "true", "true"};
		//String[] args = new String[] {"TestBuildings"};
		//"http://www.theworldavatar.com/Building/01_buildings4.owl#BuildingGUID_75F6C3E1-4D6D-4087-A81B-3E218741173B"
		//String[] args = new String[] {"TestBuildings", "5", "http://localhost:3030/buildingsthehague", "GUID_75F6C"}; 
		//String[] args = new String[] {"TestBuildings", "1", "http://localhost:8080/rdf4j-server/repositories/buildingsthehague", "GUID_5"}; 
		
		// Upload files
		//String url = "http://localhost:8080/rdf4j-server/repositories/buildingsthehague/statements";
		//String fileName = "C:/Users/Andreas/my/cityGML/buildingsthehague/01_buildings3.owl";
		//String[] args = new String[] {"UploadFiles", url, fileName}; 
		//String dirName = "C:/Users/Andreas/TMP/20190313";
		//String[] args = new String[] {"UploadFiles", url, dirName}; 
		
		
		// HttpGet for AdmsStarter
		//String[] args = new String[] {"HttpGet", "http://www.theworldavatar.com/JPS_SHIP"}; 
		
		
		// AdmsLoop
		String[] args = new String[] {"AdmsLoop", "15", "2019-06-04T07:35:14.000", "2019-06-05T07:35:14.000", "scloop1", "10", "15"}; 
		
		
		try {
			main(args);
		} catch (IOException | SQLException e) {
			logger.error("TestPowerPlants: An exception occured due to "+e.getMessage());
			System.out.println("TestPowerPlants: The reason for the exception is "+e.getMessage());
		}
	}
	
	private void start(String[] args) throws IOException, SQLException {
			
		if (args.length == 0) {
			printHelp();
			return;
		}
		
		System.out.println("starting with arguments:");
		
		for (String current : args) {
			System.out.println(current);
		}
		
		String application = args[0];
		String[] applargs = getApplicationArguments(args);
		
		if ("TestPowerPlants".equals(application)) {
			try {
				new TestPowerPlants().start(applargs);
			} catch (SQLException e) {
				logger.error("TestPowerPlants: An exception occured due to "+e.getMessage());
				System.out.println("TestPowerPlants: The reason for the exception is "+e.getMessage());
				try {
					throw new SQLException(e.getMessage());
				} catch (SQLException e1) {
					logger.error("TestPowerPlants: An exception occured due to "+e.getMessage());
					System.out.println("TestPowerPlants: The reason for the exception is "+e.getMessage());
					throw new SQLException(e.getMessage());
				}
			}
		} else if ("TestHttp".equals(application)) {
			new TestHttp().start(applargs);
		} else if ("TestBuildings".equals(application)) {
			new TestBuildings().start(applargs);
		} else if ("UploadFiles".equals(application)) {
			new UploadFilesForRDF4J().start(applargs);
		} else if ("HttpGet".equals(application)) {
			new HttpGet().start(applargs);
		} else if ("AdmsLoop".equals(application)) {
			new AdmsLoop().start(applargs);
		} else {
			System.out.println("\nApplication not found\n");
			printHelp();
		}
	}
	
	private void printHelp() {
		System.out.println("Please use one of the following names as first argument to run the according application");
		System.out.println("... followed by the application specific arguments\n");

		new TestPowerPlants().printHelp();
		System.out.println();
		new TestHttp().printHelp();
		System.out.println();
		new TestBuildings().printHelp();
		System.out.println();
		new UploadFilesForRDF4J().printHelp();
		System.out.println();
		new HttpGet().printHelp();
		System.out.println();
		new AdmsLoop().printHelp();
	}
	
	private String[] getApplicationArguments(String[] args) {
		String[] result = new String[args.length-1];
		for (int i=1; i<args.length; i++) {
			result[i-1] = args[i];
		}
		return result;
	}
}
