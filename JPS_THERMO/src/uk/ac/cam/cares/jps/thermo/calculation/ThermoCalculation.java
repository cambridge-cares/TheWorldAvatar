package uk.ac.cam.cares.jps.thermo.calculation;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import uk.ac.cam.cares.jps.thermo.manager.PropertiesManager;

/**
 * 
 * @author NK510
 * Runs thermo calculations implemented by using Python.
 *
 */

public class ThermoCalculation {

	
	private Properties jpsThermoProperties = PropertiesManager.loadProperties(ThermoCalculation.class.getClassLoader().getResourceAsStream("jps_thermo.management.properties"));
	
	private String pythonScript = jpsThermoProperties.getProperty("python.file.path");
	
	/**
	 * 
	 * @param jsonInputFilePath json file that contains results of sparql query over compchem repository (graph).
	 * @param catalinaFolderPath folder inside Apache Tomcat .
	 * @throws IOException
	 * 
	 */	
	
	 public void runThermoCalculation (String jsonInputFilePath, String jsonOutputFilePath) throws IOException {
		
		/**
		 * @author NK510
		 * 
		 * Thermo calculation that runs Python script.
		 * 
		 */
		
		File outputFile = new File(jsonOutputFilePath);
		
		if(!outputFile.exists()) {
			
			outputFile.createNewFile();
		}
		
		File inputFile = new File(jsonInputFilePath);
		
//		String pyscript = catalinaFolderPath + "/conf/Catalina/c4e-dln22-TDC/Source/thermoDriver.py";

		String pyscript =pythonScript;
		
		String[] cmd = { "python", pyscript, "-j", inputFile.getAbsolutePath() };

		Runtime.getRuntime().exec(cmd);
	}	 
}