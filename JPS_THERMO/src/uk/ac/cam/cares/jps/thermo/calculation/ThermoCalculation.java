package uk.ac.cam.cares.jps.thermo.calculation;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import uk.ac.cam.cares.jps.thermo.manager.PropertiesManager;

/**
 * 
 * @author NK510
 * Runs thermo calculations implemented by using Python. It calculates NASA polynomials for selected species by using temperature and enthalpy of formation for selected species.
 *
 */

public class ThermoCalculation {

	
	
	private Properties jpsThermoProperties = PropertiesManager.loadProperties(ThermoCalculation.class.getClassLoader().getResourceAsStream("jps_thermo.management.properties"));
	
	private String pythonScript = jpsThermoProperties.getProperty("python.file.path");
	
	/**
	 * 
	 * @param jsonInputFilePath The json file that contains results of sparql query over compchem repository (graph).
	 * @param catalinaFolderPath The folder inside Apache Tomcat .
	 * @throws IOException The IO exception.
	 * 
	 */	
	
	 public void runThermoCalculation (String jsonInputFilePath, String jsonOutputFilePath, String enthalpyOfFormation) throws IOException {
		 
		/**
		 * 
		 * @author NK510
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
		
		if(!enthalpyOfFormation.isEmpty()) {
			
		String[] cmd = { "python", pyscript, "-j", inputFile.getAbsolutePath(), "--href " , "\"" + enthalpyOfFormation +"\"" };

		Runtime.getRuntime().exec(cmd);
		
		}else {
			
			String[] cmd = { "python", pyscript, "-j", inputFile.getAbsolutePath() };

			Runtime.getRuntime().exec(cmd);
		}
		
	}	 
}