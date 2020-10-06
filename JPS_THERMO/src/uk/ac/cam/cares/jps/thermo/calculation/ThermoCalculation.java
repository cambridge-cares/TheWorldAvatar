package uk.ac.cam.cares.jps.thermo.calculation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
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

		 System.out.println("JsonInputFilePath:"+jsonInputFilePath);
		 System.out.println("jsonOutputFilePath:"+jsonOutputFilePath);
		 System.out.println("enthalpyOfFormation:"+enthalpyOfFormation);
		/**
		 * 
		 * 
		 * @author NK510
		 * Thermo calculation that runs Python script.
		 * 
		 */
		
		File outputFile = new File(jsonOutputFilePath);
		
		if(!outputFile.exists()) {
			if(!new File(jsonOutputFilePath.substring(0, jsonOutputFilePath.lastIndexOf("/"))).exists()){
				new File(jsonOutputFilePath.substring(0, jsonOutputFilePath.lastIndexOf("/"))).mkdir();
				System.out.println("Created directory:"+jsonOutputFilePath.substring(0, jsonOutputFilePath.lastIndexOf("/")));
			}
			outputFile.createNewFile();
		}
		
		File inputFile = new File(jsonInputFilePath);
		
//		String pyscript = catalinaFolderPath + "/conf/Catalina/c4e-dln22-TDC/Source/thermoDriver.py";

		String pyscript =pythonScript;
		
		if(!enthalpyOfFormation.isEmpty()) {
			
		String[] cmd = { "python", pyscript, "-j", inputFile.getAbsolutePath(), "--href " , "\"" + enthalpyOfFormation +"\"" };

		Runtime.getRuntime().exec(cmd);
		if(!(new File(jsonOutputFilePath).exists()) || isEmpty(jsonOutputFilePath)){
			Runtime.getRuntime().exec("python "+pyscript+" -j "+inputFile.getAbsolutePath()+" --href \""+enthalpyOfFormation+"\"");			
		}
		}else {
			String[] cmd = { "python", pyscript, "-j", inputFile.getAbsolutePath() };
			Runtime.getRuntime().exec(cmd);
			if(!(new File(jsonOutputFilePath).exists()) || isEmpty(jsonOutputFilePath)){
				Runtime.getRuntime().exec("python "+pyscript+" -j "+inputFile.getAbsolutePath());
			}
		}
	}
	 
	 /**
	  * Checks if a file is empty.
	  * 
	  * @param filePath
	  * @return
	  * @throws IOException
	  */
	 private boolean isEmpty(String filePath) throws IOException{
		 BufferedReader br = openSourceFile(filePath);
		 String line;
		 while((line=br.readLine())!=null){
			 if(!line.trim().isEmpty()){
				 return false;
			 }
		 }
		 return true;
	 }
		/**
		 * Creates and returns an instance of the BufferedReader class.
		 * 
		 * @param filePathPlusName
		 *            the path plus name of the file being read
		 * @return
		 * @throws IOException
		 */
		public BufferedReader openSourceFile(String filePathPlusName)
				throws IOException {
			return new BufferedReader(new InputStreamReader(new FileInputStream(
					filePathPlusName), "UTF-8"));
		}
}