package uk.ac.cam.cares.jps.thermo.calculation;

import java.io.File;
import java.io.IOException;

public class ThermoCalculation {

	public void runThermoCalculation (String jsonInputFilePath, String catalinaFolderPath) throws IOException {
		
		/**
		 * @author NK510
		 * Thermo calculation that runs Python script
		 * 
		 */
		File inputFile = new File(jsonInputFilePath);
		
		String pyscript = catalinaFolderPath + "/conf/Catalina/c4e-dln22-TDC/Source/thermoDriver.py";

		String[] cmd = { "python", pyscript, "-j", inputFile.getAbsolutePath(), };

		Runtime.getRuntime().exec(cmd);
		
	}
}