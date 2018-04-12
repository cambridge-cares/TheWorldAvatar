package uk.ac.cam.cares.jps.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import uk.ac.cam.cares.jps.config.AgentLocator;

public class PythonHelper {

	/**
	 * @param pythonScriptName
	 *            (including package name followed by script name and .py extension,
	 *            e.g. caresjpsadmsinputs/ADMSGeoJsonGetter.py)
	 * @return
	 * @throws IOException
	 */
	public static String callPython(String pythonScriptName, String parameter) throws IOException {
		String path = AgentLocator.getPathToPythonScript(pythonScriptName);
		
		System.out.println(path);

		String[] cmd = { "python", path, parameter };

		Process p = Runtime.getRuntime().exec(cmd);

		BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
		return stdInput.readLine();
	}
	
	public static String callPython(String pythonScriptName, String ADMSOutputFileName, String parameter) throws IOException {
		String pathPythonScript = AgentLocator.getPathToPythonScript(pythonScriptName);
		
		System.out.println(pathPythonScript);
		
		String pathADMSOutput = AgentLocator.getPathToADMSOutputFile(ADMSOutputFileName);
		
		System.out.println(pathADMSOutput);
		
		String[] cmd = { "python", pathPythonScript, pathADMSOutput, parameter };
		
		Process p = Runtime.getRuntime().exec(cmd);
		
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
		return stdInput.readLine();
	}
}
