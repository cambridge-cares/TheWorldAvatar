package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;



public class CommandHelper {
	/* Author ZHOU XIAOCHI 2018.5.17*/
	
 	
	/**
	 * @param targetFolder target folder path you want to apply the commands upon
	 * @param args An array of commands you want to execute  
	 */
	
	
	//Since the command line commands are dependant on the OS, its imp to identify the OS.
	private static String OS = System.getProperty("os.name").toLowerCase();
	public static boolean isWindows() {

		return (OS.indexOf("win") >= 0);

	}

	public static boolean isMac() {

		return (OS.indexOf("mac") >= 0);

	}

	public static boolean isUnix() {

		return (OS.indexOf("nix") >= 0 || OS.indexOf("nux") >= 0 || OS.indexOf("aix") > 0 );

	}

	public static boolean isSolaris() {

		return (OS.indexOf("sunos") >= 0);

	}
	
	
	private static Logger logger = LoggerFactory.getLogger(CommandHelper.class);


	public static String executeSingleCommand(String targetFolder , String command) 
	{  
	 
		logger.info("In folder: " + targetFolder + " Excuted: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {

			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	 
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			while((line = bfr.readLine()) != null) {
				resultString += line;
			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return resultString; 
	}
	
	public static String executeCommands(String targetFolder , ArrayList<String> commands) {  
	 
		logger.info("In folder: " + targetFolder + " Excuted: " + commands);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		
		try {
			String[] command = commands.toArray(new String[0]);
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			while((line = bfr.readLine()) != null) {
				resultString += line;
			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		int min = Math.min(resultString.length(), 200);
		logger.info("=== Result (only the first 200 characters) === :" + resultString.substring(0, min));
		return resultString; 
	}
	
	public static String executeAsyncSingleCommand(String targetFolder , String command) 
	{  
	 
		logger.info("In folder: " + targetFolder + " Excuted: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			
			if (isWindows()) {
				pr = rt.exec("start  "+command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
			} else if (isMac()) {
				pr = rt.exec("open "+command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
			}
			
			
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
				 
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			while((line = bfr.readLine()) != null) {
				resultString += line;
			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return resultString; 
	}
}