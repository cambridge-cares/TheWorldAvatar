package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CommandHelper {
	/* Author ZHOU XIAOCHI 2018.5.17*/
	
	private static Logger logger = LoggerFactory.getLogger(CommandHelper.class);

	public static String executeSingleCommand(String targetFolder , String command) {  
		List<String> commands = new ArrayList<String>();
		commands.add(command);
		return executeCommands(targetFolder, commands);
	}
	
	/**
	 * @param targetFolder target folder path you want to apply the commands upon
	 * @param args An array of commands you want to execute  
	 */
	public static String executeCommands(String targetFolder , List<String> commands) {  
	 
		logger.info(commands + ", executed in folder: " + targetFolder);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		
		try {
			pr = rt.exec(commands.toArray(new String[0]), null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
		}
		
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			while((line = bfr.readLine()) != null) {
			resultString += line;
			}
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
		}
		
		return resultString; 
	}
}
