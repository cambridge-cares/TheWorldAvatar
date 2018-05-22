package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletResponse;

import org.junit.platform.commons.logging.LoggerFactory;

import com.sun.istack.internal.logging.Logger;

public class CommandHelper {
	/* Author ZHOU XIAOCHI 2018.5.17*/
	
 	
	/**
	 * @param targetFolder target folder path you want to apply the commands upon
	 * @param args An array of commands you want to execute  
	 */
	
	
	public static String executeSingleCommand(String targetFolder , String command) 
	{  
	 
		Runtime rt = Runtime.getRuntime();
	
		
		Process pr = null;
				try {
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
				 
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			while((line = bfr.readLine()) != null) {
			resultString += line;
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return resultString; 
	}
	
	
	
	
	public static String executeCommands(String targetFolder , ArrayList<String> commands) 
	{  
	 
		
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
				try {
			pr = rt.exec(commands.toArray(new String[0]), null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			while((line = bfr.readLine()) != null) {
			resultString += line;
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return resultString; 
	}
	
	
	
	
	
}
