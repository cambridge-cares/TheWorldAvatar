package com.cmclinnovations.slurm.job;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Utils {
	private static Logger logger = LoggerFactory.getLogger(Utils.class);	
	public static long previousTimeStamp;
	
	/**
	 * Generate unique time stamp to be used in naming Slurm jobs.
	 * 
	 * @return
	 */
	public static long getTimeStamp(){
		long currentTimeStamp = System.nanoTime();
		while(!(currentTimeStamp > previousTimeStamp)){
			currentTimeStamp = System.nanoTime();
		}
		previousTimeStamp = currentTimeStamp;
		return currentTimeStamp;
	}
	
	/**
	 * Creates an instance of the BufferedWriter class.
	 * 
	 * @param filePathPlusName the path plus name of the file being written
	 * @return
	 * @throws IOException
	 */
	public static BufferedWriter openBufferedWriter(String filePathPlusName) throws IOException{
		return new BufferedWriter(new FileWriter(filePathPlusName));
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	public static BufferedReader openSourceFile(String filePathPlusName)
			throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(
				filePathPlusName), "UTF-8"));
	}

	/**
	 * Reads the address of the machine where an instance of DFT Agent is hosted.
	 * 
	 * @return
	 * @throws UnknownHostException
	 */
	public static String getMachineAddress() throws UnknownHostException{
		try {
		   InetAddress address;
		   address = InetAddress.getLocalHost();
		   return address.toString().replace("/", "_");
		} catch (UnknownHostException e){
		    e.printStackTrace();
		}
		return null;
	}

}
