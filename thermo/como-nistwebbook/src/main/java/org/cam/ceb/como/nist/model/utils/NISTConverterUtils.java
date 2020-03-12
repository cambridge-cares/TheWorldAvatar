package org.cam.ceb.como.nist.model.utils;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

import org.cam.ceb.como.nist.converter.NISTConverter;
import org.slf4j.Logger;

/**
 * A utility class that supports the following functionality:<p>
 * - Reading any types of text files.
 *  
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class NISTConverterUtils extends NISTConverter{
	
	static Logger logger = org.slf4j.LoggerFactory.getLogger(NISTConverterUtils.class);
	
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
	 * Creates and returns an instance of the BufferedReader class to read</br>
	 * a txt or csv file from the resource folder of a maven project.
	 * 
	 * @param fileName
	 * @return
	 * @throws IOException
	 */
	public static BufferedReader openResourceFile(String fileName) throws IOException{
		InputStream inputStream = NISTConverterUtils.class.getClassLoader().getResourceAsStream(FRONTSLASH.concat(fileName));
		if(inputStream==null){
			inputStream = NISTConverterUtils.class.getClassLoader().getResourceAsStream(fileName);
		}
		InputStreamReader streamReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
		return new BufferedReader(streamReader);
	}

}
