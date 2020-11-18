package com.cmclinnovations.ontochem.model.utils;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import org.slf4j.Logger;

/**
 * A utility class that supports the following functionalities:<p>
 * 1. open a source csv file or text file 
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class TBoxManagementUtils{
	
	static Logger logger = org.slf4j.LoggerFactory.getLogger(TBoxManagementUtils.class);
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * It takes 
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

}
