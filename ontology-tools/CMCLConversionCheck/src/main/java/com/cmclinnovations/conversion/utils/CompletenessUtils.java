package com.cmclinnovations.conversion.utils;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import org.slf4j.Logger;

/**
 * A utility class that supports the following functionality:<p>
 * 1. Opens a text or csv file file in read mode.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class CompletenessUtils{
	
	static Logger logger = org.slf4j.LoggerFactory.getLogger(CompletenessUtils.class);
	/**
	 * Creates and returns an instance of the BufferedReader class.</br>
	 * It takes the absolute file path including the file name as input.
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
