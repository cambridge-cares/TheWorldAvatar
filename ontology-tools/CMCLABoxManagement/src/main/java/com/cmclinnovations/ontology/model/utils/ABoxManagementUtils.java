package com.cmclinnovations.ontology.model.utils;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import org.slf4j.Logger;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * A utility class that supports the following functionality:<p>
 * 1. Opens a text or csv file file in read mode.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ABoxManagementUtils{
	
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ABoxManagementUtils.class);
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
	
	/**
	 * Forms the absolute path including the name of the source file being</br> 
	 * generated using:</br>
	 * (i) the absolute path including the name of the OWL file being</br> 
	 * converted, and</br>
	 * (ii) the path user indicates to save the sourceL file after the</br> 
	 * conversion.
	 * 
	 * @param owlFileAbsolutePath
	 * @param sourceFilePath
	 *   
	 * @return the absolute path including the name of the source file being 
	 * generated
	 * @throws ABoxManagementException
	 */
	public static String formSourceFileAbsoultePath(String owlFileAbsolutePath, String sourceFilePath) throws ABoxManagementException {
		if (owlFileAbsolutePath == null) {
			logger.error("Provided owlFileAbsolutePath path is null.");
			throw new ABoxManagementException("Provided owlFileAbsolutePath path is null.");
		}
		if (sourceFilePath == null) {
			logger.error("Provided sourceFilePath is null.");
			throw new ABoxManagementException("Provided sourceFilePath is null.");
		}
		String owlFileName = extractSourceFileName(owlFileAbsolutePath);
		owlFileName = owlFileName.concat(".xml");
		sourceFilePath = removeFilePrefix(sourceFilePath);
		if(sourceFilePath.contains("/")){
			return sourceFilePath.concat("/").concat(owlFileName);
		}
		return sourceFilePath.concat("\\").concat(owlFileName);
	}

	/**
	 * Removes the preamble "file:\" or "file:/"  from the current file path. 
	 * 
	 * @param sourceFilePath
	 * @return
	 */
	private static String removeFilePrefix(String sourceFilePath){
		if(sourceFilePath.startsWith("file:\\")){
			sourceFilePath = sourceFilePath.replace("file:\\", "");
		}
		if(sourceFilePath.startsWith("file:/")){
			sourceFilePath = sourceFilePath.replace("file:/", "");
		}
		return sourceFilePath;
	}
	
	/**
	 * Extracts the name of the source file name being processed from an
	 * OWL file path. 
	 * 
	 * @param file The file path
	 * @return String returns the name of the file 
	 * @throws ABoxManagementException
	 */
	public static String extractSourceFileName(String file) throws ABoxManagementException {
		if (!(file.contains("\\") || file.contains("/"))) {
			logger.error("Unexpected file path.");
			throw new ABoxManagementException("Unexpected file path.");
		}
		String tokens[];
		if (file.contains("\\")) {
			tokens = file.split("\\".concat("\\"));
		} else {
			tokens = file.split("/");
		}
		if (tokens.length < 2) {
			logger.error("The file path is unexpectedly short.");
			throw new ABoxManagementException("The file path is unexpectedly short.");
		}
		if (!tokens[tokens.length - 1].contains(".owl")) {
			logger.error("Provided owl file name does not contain the owl extension.");
			throw new ABoxManagementException("Provided owl file name does not contain the owl extension.");
		}
		System.out.println("token below:"+tokens[tokens.length - 1].replace(".owl", ""));
		return tokens[tokens.length - 1].replace(".owl", "");
	}
}
