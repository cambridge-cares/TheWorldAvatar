package uk.ac.cam.ceb.como.jaxb.parsing.utils;

import java.io.File;

/**
 * @author nk510
 * The Interface Utility.
 */
public interface Utility {

	/**
	 * Gets the file list.
	 *
	 * @param folderPath the folder path
	 * @param format the format
	 * @return the file list
	 */
	public File[] getFileList(String folderPath, final String ...format);	
	
}