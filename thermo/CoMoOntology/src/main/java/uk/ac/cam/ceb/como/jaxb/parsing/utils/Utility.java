package uk.ac.cam.ceb.como.jaxb.parsing.utils;

import java.io.File;
import java.util.List;

/**
 * The Interface Utility.
 *
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
	
	
	/**
	 * Gets the array file list.
	 *
	 * @param folderPath the folder path
	 * @param format the format
	 * @return the array file list
	 */
	public List<File> getArrayFileList(String folderPath, final String format);
	
}