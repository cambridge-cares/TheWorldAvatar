/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.parsing.utils;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.collect.Lists;

// TODO: Auto-generated Javadoc
/**
 * The Class FileUtility.
 */

public class FileUtility implements Utility {

	/**
	 * Gets file list.
	 *
	 * @author nk510
	 * @param folderPath the folder path
	 * @param format the format
	 * @return <p>Method reads all files in given folder path. Supported file
	 *         extensions are '.owl', '.rdf', '.ttl', '.sparql', '.xml', '.json'. </p>
	 */
	
	public File[] getFileList(String folderPath, final String ...format) {

		File dir = new File(folderPath);
		
		File[] fileList = dir.listFiles(new FilenameFilter(){
			
			public boolean accept(File dir, String name) {
				
				return (name.endsWith(format[0]) || name.endsWith(format[1]) || name.endsWith(format[2]));
			}
		});

		return fileList;
	}	

	
	/**
	 * Gets the array file list.
	 *
	 * @param folderPath the folder path
	 * @param strings the format 
	 * @return <p>The array file list all files in given folder path. Supported file
	 *         extensions are '.owl', '.rdf', '.ttl', '.sparql', '.xml', '.json'.</p>
	 */
	public List<File> getArrayFileList(String folderPath, final String strings){
		
		File dir = new File(folderPath);
		
		List<File> list = Arrays.asList(dir.listFiles(new FilenameFilter(){
	       
	        public boolean accept(File dir, String name) {
	            return name.endsWith(strings); // or something else
	        }}));
		
		return list;
	}	
}