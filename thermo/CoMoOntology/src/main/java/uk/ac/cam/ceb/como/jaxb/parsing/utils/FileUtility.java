/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.parsing.utils;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.List;

/**
 * The Class FileUtility.
 */

public class FileUtility implements Utility {

	/**
	 * Gets file list.
	 *
	 * @author nk510
	 * @param folderPath the folder path 
	 * @param format the format of files in file list
	 * @return <p>Method reads all files in given folder path. Supported file
	 *         extensions are '.owl', '.rdf', '.ttl', '.sparql', '.xml', '.json'. </p>
	 */
	
	public File[] getFileList(String folderPath, final String ...format) {


		File dir = new File(folderPath);
		
		File[] fileList = dir.listFiles(new FilenameFilter(){
			
			public boolean accept(File dir, String name) {
				
				if(format.length==1) {
					
					return (name.endsWith(format[0]));
					
				}else if(format.length==2) {
					
					return (name.endsWith(format[0]) || name.endsWith(format[1]));
					
				}else if(format.length==3) {
					
				return (name.endsWith(format[0]) || name.endsWith(format[1]) || name.endsWith(format[2]));
				}
				
			return false;	
			
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
	        	
	            return name.endsWith(strings);
	        
	        }}));
		
		return list;
	}	
}