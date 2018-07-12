/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.parsing.utils;

import java.io.File;
import java.io.FilenameFilter;

/**
 * The Class FileUtility.
 */

public class FileUtility {

	/**
	 * Gets the ontology file list.
	 *
	 * @author nk510
	 * @param folderPath the folder path
	 * @return <p>Method reads all ontology files in given folder path. Supported file
	 *         extensions are '.owl', '.rdf', '.ttl'.</p>
	 */
	
	public File[] getOntologyFileList(String folderPath) {

		File dir = new File(folderPath);

		
		File[] fileList = dir.listFiles(new FilenameFilter(){
			
			public boolean accept(File dir, String name) {
				
				return (name.endsWith(".owl") || name.endsWith(".rdf") || name.endsWith(".ttl"));
			}
		});

		return fileList;
	}	
	
}