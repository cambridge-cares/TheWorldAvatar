package com.cmclinnovations.ontochem.generation.batch;

import java.io.File;
import java.io.IOException;
import org.apache.commons.io.output.FileWriterWithEncoding;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BatchGeneration {

	static Logger logger = LoggerFactory.getLogger(BatchGeneration.class);
	private static String rootDirectory = "C:\\Users\\msff2\\.m2\\repository"; 
	private static String fileNameEndsWith = ".jar";
	private static String owlToBin = "owltobin.bat";
	private static String destinationFolder = System.getProperty("user.home")+File.separator+"adms";
	private static String destinationFile = destinationFolder+File.separator+owlToBin;
	private static FileWriterWithEncoding file;
	private static String FOLDER_LIB = "lib";
	private static String JAR_FILES_LIB_FOLDER = destinationFolder+File.separator+FOLDER_LIB;
	public BatchGeneration() {
		super();
	}
	
	public static void main(String[] args){
		try{
			new File(destinationFolder).mkdirs();
			new File(JAR_FILES_LIB_FOLDER).mkdirs();
			new BatchGeneration().traverseFolderHierarchy(new File(rootDirectory));
			if(file!=null){	
				file.close();
			}
		}catch(IOException e){
			logger.error("The following destination file could not be created:"+destinationFile);
		}

	}
	
	/**
	 * Traverses the folder hierarchy rooted at the folder provided by user. 
	 * 
	 * @param folder the root folder of a hierarchy
	 */
	private void traverseFolderHierarchy(File folder) {
		try{
		if(folder!=null && folder.getAbsolutePath().endsWith(fileNameEndsWith)){
			String fileOnly = folder.getAbsolutePath().substring(folder.getAbsolutePath().lastIndexOf(File.separator)+1);
			if(file==null){
				file = new FileWriterWithEncoding(destinationFile, "UTF-8");
			}
			file.write("."+File.separator+FOLDER_LIB+File.separator+fileOnly+";");
			logger.info("Currently processing the following folder:"+folder.getAbsolutePath());
			try{
				System.out.println("Path:"+"copy "+folder.getAbsolutePath()+" "+JAR_FILES_LIB_FOLDER);
				Process p = Runtime.getRuntime().exec("cmd.exe /c copy "+folder.getAbsolutePath()+" "+JAR_FILES_LIB_FOLDER);
						p.waitFor();
			}catch(IOException e){
				logger.error("Failed to copy the following file "+folder);
				e.printStackTrace();
			}catch(InterruptedException e){
				logger.error("Failed to copy the following file "+folder);
				e.printStackTrace();
			}
		}
		if (folder.isDirectory()) {
			String[] subNote = folder.list();
			for (String fileName : subNote) {
				traverseFolderHierarchy(new File(folder, fileName));
			}
		}
		}catch(IOException e){
			logger.error("Failed to write to the following file "+destinationFile);
			e.printStackTrace();
		}
	}
}
