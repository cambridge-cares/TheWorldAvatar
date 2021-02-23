package com.cmclinnovations.ontokin.controller;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import javax.swing.filechooser.FileFilter;

import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.controller.OntoKin;
import com.cmclinnovations.ontochem.controller.OntoKinFactory;
import com.cmclinnovations.ontochem.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.utils.Dialogs;

/**
 * Tests the production of a version of Ontokin KB.
 * 
 * @author msff2
 *
 */
public class KBGenerationTest {
	private static Logger logger = LoggerFactory.getLogger(KBGenerationTest.class);
	private ApplicationContext applicationContext;
	private AppConfigOperationControl opCtrl;
	private OntoKin ontoKin;
	int ctmlMechanismSerialNo = 0;
	int owlMechanismSerialNo = 0;
	int noOfDiffInXmlComment = 0;
	private String PATH_KB = "";
	private String FILE_NAME_ABOX_ONTOKIN = "ontokinabox.txt";
	private BufferedWriter bw;
	private ArrayList<String> sourceFiles = new ArrayList<String>();
	
	File folder;
	
	@Before
	public void readAllMechanisms(){
		folder = openMechanismFolder();
		getSavingFolder();
	}
	
	@Test
	public void converstionTest() {
		if(folder!=null){
			bw = getBufferedWriter(PATH_KB.concat(FILE_NAME_ABOX_ONTOKIN));
			traverseFolderHierarchy(folder);
			convertCtmlToOwl(sourceFiles, PATH_KB);
			try{
				bw.close();
			}catch(IOException e){
				logger.error("Ontokinabox.txt could not be closed propery.");
				e.printStackTrace();
			}
			logger.info("Finished the conversion of ".concat(Integer.toString(ctmlMechanismSerialNo))
					.concat(" mechanism(s)"));
		}
	}
	
	/**
	 * Requests user to choose a folder which may contain a set of mechanisms 
	 * in CTML format.</br> 
	 * Following the selection of a valid path, it converts a list of CTML</br>
	 * files into OWL files representing chemical mechanism ontologies.</br>
	 * 
     * @return File the folder chosen by user
     */
	private File openMechanismFolder() {
		File folder = Dialogs.selectDirectoryDialog(new File(System.getProperty("user.home")), new FileFilter[] {},
				false);
		if (folder == null) {
		} else if (!folder.exists()) {
			Dialogs.showErrorDialog("Selected folder does not exist.", "Read");
		}

		if (folder != null) {
			if (applicationContext == null) {
				applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
			}
			if (opCtrl == null) {
				opCtrl = applicationContext.getBean(AppConfigOperationControl.class);
			}
		}
		return folder;
    }

	/**
	 * Traverses the folder hierarchy rooted at the folder chosen by user. 
	 * 
	 * @param folder the root folder of a hierarchy
	 */
	private void traverseFolderHierarchy(File folder) {
		if(folder!=null && folder.getAbsolutePath().endsWith("\\mechanism.xml")){
			String folderOnly = folder.getAbsolutePath();
			folderOnly = folderOnly.substring(0, folderOnly.lastIndexOf("\\mechanism.xml"));
			logger.info("Processing mechanism "+ ++ctmlMechanismSerialNo);
			logger.info("Currently processing the following mechanism:"+folder.getAbsolutePath());
			sourceFiles.add(folder.getAbsolutePath());
		}
		if (folder.isDirectory()) {
			String[] subNote = folder.list();
			for (String fileName : subNote) {
				traverseFolderHierarchy(new File(folder, fileName));
			}
		}
	}
	
	/**
	 * Converts a CTML file into OWL.
	 * 
	 * @param ctmlFile the path to the CTML file.  
	 * @param absoluteFilePath the absolute path to the file being processed
	 */
	private void convertCtmlToOwl(ArrayList<String> sourceFiles, String owlFilesPath){
		ontoKin = OntoKinFactory.getOntoKin(opCtrl.getOpReadCtml(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilesPath);
		try {
			ontoKin.convert();
			String owlFileName = generateOwlFileName(owlFilesPath);
//			bw.write("test "+owlFileName.concat("\n"));
		} catch (OWLOntologyCreationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch(OntoException e){
			e.printStackTrace();
		}
//		} catch(IOException e){
//			e.printStackTrace();
//		}

	}
	/**
	 * Produces the name of the current OWL file from its path.
	 * 
	 * @param owlFilesPath
	 * @return
	 */
	private String generateOwlFileName(String owlFilesPath){
		if(owlFilesPath.contains("\\")){
			return owlFilesPath.substring(owlFilesPath.lastIndexOf("\\")+1);
		}
		return owlFilesPath;
	}
	
	/**
	 * Produces an instance of BufferedWriter to write a file.
	 * 
	 * @param filePathPlusName
	 */
	private BufferedWriter getBufferedWriter(String filePathPlusName) {
		BufferedWriter fileCommented = null;
		try {
			fileCommented = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(filePathPlusName), "UTF8"));
		} catch (IOException e) {
			logger.error("A BufferedWriter object for the following file could not be created.");
			e.printStackTrace();
		}
		return fileCommented;
	}
	
	/**
	 * Asks user to provide the folder where they want to save the KB.
	 */
	private void getSavingFolder(){
        File directory = Dialogs.selectDirectoryDialog(new File(System.getProperty("user.home")), null, true);
		if (folder == null) {
		} else if (!folder.exists()) {
			Dialogs.showErrorDialog("Selected folder does not exist.", "Read");
		} else{			
				PATH_KB = directory.getAbsolutePath();
		}
	}
	
}
