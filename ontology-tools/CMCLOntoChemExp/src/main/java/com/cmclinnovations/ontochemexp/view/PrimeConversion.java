package com.cmclinnovations.ontochemexp.view;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.filechooser.FileFilter;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochemexp.controller.OntoChemExp;
import com.cmclinnovations.ontochemexp.controller.OntoChemExpFactory;
import com.cmclinnovations.ontochemexp.model.configuration.OperationControlConfig;
import com.cmclinnovations.ontochemexp.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontochemexp.model.utils.Dialogs;

/**
 * Enables users to convert PrIMe experiments into OWL ontologies.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class PrimeConversion {
	private static Logger logger = LoggerFactory.getLogger(PrimeConversion.class);
	private ApplicationContext applicationContext;
	private OperationControlConfig opCtrl;
	private OntoChemExp ontoChemExp;
	int primeExperimentSerialNo = 0;
	int owlExperimentSerialNo = 0;
	long instanceSerialID = System.nanoTime();
	File folder;
	HashMap<String, String> folderVsSourcePrimes = new HashMap<String, String>();
	
	public static void main(String[] args){
		new PrimeConversion().readPrimeExperiments();
	}
	
	private void readPrimeExperiments(){
		folder = openExperimentsFolder();
		conversionTest();
	}
	
	private void conversionTest() {
		if(folder!=null){
			traverseFolderHierarchy(folder);
			logger.info("Finished the conversion of ".concat(Integer.toString(primeExperimentSerialNo))
					.concat(" experiment(s)"));
		}
	}
	
	/**
	 * Requests user to choose a folder which may contain a set of PrIMe files.</br> 
	 * Following the selection of a valid path, it converts a list of PrIMe</br>
	 * files into OWL files representing chemical experiments.</br>
	 * 
     * @return File the folder chosen by user
     */
	private File openExperimentsFolder() {
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
				opCtrl = applicationContext.getBean(OperationControlConfig.class);
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
		if(folder!=null && folder.getAbsolutePath().endsWith(".xml")){
			String folderOnly = folder.getAbsolutePath();
			folderOnly = folderOnly.substring(0, folderOnly.lastIndexOf("\\"));
			logger.info("Processing experiment file "+ ++primeExperimentSerialNo);
			logger.info("Currently processing the following experiment:"+folder.getAbsolutePath());
			folderVsSourcePrimes.put(folderOnly, folder.getAbsolutePath());			
			convertPrimeToOwl(folder.getAbsolutePath(), folderOnly, instanceSerialID);
		}
		if (folder.isDirectory()) {
			String[] subNote = folder.list();
			for (String fileName : subNote) {
				traverseFolderHierarchy(new File(folder, fileName));
			}
		}
	}
	
	/**
	 * Converts a PrIMe file into OWL.
	 * 
	 * @param primeFile the path to the PrIMe experiment file.  
	 * @param owlFilePath the path where the generated OWL file will be stored.
	 */
	private void convertPrimeToOwl(String primeFile, String owlFilePath, long instanceSerialID){
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(primeFile);
		ontoChemExp = OntoChemExpFactory.getOntoChemExp(opCtrl.getOpReadPrime(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilePath);
		try {
			ontoChemExp.convert(instanceSerialID);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch(OntoChemExpException e){
			e.printStackTrace();
		}

	}
}
