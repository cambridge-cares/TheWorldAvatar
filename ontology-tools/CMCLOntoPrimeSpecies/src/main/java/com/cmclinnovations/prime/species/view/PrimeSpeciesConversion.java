package com.cmclinnovations.prime.species.view;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.filechooser.FileFilter;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.prime.species.controller.OntoPrimeSpecies;
import com.cmclinnovations.prime.species.controller.OntoPrimeSpeciesFactory;
import com.cmclinnovations.prime.species.model.configuration.OperationControlConfig;
import com.cmclinnovations.prime.species.model.configuration.SpringConfiguration;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;
import com.cmclinnovations.prime.species.model.utils.Dialogs;

/**
 * Enables users to convert PrIMe chemicalSpecies into OWL ontologies.
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
public class PrimeSpeciesConversion {
	private static Logger logger = LoggerFactory.getLogger(PrimeSpeciesConversion.class);
	private ApplicationContext applicationContext;
	private OperationControlConfig opCtrl;
	private OntoPrimeSpecies ontoPrimeSpecies;
	int primeSpeciesSerialNo = 0;
	int owlSpeciesSerialNo = 0;
	long instanceSerialID = System.nanoTime();
	File folder;
	HashMap<String, String> folderVsSourcePrimes = new HashMap<String, String>();
	
	public static void main(String[] args) {
		new PrimeSpeciesConversion().readPrimeSpecies();
	}
	
	private void readPrimeSpecies() {
		folder = openSpeciesFolder();
		conversionTest();
	}
	
	private void conversionTest() {
		if (folder!=null) {
			traverseFolderHierarchy(folder);
			logger.info("Finished the conversion of ".concat(Integer.toString(primeSpeciesSerialNo)).concat(" species"));
		}
	}
	
	/**
	 * Requests user to choose a folder which may contain a set of PrIMe files.</br> 
	 * Following the selection of a valid path, it converts a list of PrIMe</br>
	 * files into OWL files representing chemical experiments.</br>
	 * 
     * @return File the folder chosen by user
     */
	private File openSpeciesFolder() {
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
			logger.info("Processing experiment file "+ ++primeSpeciesSerialNo);
			logger.info("Currently processing the following experiment:"+folder.getAbsolutePath());
			folderVsSourcePrimes.put(folderOnly, folder.getAbsolutePath());			
			convertPrimeSpeciesToOwl(folder.getAbsolutePath(), folderOnly, instanceSerialID);
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
	private void convertPrimeSpeciesToOwl(String primeFile, String owlFilePath, long instanceSerialID) {
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(primeFile);
		ontoPrimeSpecies = OntoPrimeSpeciesFactory.getOntoPrimeSpecies(opCtrl.getOpReadSpecies(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilePath);
		try {
			ontoPrimeSpecies.convert(instanceSerialID);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch(OntoPrimeSpeciesException e){
			e.printStackTrace();
		}
	}
}
