package com.cmclinnovations.ontochem.controller;

import java.util.ArrayList;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;

/**
 * A factory class that instantiates either the CtmlToOwl class or OwlToCtml
 * class based on user's selection of input file type and output file type.
 * If the selection of the input file type is equal to CTML and the output file
 * type is equal to OWL, it will instantiate the CtmlToOwl class. In the case
 * of OWL as the input file type and CTML as the output file type, it will
 * instantiate the OwlToCtml class.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OntoKinFactory {
	/**
	 * 
	 * @param inputFileType The type of input files, e.g. CTML.
	 * @param outputFileType The type of output files, e.g. OWL.
	 * @param sourceFiles The list of input file names, including absolute paths.
	 * @param destination The path where the output or converted files will 
	 * be stored. 
	 * @return An instance of OwlToCtml or CtmlToOwl based on user request. 
	 */
	public static OntoKin getOntoKin(String inputFileType, String outputFileType, ArrayList<String> sourceFiles, String destination) {
		ApplicationContext applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		AppConfigOperationControl opCtrl = applicationContext.getBean(AppConfigOperationControl.class);
		// Application context is closed here to avoid the risk of 
		// information leak.
		((ConfigurableApplicationContext)applicationContext).close();
		// If the input file type is CTML and output file type is OWL, it
		// returns an instance of CtmlToOwl converter class.
		if (opCtrl.getOpReadCtml().equalsIgnoreCase(inputFileType)
				&& opCtrl.getOpWriteOwl().equalsIgnoreCase(outputFileType))
			return new CtmlToOwl(sourceFiles, destination);
		// If the input file type is OWL and output file type is CTML, it
		// returns an instance of OwlToCtml converter class.
		if (opCtrl.getOpReadOwl().equalsIgnoreCase(inputFileType)
				&& opCtrl.getOpWriteCtml().equalsIgnoreCase(outputFileType))
			return new OwlToCtml(sourceFiles, destination);
		// If none of the above conditions matches, it returns null.
		return null;
	}
}
