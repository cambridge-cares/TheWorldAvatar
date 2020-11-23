package com.cmclinnovations.ontochemexp.controller;

import java.util.ArrayList;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochemexp.model.configuration.OperationControlConfig;
import com.cmclinnovations.ontochemexp.model.configuration.SpringConfiguration;

/**
 * A factory class that instantiates the PrimeToOwl class following a 
 * user request.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OntoChemExpFactory {
	/**
	 * 
	 * 
	 * @param inputFileType The type of input files, e.g. PrIMe.
	 * @param outputFileType The type of output files, e.g. OWL.
	 * @param sourceFiles The list of input file names, including absolute paths.
	 * @param destination The path where the output or converted files will 
	 * be stored. 
	 * @return An instance of PrimeToOwl or null based on user request. 
	 */
	public static OntoChemExp getOntoChemExp(String inputFileType, String outputFileType, ArrayList<String> sourceFiles, String destination) {
		ApplicationContext applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		OperationControlConfig opCtrl = applicationContext.getBean(OperationControlConfig.class);
		// Application context is closed here to avoid the risk of 
		// information leak.
		((ConfigurableApplicationContext)applicationContext).close();
		// If the input file type is PrIMe and output file type is OWL, it
		// returns an instance of PrimeToOwl converter class.
		if (opCtrl.getOpReadPrime().equalsIgnoreCase(inputFileType)
				&& opCtrl.getOpWriteOwl().equalsIgnoreCase(outputFileType))
			return new PrimeToOwl(sourceFiles, destination);
		// If the input file type is OWL and output file type is PrIMe, it
		// returns an instance of OwlToPrime converter class.
		if (opCtrl.getOpReadOwl().equalsIgnoreCase(inputFileType) 
				&& opCtrl.getOpWritePrime().equalsIgnoreCase(outputFileType)) {
			return new OwlToPrime(sourceFiles, destination);
		}
		// If the above condition does not match, it returns null.
		return null;
	}
}
