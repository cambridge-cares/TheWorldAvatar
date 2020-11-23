package com.cmclinnovations.prime.species.controller;

import java.util.ArrayList;

import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.prime.species.controller.PrimeSpeciesToOWL;
import com.cmclinnovations.prime.species.model.configuration.OperationControlConfig;
import com.cmclinnovations.prime.species.model.configuration.SpringConfiguration;

public class OntoPrimeSpeciesFactory {
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
	public static OntoPrimeSpecies getOntoPrimeSpecies(String inputFileType, String outputFileType, ArrayList<String> sourceFiles, String destination) {
		ApplicationContext applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		OperationControlConfig opCtrl = applicationContext.getBean(OperationControlConfig.class);
		// Application context is closed here to avoid the risk of 
		// information leak.
		((ConfigurableApplicationContext)applicationContext).close();
		// If the input file type is PrIMe and output file type is OWL, it
		// returns an instance of PrimeToOwl converter class.
		if (opCtrl.getOpReadSpecies().equalsIgnoreCase(inputFileType)
				&& opCtrl.getOpWriteOwl().equalsIgnoreCase(outputFileType))
			return new PrimeSpeciesToOWL(sourceFiles, destination);
		// If the input file type is OWL and output file type is PrIMeSpecies, it
		// returns an instance of OwlToPrime converter class.
		if (opCtrl.getOpReadOwl().equalsIgnoreCase(inputFileType) 
				&& opCtrl.getOpWriteSpecies().equalsIgnoreCase(outputFileType)) {
			return new OWLToPrimeSpecies(sourceFiles, destination);
		}
		// If the above condition does not match, it returns null.
		return null;
	}
}
