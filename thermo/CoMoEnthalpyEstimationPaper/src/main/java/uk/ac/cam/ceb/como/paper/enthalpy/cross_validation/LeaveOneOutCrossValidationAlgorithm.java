package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing.DataPreProcessing;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSolver;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSpecies;
import uk.ac.cam.ceb.como.paper.enthalpy.reduction.list_calculator.ErrorBarCalculation;

/**
 * 
 * @author nk510 (caresssd@hermes.cam.ac.uk)
 * @author am2145(am2145@cam.ac.uk)
 *
 * This class contains main method that runs Global cross validation algorithm published in [1].
 * 
 * References used in documentation:
 *   
 * [1] Philipp Buerger, First-Principles Investigation of Titanium Dioxide Gas-Phase Precursor Chemistry, PhD thesis, St Edmund’s College, February 7, 2017.
 *   
 */
public class LeaveOneOutCrossValidationAlgorithm {
	
	static String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\\";
    
    static String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv";
    
    static String destRList = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg\\";
    
    static String tempFolder="D:\\Data-Philip\\LeaveOneOutCrossValidation_temp\\";

	public static Map<String, Integer[]> mapElPairing = new HashMap<>();

	public static Map<Species, Integer> spinMultiplicity = new HashMap<>();

	public static Set<Species> validSpecies = new HashSet<Species>();

	public static Set<Species> invalidSpecies = new HashSet<Species>();

	public static Map<Reaction, Double> validReaction = new HashMap<Reaction, Double>();

	public static Map<Reaction, Double> invalidReaction = new HashMap<Reaction, Double>();
	
	public static Map<Species, Double> invalidSpeciesErrorBar = new HashMap<Species, Double>();

	/**
	 * Number of runs.
	 */
	static int[] ctrRuns = new int[] { 1 };

	/**
	 * Number of reactions that will be generated for each species.
	 */
	static int[] ctrRes = new int[] { 5 }; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10

	/**
	 * Number of radicals.
	 */
	static int[] ctrRadicals = new int[] { 100 }; // 0, 1, 2, 3, 4, 5

	public static void main(String[] args) throws Exception {

		LoadSpecies ls = new LoadSpecies();
		
		List<Species> refSpecies = ls.loadSpeciesProperties(ls.loadReferenceSpeciesFiles(srcRefPool),spinMultiplicity,srcCompoundsRef, mapElPairing);

		LoadSolver lSolver = new LoadSolver();

		DataPreProcessing dpp = new DataPreProcessing();

		ErrorBarCalculation errorBarCalculation = new ErrorBarCalculation();
		
		System.out.println("- - - - - - - - - - - - - - - - Pre-processing step - - - - - - - - - - - - - - - -");
		
		/**
		 * 
		 * Data pre-processing step in cross validation algorithm. 
		 * Determine the error metrics for reactions and species. Recommended a list of rejected species.
		 * 
		 */
		dpp.getPreProcessingCorssValidation(1500, 20, destRList, ctrRadicals, ctrRuns, ctrRes, refSpecies, spinMultiplicity, lSolver.loadLPSolver(mapElPairing, 15000, tempFolder), validSpecies, invalidSpecies, validReaction, invalidReaction);
		
		/**
		 * 
		 * Printing valid reactions in txt file and on console.
		 * 
		 */
		BufferedWriter validReactionFile = new BufferedWriter(new FileWriter(destRList +  "valid_reactions" + ".txt", true));
		
		System.out.println("Valid reactions writing . . . ");
		
		errorBarCalculation.generateInitialReactionListFile(validReactionFile, validReaction);
		
		/**
		 * 
		 * Printing invalid reactions in txt file and on console.
		 * 
		 */
		BufferedWriter invalidReactionFile = new BufferedWriter(new FileWriter(destRList + "invalid_reactions" + ".txt", true));
		
		System.out.println("Invalid reactions writing . . .");
		
		errorBarCalculation.generateInitialReactionListFile(invalidReactionFile, invalidReaction);
		
		/**
		 * 
		 * Printing valid species in txt file and on console.
		 * 
		 */
		System.out.println("Valid species writing . . . ");
		
		BufferedWriter validSpeciesFile = new BufferedWriter(new FileWriter(destRList + "valid_species" + ".txt", true));
		
		errorBarCalculation.generateInitialValidSpeciesFile(validSpeciesFile, validSpecies);
		
		/**
		 * 
		 * Printing invalid species in txt file and on console.
		 * 
		 */
		System.out.println("Invalid species writing . . .");
		
		BufferedWriter invalidSpeciesFile = new BufferedWriter(new FileWriter(destRList + "invalid_species" + ".txt", true));

		errorBarCalculation.generateInitialInvalidSpeciesFile(invalidSpeciesFile, invalidSpecies,validSpecies);		
		
		/**
		 * 
		 * Error bar is average of all errors generated for each reaction of given species.
		 *  
		 * Calculates error bar for each species in invalid set of species.
		 */
		
		invalidSpeciesErrorBar.putAll(errorBarCalculation.calculateSpeciesErrorBar(invalidReaction, validSpecies, invalidSpecies));

		/**
		 * Reports a species name and its maximum error bar from the set of all error bars of rejected species.
		 */
		double error = errorBarCalculation.getMaximumErrorBar(invalidSpeciesErrorBar);
		
		System.out.println("error: " + error);
		
		 ls.loadReferenceSpeciesForInitialAnalysis(srcRefPool, validSpecies);
		 
		 
		
		
}

}
