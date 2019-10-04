package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ca.ceb.como.paper.enthalpy.data.analysis.InitialDataAnalysis;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing.DataPreProcessing;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSolver;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSpecies;
import uk.ac.cam.ceb.como.paper.enthalpy.reduction.list_calculator.ErrorBarCalculation;
import uk.ac.cam.ceb.paper.sort.Sort;

/**
 * 
 * @author nk510 (caresssd@hermes.cam.ac.uk)
 * @author am2145(am2145@cam.ac.uk)
 *
 *         This class contains main method that runs Global cross validation
 *         algorithm published in [1].
 * 
 *         References used in documentation:
 * 
 *         [1] Philipp Buerger, First-Principles Investigation of Titanium
 *         Dioxide Gas-Phase Precursor Chemistry, PhD thesis, St Edmund’s
 *         College, February 7, 2017.
 * 
 *         This code requires at least Java 1.8
 *
 */

public class LeaveOneOutCrossValidationAlgorithm {

	static String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\\";
	/**
	 * HPC settings
	 */
//	static String srcCompoundsRef = "g09/";

	static String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv";
	/**
	 * HPC settings
	 */
//	static String srcRefPool = "csv/ref_scaled_kJperMols_v8.csv";

	static String destRList = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg\\";
	/**
	 * HPC settings
	 */
//	static String destRList = "ti_isg/";

	static String tempFolder = "D:\\Data-Philip\\LeaveOneOutCrossValidation_temp\\";
	/**
	 * HPC settings
	 */
//	static String tempFolder = "LeaveOneOutCrossValidation_temp/";

	public static Map<String, Integer[]> mapElPairing = new HashMap<>();

	public static Map<Species, Integer> spinMultiplicity = new HashMap<>();

	public static Set<Species> validSpecies = new HashSet<Species>();

	public static Set<Species> invalidSpecies = new HashSet<Species>();

	public static Map<Reaction, Double> validReaction = new HashMap<Reaction, Double>();

	public static Map<Reaction, Double> invalidReaction = new HashMap<Reaction, Double>();

	public static Map<Species, Double> invalidSpeciesErrorBar = new HashMap<Species, Double>();

	/**
	 * 
	 * Number of runs.
	 * 
	 */
	
	static int[] ctrRuns = new int[] {1};

	/**
	 * 
	 * Number of reactions that will be generated for each species.
	 * 
	 */
	
	static int[] ctrRes = new int[] {5}; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10

	/**
	 * 
	 * Number of radicals.
	 * 
	 */
	
	static int[] ctrRadicals = new int[] {100}; // 0, 1, 2, 3, 4, 5

	public static void main(String[] args) throws Exception {

	LoadSpecies ls = new LoadSpecies();

	List<Species> refSpecies = ls.loadSpeciesProperties(ls.loadReferenceSpeciesFiles(srcRefPool), spinMultiplicity,srcCompoundsRef, mapElPairing);

	System.out.println("refSpecies.isEmpty() before solver (main method): " + refSpecies.isEmpty());
		
	LoadSolver lSolver = new LoadSolver();

	DataPreProcessing dpp = new DataPreProcessing();

	ErrorBarCalculation errorBarCalculation = new ErrorBarCalculation();

    System.out.println("- - - - - - - - - - - - - - - - Pre-processing step - - - - - - - - - - - - - - - -");

		/**
		 * 
		 * Data pre-processing step in cross validation algorithm. Determine the error
		 * metrics for reactions and species. Recommends a list of rejected species and the list of valid species.
		 * 
		 */
    
	dpp.getPreProcessingCorssValidation(1500, 20, destRList, ctrRadicals, ctrRuns, ctrRes, refSpecies,
				spinMultiplicity, lSolver.loadLPSolver(mapElPairing, 15000, tempFolder), validSpecies, invalidSpecies,
				validReaction, invalidReaction);

		/**
		 * 
		 * After completing pre-processing step, it prints valid reactions in txt file and on console.
		 * 
		 */
	
	BufferedWriter validReactionFile = new BufferedWriter(
				
	new FileWriter(destRList + "data-pre-processing" + "\\"+ "valid_reactions" + ".txt", true));
	/**
	 * HPC settings
	 */
//	new FileWriter(destRList + "data-pre-processing" + "/"+ "valid_reactions" + ".txt", true));

	System.out.println("Valid reactions writing . . . ");

	errorBarCalculation.generateInitialReactionListFile(validReactionFile, validReaction);

		/**
		 * 
		 * Printing invalid reactions in txt file and on console.
		 * 
		 */
	
	BufferedWriter invalidReactionFile = new BufferedWriter(
	
    new FileWriter(destRList + "data-pre-processing" + "\\"+ "invalid_reactions" + ".txt", true));
	/**
	 * HPC settings
	 */
//	new FileWriter(destRList + "data-pre-processing" + "/"+ "invalid_reactions" + ".txt", true));

	System.out.println("Invalid reactions writing . . .");

	errorBarCalculation.generateInitialReactionListFile(invalidReactionFile, invalidReaction);

		/**
		 * 
		 * Printing valid species in txt file and on console.
		 * 
		 */
	
	System.out.println("Valid species writing . . . ");

	BufferedWriter validSpeciesFile = new BufferedWriter(
	
	new FileWriter(destRList + "data-pre-processing" + "\\"+ "valid_species" + ".txt", true));
	
	/**
	 * HPC settings
	 */
//	new FileWriter(destRList + "data-pre-processing" + "/"+ "valid_species" + ".txt", true));

	errorBarCalculation.generateInitialValidSpeciesFile(validSpeciesFile, validSpecies);

		/**
		 * 
		 * Printing invalid species in txt file and on console.
		 * 
		 */
	
	System.out.println("Invalid species writing . . .");

	BufferedWriter invalidSpeciesFile = new BufferedWriter(
	
	new FileWriter(destRList + "data-pre-processing" + "\\"+ "invalid_species" + ".txt", true));
	/**
	 * HPC settings
	 */
//	new FileWriter(destRList + "data-pre-processing" + "/"+ "invalid_species" + ".txt", true));

	errorBarCalculation.generateInitialInvalidSpeciesFile(invalidSpeciesFile, invalidSpecies, validSpecies);

		/**
		 * 
		 * Error bar is average of all errors generated for each reaction of given
		 * species.
		 * 
		 * Calculates error bar for each species in invalid set of species.
		 * 
		 */

		invalidSpeciesErrorBar.putAll(errorBarCalculation.calculateSpeciesErrorBar(invalidReaction, validSpecies, invalidSpecies));

		/**
		 * 
		 * Sorted hash map in decreasing order comparing by error bar value in Java 1.8.
		 * 
		 */
		Map<Species, Double> sortedInvalidSpeciesErrorBar = Sort.sortingSpeciesMapComparingByValue(invalidSpeciesErrorBar);

		System.out.println("Sorted species compared by error bars:");

		for (Map.Entry<Species, Double> ss : sortedInvalidSpeciesErrorBar.entrySet()) {

		System.out.println(ss.getKey().getRef() + " " + ss.getValue());
		
		}

		System.out.println("- - - - - - - - - - - - - - - - Initial Analysis step - - - - - - - - - - - - - - - -");

		/**
		 * 
		 * The code below runs initial data analysis part of cross validation algorithm.
		 * 
		 * The code iterates over sorted rejected set of species and their error bars.
		 * For each species from the hash map the code below runs initial analysis of
		 * cross validation algorithm by using valid set of species as reference set
		 * and one species as a target species from invalid set of species. For each
		 * species from invalid set, the code tries to estimate lower error bar
		 * than current error bar for the selected target species (invalid species
		 * with maximum error bar). If calculated error bar is lower that the error bar of
		 * selected species in rejected list then the code adds that species into valid
		 * set of species. The code takes next species from invalid set of species with
		 * the maximum error bar and repeats the procedure above. 
		 * 
		 * Iteration stops (terminates) if there will be no rejected species that are added into valid set of species.  
		 * 
		 * If the cross validation algorithm can not find and generate a number of
		 * reactions for selected species, then that species will stay as a member of
		 * invalid set of species.
		 * 
		 * As a final result, the code below generates a set of rejected and set of
		 * accepted species.
		 * 
		 */
		
		/**
		 * 
		 * Determines whether a species is added into valid set of species
		 * 
		 */
		
		boolean addedSpeciesToValidSet=true;
		
		int  loop = 1;
		
		/**
		 * 
		 * Iteration that terminates when there will be not species added to the set of valid species.
		 * 
		 */
		
		while(addedSpeciesToValidSet) {
		
		int iteration = 1;
		
		/**
		 * 
		 * Set of species that are still member of invalid set of species after completed cross validation.
		 *  
		 */
		
		Set<Species> tempInvalidSetOfSpecies = new HashSet<Species>();
		
		Set<Species> tempValidSpecies = new HashSet<Species>();
		
		tempValidSpecies.addAll(validSpecies);
		
		int tempValidSpeciesSize = tempValidSpecies.size();
		
		/**
		 * 
		 * Iteration through a set of invalid species.
		 * 
		 */
		
		for (Map.Entry<Species, Double> errorMap : sortedInvalidSpeciesErrorBar.entrySet()) {
			
		System.out.println("Loop number: " + loop + " Iteration number: " + iteration+ " in Initial analysis of species: " + errorMap.getKey().getRef());
			    
			    /**
			     * 
			     * Skips species that are added into valid set of species.
			     * 
			     */
			    
		if(containsSpecies(validSpecies, errorMap.getKey())) {System.out.println("Species skipped: " + errorMap.getKey().getRef());continue;}
			     
		System.out.println("Iteration number: " + iteration);
				
		List<Species> targetSpecies = new ArrayList<Species>();

		LoadSpecies loadSpeciesInitialAnalysis = new LoadSpecies();

		Map<String, Integer[]> mapElPairing = new HashMap<>();

		Map<Species, Integer> spinMultiplicity = new HashMap<>();

		Set<Species> all = new HashSet<>();

		List<Species> srcRefPoolSpecies = new ArrayList<Species>();

		Collection<Species> invalids = new HashSet<>();

				/**
				 * 
				 * Creates reference set of species from valid set of species generated in
				 * pre-processing step. Also, adds a species from invalid set of species that is under current investigation  (cross validation) to a
				 * reference set of species.
				 * 				 
				 */
				
				srcRefPoolSpecies.addAll(ls.loadReferenceSpeciesForInitialAnalysis(srcRefPool, validSpecies));

				/**
				 * 
				 * Adding invalid species with current max error bar into source reference pool
				 * species list (srcRefPoolSpecies) i.e. set of reference species.
				 * 
				 */
				srcRefPoolSpecies.add(errorMap.getKey());

				System.out.println("Species reference list used in loop (" + loop+ ") and interation (" +iteration+ "):");

				for (Species s : srcRefPoolSpecies) {

					System.out.println(s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + s.getBondTypeMultiset());
				}

				System.out.println("Soi species name: " + errorMap.getKey().getRef() + " , " + errorMap.getKey().getHf() + " , " + errorMap.getKey().getTotalEnergy() + " , " 
				+ errorMap.getKey().getAtomMap() + " , " + errorMap.getKey().getBondTypeMultiset());

				double targetSpeciesEnthalpy = errorMap.getKey().getHf();
				
				double currentErrorBar= errorMap.getValue();
				
				System.out.println("targetSpeciesEnthalpy: " + targetSpeciesEnthalpy);
				
				System.out.println("After updated Hf: ");

				/**
				 * 
				 * Enthalpy of formation for target species is set to zero.
				 * 
				 */
				
				errorMap.getKey().setHf(0.0);
				
				targetSpecies.add(errorMap.getKey());

				System.out.println("Species of interest (target species): " + errorMap.getKey().getRef() + " , " + errorMap.getKey().getHf() + " , " + 
				errorMap.getKey().getTotalEnergy() + " , " 	+ errorMap.getKey().getAtomMap() + " , " + errorMap.getKey().getBondTypeMultiset());

				loadSpeciesInitialAnalysis.loadSpeciesPropertiesInitialAnalysis(srcRefPoolSpecies, targetSpecies, all,spinMultiplicity, srcCompoundsRef, mapElPairing, invalids);
				
				SolverHelper.add(mapElPairing);
				
				InitialDataAnalysis initialDataAnalysis = new InitialDataAnalysis();
				
				initialDataAnalysis.getInitialDataAnalysisCrossValidation(loop, addedSpeciesToValidSet,iteration, destRList, ctrRadicals, ctrRuns, ctrRes, srcRefPoolSpecies, 
						targetSpecies, srcCompoundsRef, spinMultiplicity, mapElPairing, tempFolder, invalids, all, validSpecies, targetSpeciesEnthalpy, currentErrorBar);
				
				iteration++;
			}
		
	System.out.println("Updated valid set of species after ("  + loop + ".) loop through the set of invalid species in initial analysis:");
	
	int num = 1;
	
	for(Species s: validSpecies) {
	
	System.out.println( num + ". species name: " + s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + " , " + s.getBondTypeMultiset());

	num++;
	
	}
	
	System.out.println("Invalid species file after (" + loop+ ".) loop through invalid species list with error bar . . .");

	BufferedWriter invalidSpeciesFileAfterInitialAnalysis = new BufferedWriter(new FileWriter(destRList + "initial-analysis" + "\\" + "loop_" + loop +"\\"+ "invalid_species_after_"+loop+"._loop" + ".txt", true));
	/**
	 * HPC settings
	 */
//	BufferedWriter invalidSpeciesFileAfterInitialAnalysis = new BufferedWriter(new FileWriter(destRList + "initial-analysis" + "/" + "loop_" + loop +"/"+ "invalid_species_after_"+loop+"._loop" + ".txt", true));
	
	errorBarCalculation.generateInvalidSpeciesFileAfterInitialAnalysis(loop, invalidSpeciesFileAfterInitialAnalysis, tempInvalidSetOfSpecies, sortedInvalidSpeciesErrorBar,invalidSpecies, validSpecies);
	
	/**
	 * 
	 * If there are not more invalid species that will be added to valid set of species then addedSpeciesToValidSet=false;
	 *  
	 */
	
	if(tempInvalidSetOfSpecies.isEmpty()) {
		
		addedSpeciesToValidSet=false;
	}
	
	/**
	 * 
	 * If there are not species added to the list of valid species then iteration process terminates.
	 * 
	 */
	
	if(addedSpeciesToValidSet) {	

		System.out.println("addedSpeciesToValidSet: " + addedSpeciesToValidSet);

	} else {
		
	System.out.println("After the (" + loop +".) loop through invalid set of species, there are not species from invalid set of species that are added into valid set of species.");
	
	}
	
	System.out.println("After (" + loop + ".) loop the set of invalid species is:");
	
	for(Species s: tempInvalidSetOfSpecies) {
	
		System.out.println(s.getRef() + " " + s.getHf());
	}
	
	loop++;

	/**
	 * 
	 * Size of species could be large. It means we need to change int -> BigInteger
	 * 
	 */
	
	int validSpeciesSize = validSpecies.size();
	
	/**
	 * 
	 * After passing through all invalid species, If there are not more invalid species that are added into valid set of species then the algorithms terminates.
	 *  
	 */
	
	if(tempValidSpeciesSize==validSpeciesSize) {
	
		System.out.println("Cross Validattion algorithm terminated (completed).");
		
		break;
	}
	
	};
	
		/**
		 * 
		 * Terminates program
		 * 
		 */
		System.exit(0);
	}
	
	/**
	 * 
	 * @param validSpecies the set of valid species.
	 * @param currentSpecies the current species.
	 * @return true if the species that is currently in use is the member of valid set of species.
	 *  
	 */
	public static boolean containsSpecies(Set<Species> validSpecies, Species currentSpecies) {
		
		boolean contains = false;
		
	     for(Species vs: validSpecies) {
	    	 
	    	 if(currentSpecies.getRef().equals(vs.getRef())) { contains=true; return contains;}
	     }
	     
	     return contains;
	}	
}