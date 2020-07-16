package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import uk.ac.ca.ceb.como.paper.enthalpy.data.analysis.InitialDataAnalysis;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HHDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing.DataPreProcessing;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSolver;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSpecies;
import uk.ac.cam.ceb.como.paper.enthalpy.reduction.list_calculator.ErrorBarCalculation;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;
import uk.ac.cam.ceb.paper.sort.Sort;

/**
 * This class contains the method that runs the Global Cross Validation
 * algorithm published in [1].
 * 
 * References:
 * 
 * [1] Philipp Buerger, First-Principles Investigation of Titanium
 * Dioxide Gas-Phase Precursor Chemistry, PhD thesis, St Edmund’s
 * College, February 7, 2017.
 * 
 * This code requires at least Java 1.8
 * This code can run from shell on HPC or local machine.
 * 
 * @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 * @author Angiras Menon (am2145@cam.ac.uk)
 * @author Feroz Farazi (msff2@cam.ac.uk)
 */

public class LeaveOneOutCrossValidationAlgorithm {
	public Map<String, Integer[]> mapElPairing = new LinkedHashMap<>();
	public Map<Species, Integer> spinMultiplicity = new LinkedHashMap<>();
	public LinkedHashSet<Species> validSpecies = new LinkedHashSet<Species>();
	public LinkedHashSet<Species> invalidSpecies = new LinkedHashSet<Species>();
	public Map<Reaction, Double> validReaction = new LinkedHashMap<Reaction, Double>();
	public Map<Reaction, Double> invalidReaction = new LinkedHashMap<Reaction, Double>();
	public Map<Species, Double> invalidSpeciesErrorBar = new LinkedHashMap<Species, Double>();
    BufferedWriter printedResultsTxtFile;
    Map<Species, Double> sortedInvalidSpeciesErrorBar;
    ErrorBarCalculation errorBarCalculation;
    long startTime;
    LoadSpecies ls;
    
    /**
     * Implement the Global Cross-Validation algorithm that consists of two</br>
     * two parts.</br>
     * - Data Preprocessing, and
     * - Initial Data Analysis.
     * 
     * @param srcCompoundsRef
     * @param srcRefPool
     * @param destRList
	 * @param ctrRuns Number of runs. In the calling method, define this as follows: int[] ctrRuns = new int[] {1};
	 * @param ctrRes Number of reactions that will be generated for each species. In the calling method, define this as follows: int[] ctrRes = new int[] {1}; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10  //5
	 * @param ctrRadicals Number of radicals. In the calling method, define this as follows: int[] ctrRadicals = new int[] {5}; // 0, 1, 2, 3, 4, 5 //100
	 * @param reactionType
     * @param tempFolder
     * @throws Exception
     */
    public void runGlobalCrossValidation(String srcCompoundsRef, String srcRefPool, String destRList, int[] ctrRuns, int[] ctrRes, int[] ctrRadicals, ReactionType reactionType, String tempFolder) throws Exception{
    	
    	/**
    	 * @author msff2@cam.ac.uk
    	 * Creates destination folder if it does not exist.
    	 */
    	if(!(new File(destRList).exists())){
    		
    	new File(destRList).mkdir();
    	new File(destRList + File.separator + "initial-analysis").mkdir();
    	
    	}
		
    	/**
    	 * @author msff2@cam.ac.uk
    	 * Creates temp folder if it does not exist.
    	 */
        if(!(new File(tempFolder).exists())){   		
  		new File(tempFolder).mkdir();
    	}

        System.out.println(destRList+"/" + "printed_results" + ".txt");	
		printedResultsTxtFile = new BufferedWriter(new FileWriter(destRList+"/" + "printed_results" + ".txt", true));
		startTime = System.currentTimeMillis();
		ls = new LoadSpecies();
    	runPreProcessing(srcCompoundsRef, srcRefPool, destRList, ctrRuns, ctrRes, ctrRadicals, reactionType, tempFolder);
    	runInitialDataAnalysis(srcCompoundsRef, srcRefPool, destRList, ctrRuns, ctrRes, ctrRadicals, reactionType, tempFolder);
    	printedResultsTxtFile.close();
    	System.out.println("Results file has been closed.");
    	
    	/**
         * @author NK510 (caresssd@hermes.cam.ac.uk)
         * Zipping folder which contains generated results (data) after running global cross validation algorithm. 
         */
        
        FolderUtils.getZipFile(destRList);
        
    	/**
    	 * @author NK510 (caresssd@hermes.cam.ac.uk)
    	 * Terminates the java process 'comoenthalpyestimationpaper.jar'
    	 */
    	System.exit(0);
    }
    
	/**
	 * This method performs the data preprocessing step of the Global</br> 
	 * Cross-Validation algorithm.
	 * 
	 * @param srcCompoundsRef
	 * @param srcRefPool
	 * @param destRList
	 * @param ctrRuns Number of runs. In the calling method, define this as follows: int[] ctrRuns = new int[] {1};
	 * @param ctrRes Number of reactions that will be generated for each species. In the calling method, define this as follows: int[] ctrRes = new int[] {1}; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10  //5
	 * @param ctrRadicals Number of radicals. In the calling method, define this as follows: int[] ctrRadicals = new int[] {5}; // 0, 1, 2, 3, 4, 5 //100
	 * @param reactionType
	 * @throws Exception
	 */
	private void runPreProcessing(String srcCompoundsRef, String srcRefPool, String destRList, int[] ctrRuns, int[] ctrRes, int[] ctrRadicals, ReactionType reactionType, String tempFolder) throws Exception {
		List<Species> refSpecies = ls.loadSpeciesProperties(ls.loadReferenceSpeciesFiles(srcRefPool), spinMultiplicity,srcCompoundsRef, mapElPairing, printedResultsTxtFile);
		System.out.println("refSpecies.isEmpty() before solver (main method): " + refSpecies.isEmpty());
		printedResultsTxtFile.write("refSpecies.isEmpty() before solver (main method): " + refSpecies.isEmpty());
		printedResultsTxtFile.write("\n");
		LoadSolver lSolver = new LoadSolver();
		DataPreProcessing dpp = new DataPreProcessing();
		errorBarCalculation = new ErrorBarCalculation();
	    System.out.println("- - - - - - - - - - - - - - - - Pre-processing step - - - - - - - - - - - - - - - -");
		printedResultsTxtFile.write("- - - - - - - - - - - - - - - - Pre-processing step - - - - - - - - - - - - - - - -");
		printedResultsTxtFile.write("\n");
		
		/**
		 * 
		 * Data pre-processing step in cross validation algorithm. Determine the error
		 * metrics for reactions and species. Recommends a list of rejected species and the list of valid species.
		 * 
		 */
		dpp.getPreProcessingCorssValidation(reactionType,1500, 20, destRList, ctrRadicals, ctrRuns, ctrRes, refSpecies,
					spinMultiplicity, lSolver.loadLPSolver(mapElPairing, 15000, tempFolder), validSpecies, invalidSpecies,
					validReaction, invalidReaction,printedResultsTxtFile);
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * HPC settings
		 * 
		 */
		BufferedWriter validReactionFile;
		validReactionFile = new BufferedWriter(new FileWriter(destRList + "/" +"data-pre-processing" + "/"+ "valid_reactions" + ".txt", true));
		System.out.println("Valid reactions writing . . . ");
		printedResultsTxtFile.write("Valid reactions writing . . . ");
		printedResultsTxtFile.write("\n");
		errorBarCalculation.generateInitialReactionListFile(validReactionFile, printedResultsTxtFile,validReaction);
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * HPC settings
		 * 
		 */
		BufferedWriter invalidReactionFile;
	    invalidReactionFile = new BufferedWriter(new FileWriter(destRList + "/" +"data-pre-processing" + "/"+ "invalid_reactions" + ".txt", true));
		System.out.println("Invalid reactions writing . . .");
		printedResultsTxtFile.write("Invalid reactions writing . . .");
		printedResultsTxtFile.write("\n");
		errorBarCalculation.generateInitialReactionListFile(invalidReactionFile, printedResultsTxtFile,
				invalidReaction);
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk) Printing valid species in
		 *         txt file and on console.
		 * 
		 */
		System.out.println("Valid species writing . . . ");
		printedResultsTxtFile.write("Valid species writing . . . ");
		printedResultsTxtFile.write("\n");
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * HPC settings
		 * 
		 */
		BufferedWriter validSpeciesFile;
		validSpeciesFile = new BufferedWriter(new FileWriter(destRList + "/"+"data-pre-processing" + "/"+ "valid_species" + ".txt", true));
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Saves initial valid species into json format. This initial valid species is generated in pre-processing step of cross validation algorithm.
		 *   
		 */
		BufferedWriter printedJsonFileInitialValidSpecies;
		printedJsonFileInitialValidSpecies = new BufferedWriter(new FileWriter(destRList+"/" +"data-pre-processing" + "/"+ "printed_initial_valid_species" +".json", true));
		errorBarCalculation.generateInitialValidSpeciesFile(validSpeciesFile, printedResultsTxtFile,printedJsonFileInitialValidSpecies,validSpecies);
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk) Printing invalid species in
		 *         txt file and on console.
		 * 
		 */
		System.out.println("Invalid species writing . . .");
		printedResultsTxtFile.write("Invalid species writing . . .");
		printedResultsTxtFile.write("\n");
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Settings on PC machine.
		 * 
		 */
		BufferedWriter invalidSpeciesFile;
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * HPC settings
		 * 
		 */
		invalidSpeciesFile = new BufferedWriter(new FileWriter(destRList+"/" + "data-pre-processing" + "/"+ "invalid_species" + ".txt", true));
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Saves initial invalid species into json format. This initial invalid species is generated in pre-processing step of cross validation algorithm.
		 *   
		 */
		BufferedWriter printedJsonFileInitialInvalidSpecies;
		printedJsonFileInitialInvalidSpecies = new BufferedWriter(new FileWriter(destRList+"/" +"data-pre-processing" + "/"+ "printed_initial_invalid_species" +".json", true));
		errorBarCalculation.generateInitialInvalidSpeciesFile(invalidSpeciesFile, printedResultsTxtFile, printedJsonFileInitialInvalidSpecies,invalidSpecies, validSpecies);
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Error bar is average of all errors generated for each reaction of given
		 * species.
		 * 
		 * Calculates error bar for each species in invalid set of species.
		 * 
		 */
		invalidSpeciesErrorBar.putAll(errorBarCalculation.calculateSpeciesErrorBar(invalidReaction, validSpecies, invalidSpecies,printedResultsTxtFile));

		/**
		 * @author nk510 (caresssd@hermes.cam.ac.uk) Sorted hash map in
		 *         decreasing order comparing by error bar value in Java 1.8.
		 * 
		 */
		sortedInvalidSpeciesErrorBar = Sort.sortingSpeciesMapComparingByValue(invalidSpeciesErrorBar);
		System.out.println("Sorted species compared by error bars:");
		printedResultsTxtFile.write("Sorted species compared by error bars:");
		printedResultsTxtFile.write("\n");
		for (Map.Entry<Species, Double> ss : sortedInvalidSpeciesErrorBar.entrySet()) {
			System.out.println(ss.getKey().getRef() + " " + ss.getValue());
			printedResultsTxtFile.write(ss.getKey().getRef() + " " + ss.getValue());
			printedResultsTxtFile.write("\n");
		}
	}

	/**
	 * 
	 * The code below runs initial data analysis part of cross validation algorithm.
	 * 
	 * This method performs the initial data analysis step of the Global</br> 
	 * Cross-Validation algorithm.
	 * 
	 * The code iterates over sorted invalid set of species and their error bars.
	 * For each species from the list of invalid species the code below runs initial analysis of
	 * cross validation algorithm.  It is using valid set of species as reference set
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
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param srcCompoundsRef
	 * @param srcRefPool
	 * @param destRList
	 * @param ctrRuns Number of runs. In the calling method, define this as follows: int[] ctrRuns = new int[] {1};
	 * @param ctrRes Number of reactions that will be generated for each species. In the calling method, define this as follows: int[] ctrRes = new int[] {1}; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10  //5
	 * @param ctrRadicals Number of radicals. In the calling method, define this as follows: int[] ctrRadicals = new int[] {5}; // 0, 1, 2, 3, 4, 5 //100
	 * @param reactionType
	 * @throws Exception
	 */
	private void runInitialDataAnalysis(String srcCompoundsRef, String srcRefPool, String destRList, int[] ctrRuns, int[] ctrRes, int[] ctrRadicals, ReactionType reactionType, String tempFolder) throws Exception{
		System.out.println("- - - - - - - - - - - - - - - - Initial Analysis step - - - - - - - - - - - - - - - -");
		printedResultsTxtFile.write("- - - - - - - - - - - - - - - - Initial Analysis step - - - - - - - - - - - - - - - -");
		printedResultsTxtFile.write("\n");
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Determines whether a species is added into valid set of species
		 * 
		 */
		boolean addedSpeciesToValidSet=true;
		int  loop = 1;
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Iteration that terminates when there will be no more species added into the set of valid species.
		 * 
		 */
		while(addedSpeciesToValidSet) {
		int iteration = 1;
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Set of species that are still member of invalid set of species after completed cross validation algorithm.
		 *  
		 */
		LinkedHashSet<Species> tempInvalidSetOfSpecies = new LinkedHashSet<Species>();
		LinkedHashSet<Species> tempValidSpecies = new LinkedHashSet<Species>();
		tempValidSpecies.addAll(validSpecies);
		int tempValidSpeciesSize = tempValidSpecies.size();
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Iteration through a set of invalid species.
		 * 
		 */
		for (Map.Entry<Species, Double> errorMap : sortedInvalidSpeciesErrorBar.entrySet()) {
		System.out.println("Loop number: " + loop + " Iteration number: " + iteration+ " in Initial analysis of species: " + errorMap.getKey().getRef());
		printedResultsTxtFile.write("Loop number: " + loop + " Iteration number: " + iteration+ " in Initial analysis of species: " + errorMap.getKey().getRef());
		printedResultsTxtFile.write("\n");		
			    /**
			     * 
			     * @author nk510 (caresssd@hermes.cam.ac.uk)
			     * Skips species from invalid set of species that are already added into valid set of species.
			     * 
			     */
			    
		if(containsSpecies(validSpecies, errorMap.getKey())) {
			
			System.out.println("Species skipped: " + errorMap.getKey().getRef());
			
			printedResultsTxtFile.write("Species skipped: " + errorMap.getKey().getRef());
			printedResultsTxtFile.write("\n");
			
			continue;
			
			
			
		}
			     
		System.out.println("Iteration number: " + iteration);
		
		printedResultsTxtFile.write("Iteration number: " + iteration);
		printedResultsTxtFile.write("\n");
				
		List<Species> targetSpecies = new ArrayList<Species>();

		LoadSpecies loadSpeciesInitialAnalysis = new LoadSpecies();

		Map<String, Integer[]> mapElPairing = new HashMap<>();

		Map<Species, Integer> spinMultiplicity = new HashMap<>();

		Set<Species> all = new HashSet<>();

		List<Species> srcRefPoolSpecies = new ArrayList<Species>();

		Collection<Species> invalids = new HashSet<>();

				/**
				 * 
				 * @author nk510 (caresssd@hermes.cam.ac.uk)
				 * Creates reference set of species from valid set of species generated in
				 * pre-processing step. Also, adds a species from invalid set of species that is under current investigation  (cross validation) to a
				 * reference set of species.
				 * 				 
				 */
				
				srcRefPoolSpecies.addAll(ls.loadReferenceSpeciesForInitialAnalysis(srcRefPool, validSpecies));

				/**
				 * 
				 * @author nk510 (caresssd@hermes.cam.ac.uk)
				 * Adding invalid species with current max error bar into source reference pool
				 * species list (srcRefPoolSpecies) i.e. set of reference species.
				 * 
				 */
				srcRefPoolSpecies.add(errorMap.getKey());

				System.out.println("Species reference list used in loop (" + loop+ ") and interation (" +iteration+ "):");
				
				printedResultsTxtFile.write("Species reference list used in loop (" + loop+ ") and interation (" +iteration+ "):");
				printedResultsTxtFile.write("\n");

				for (Species s : srcRefPoolSpecies) {

					System.out.println(s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + s.getBondTypeMultiset());
					
					printedResultsTxtFile.write(s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + s.getBondTypeMultiset());
					printedResultsTxtFile.write("\n");
					
				}

				System.out.println("Soi species name: " + errorMap.getKey().getRef() + " , " + errorMap.getKey().getHf() + " , " + errorMap.getKey().getTotalEnergy() + " , " 
				+ errorMap.getKey().getAtomMap() + " , " + errorMap.getKey().getBondTypeMultiset());
				
				printedResultsTxtFile.write("Soi species name: " + errorMap.getKey().getRef() + " , " + errorMap.getKey().getHf() + " , " + errorMap.getKey().getTotalEnergy() + " , " 
						+ errorMap.getKey().getAtomMap() + " , " + errorMap.getKey().getBondTypeMultiset());
				
				printedResultsTxtFile.write("\n");

				double targetSpeciesEnthalpy = errorMap.getKey().getHf();
				
				double currentErrorBar= errorMap.getValue();
				
				System.out.println("targetSpeciesEnthalpy: " + targetSpeciesEnthalpy);
				
				printedResultsTxtFile.write("targetSpeciesEnthalpy: " + targetSpeciesEnthalpy);
				printedResultsTxtFile.write("\n");
				
				System.out.println("After updated Hf: ");
				
				printedResultsTxtFile.write("After updated Hf: ");
				printedResultsTxtFile.write("\n");

				/**
				 * 
				 * @author nk510 (caresssd@hermes.cam.ac.uk)
				 * Enthalpy of formation for target species is set to zero.
				 * 
				 */
				
				errorMap.getKey().setHf(0.0);
				
				targetSpecies.add(errorMap.getKey());

				System.out.println("Species of interest (target species): " + errorMap.getKey().getRef() + " , " + errorMap.getKey().getHf() + " , " + 
				errorMap.getKey().getTotalEnergy() + " , " 	+ errorMap.getKey().getAtomMap() + " , " + errorMap.getKey().getBondTypeMultiset());
				
				printedResultsTxtFile.write("Species of interest (target species): " + errorMap.getKey().getRef() + " , " + errorMap.getKey().getHf() + " , " + 
						errorMap.getKey().getTotalEnergy() + " , " 	+ errorMap.getKey().getAtomMap() + " , " + errorMap.getKey().getBondTypeMultiset());
				
				printedResultsTxtFile.write("\n");
				

				loadSpeciesInitialAnalysis.loadSpeciesPropertiesInitialAnalysis(srcRefPoolSpecies, targetSpecies, all,spinMultiplicity, srcCompoundsRef, mapElPairing, invalids,  printedResultsTxtFile);
				
				SolverHelper.add(mapElPairing);
				
				InitialDataAnalysis initialDataAnalysis = new InitialDataAnalysis();
				
				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * 
				 */
//				ISGReactionType isgReactionTypeInitialAnalysis = new ISGReactionType(true); //remove comment on this line of the code if it is necessary to add type of reaction as argument in method  'initialDataAnalysis.getInitialDataAnalysisCrossValidation'
				
				initialDataAnalysis.getInitialDataAnalysisCrossValidation(loop, addedSpeciesToValidSet,iteration, destRList, ctrRadicals, ctrRuns, ctrRes, srcRefPoolSpecies, 
						targetSpecies, srcCompoundsRef, spinMultiplicity, mapElPairing, tempFolder, invalids, all, validSpecies, targetSpeciesEnthalpy, currentErrorBar,printedResultsTxtFile, reactionType);
				
				iteration++;
			}
		
	System.out.println("Updated valid set of species after ("  + loop + ".) loop through the set of invalid species in initial analysis:");
	
	printedResultsTxtFile.write("Updated valid set of species after ("  + loop + ".) loop through the set of invalid species in initial analysis:");
	
	printedResultsTxtFile.write("\n");
	
	int num = 1;
	
	/**
	 * Stores valid set of species after each loop in initial analysis.
	 */
	JSONArray validSpeciesJsonList = new JSONArray();
	
//	BufferedWriter printedJsonFileValidSpecies = new BufferedWriter(new FileWriter(destRList+"/" + "initial-analysis" + "/" + "loop_" + loop +"/"+ "printed_valid_species_loop_"+loop +".json", true));
	
	/**
	 * Line below is added by msff2@cam.ac.uk
	 */
	new File(destRList+File.separator + "initial-analysis"+ File.separator + "loop_" + loop).mkdir();
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * Line below is settings that works on PC (Windows) machine
	 *  
	 */
	
	BufferedWriter printedJsonFileValidSpecies = new BufferedWriter(new FileWriter(destRList + "/"+"initial-analysis" + "/" + "loop_" + loop +"/"+ "printed_valid_species_loop_"+loop +".json", true));

	for(Species s: validSpecies) {

    JSONObject jsonValidSpecies = new JSONObject();
    
    JSONObject jsonAllValidSpecies = new JSONObject();
    
	System.out.println( num + ". species name: " + s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + " , " + s.getBondTypeMultiset());

	printedResultsTxtFile.write( num + ". species name: " + s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + " , " + s.getBondTypeMultiset());
	
	printedResultsTxtFile.write("\n");
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * Store information about valid species in JSON file.
	 */
	jsonValidSpecies.put("speciesName",s.getRef().toString());
	
	jsonValidSpecies.put("validity","true");
	
	jsonAllValidSpecies.put("species", jsonValidSpecies);
	
	validSpeciesJsonList.add(jsonAllValidSpecies);
	
	num++;
	
	}
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * Save valid species list as JSON file.
	 */
	printedJsonFileValidSpecies.write(validSpeciesJsonList.toJSONString());
	printedJsonFileValidSpecies.close();
	
	System.out.println("Invalid species file after (" + loop+ ".) loop through invalid species list with error bar . . .");
	
	printedResultsTxtFile.write("Invalid species file after (" + loop+ ".) loop through invalid species list with error bar . . .");
	printedResultsTxtFile.write("\n");
	

	/**
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Settings for PC machine.
	 */
	BufferedWriter invalidSpeciesFileAfterInitialAnalysis = new BufferedWriter(new FileWriter(destRList +"/"+"initial-analysis" + "/" + "loop_" + loop +"/"+ "invalid_species_after_"+loop+"._loop" + ".txt", true));
	
	/**
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 *  Txt file that saves invalid species in each loop of initial analysis step of cross validation algorithm.
	 */
	
//	BufferedWriter invalidSpeciesFileAfterInitialAnalysis = new BufferedWriter(new FileWriter(destRList +"/" + "initial-analysis" + "/" + "loop_" + loop +"/"+ "invalid_species_after_"+loop+"._loop" + ".txt", true));
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * Json file that saves invalid species in each loop of initial analysis step of cross validation algorithm.
	 * 
	 */
//	BufferedWriter printedJsonFileInvalidSpeciesInitialAnalysis = new BufferedWriter(new FileWriter(destRList+"/" + "initial-analysis" + "/" + "loop_" + loop +"/"+ "printed_invalid_species_loop_"+loop +".json", true));
	
	
	
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * Line below is settings that works on PC (Windows) machine
	 *  
	 */
	
	BufferedWriter printedJsonFileInvalidSpeciesInitialAnalysis = new BufferedWriter(new FileWriter(destRList + "/"+"initial-analysis" + "/" + "loop_" + loop +"/"+ "printed_invalid_species_loop_"+loop +".json", true));
	
	errorBarCalculation.generateInvalidSpeciesFileAfterInitialAnalysis(loop, invalidSpeciesFileAfterInitialAnalysis, printedJsonFileInvalidSpeciesInitialAnalysis, tempInvalidSetOfSpecies, sortedInvalidSpeciesErrorBar,invalidSpecies, validSpecies,printedResultsTxtFile);
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * If there are not more invalid species that will be added to valid set of species then addedSpeciesToValidSet=false;
	 *  
	 */
	
	if(tempInvalidSetOfSpecies.isEmpty()) {
		
		addedSpeciesToValidSet=false;
	}
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * If there are not species added to the list of valid species then iteration process terminates.
	 * 
	 */
	
	if(addedSpeciesToValidSet) {	

	System.out.println("addedSpeciesToValidSet: " + addedSpeciesToValidSet);
		
	printedResultsTxtFile.write("addedSpeciesToValidSet: " + addedSpeciesToValidSet);
		
	printedResultsTxtFile.write("\n");

	} else {
		
	System.out.println("After the (" + loop +".) loop through invalid set of species, there are not species from invalid set of species that are added into valid set of species.");
	
	printedResultsTxtFile.write("After the (" + loop +".) loop through invalid set of species, there are not species from invalid set of species that are added into valid set of species.");
	
	printedResultsTxtFile.write("\n");
	
	}
	
	System.out.println("After (" + loop + ".) loop the set of invalid species is:");
	
	printedResultsTxtFile.write("After (" + loop + ".) loop the set of invalid species is:");
	printedResultsTxtFile.write("\n");
	
	
	for(Species s: tempInvalidSetOfSpecies) {
	
		System.out.println(s.getRef() + " " + s.getHf());
		
		printedResultsTxtFile.write(s.getRef() + " " + s.getHf());
		
		printedResultsTxtFile.write("\n");
		
	}
	
	
	loop++;

	/**
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Size of species could be large. It means we need to change int -> BigInteger
	 * 
	 */
	
	int validSpeciesSize = validSpecies.size();
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * After passing through all invalid species, If there are not more invalid species that are added into valid set of species then the algorithms terminates.
	 *  
	 */
	
	if(tempValidSpeciesSize==validSpeciesSize) {

		NumberFormat formatter = new DecimalFormat("#00.000");
		
		final long endTime = System.currentTimeMillis();

		String runningTime = formatter.format((endTime - startTime) / 1000d) + " seconds";
		
		System.out.println("Cross Validattion algorithm terminated (completed) in " + runningTime);
		
		printedResultsTxtFile.write("\n");
		
		break;
	}
	
	};
	}
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * @param validSpecies the set of valid species.
	 * @param currentSpecies the current species.
	 * @return true if the species that is currently in use is the member of valid set of species.
	 *  
	 */
	
	public static boolean containsSpecies(LinkedHashSet<Species> validSpecies, Species currentSpecies) {
		
		boolean contains = false;
		
	     for(Species vs: validSpecies) {
	    	 
	    	 if(currentSpecies.getRef().equals(vs.getRef())) { contains=true; return contains;}
	     }
	     
	     return contains;
	}	
}