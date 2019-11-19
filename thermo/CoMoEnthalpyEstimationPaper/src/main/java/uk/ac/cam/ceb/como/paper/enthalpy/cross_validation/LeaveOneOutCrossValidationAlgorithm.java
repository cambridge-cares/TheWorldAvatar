package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

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
 *         This code is running (start) from shell on HPC or local machine.
 *
 */

public class LeaveOneOutCrossValidationAlgorithm {	
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Local Windows machine settings.
	 * 
	 */
//	static String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\\";
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	/**
	 * 
	 * Ti-based reference species
	 * 
	 */
	static String srcCompoundsRef = "g09/";
	
	/**
	 * 
	 * HCO-based reference species
	 *  
	 */
//	static String srcCompoundsRef = "esc/g09/";
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Local Windows machine settings.
	 * 
	 */

//	static String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv";
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Ti-based target species
	 * 
	 */
	static String srcRefPool = "csv/ref_scaled_kJperMols_v8.csv";

	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HCO-based target species
	 * 
	 */
	
//	static String srcRefPool = "csv/ref-enthalpy_scaled_kJperMol.csv";
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Local Windows machine settings.
	 * 
	 */
//	static String destRList = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg\\";
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	
	static String destRList = "ti_isg/";
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Local Windows machine settings.
	 * 
	 */
//	static String tempFolder = "D:\\Data-Philip\\LeaveOneOutCrossValidation_temp\\";
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	static String tempFolder = "LeaveOneOutCrossValidation_temp/";

	public static Map<String, Integer[]> mapElPairing = new HashMap<>();

	public static Map<Species, Integer> spinMultiplicity = new HashMap<>();

	public static LinkedHashSet<Species> validSpecies = new LinkedHashSet<Species>();

	public static LinkedHashSet<Species> invalidSpecies = new LinkedHashSet<Species>();

	public static Map<Reaction, Double> validReaction = new HashMap<Reaction, Double>();

	public static Map<Reaction, Double> invalidReaction = new HashMap<Reaction, Double>();

	public static Map<Species, Double> invalidSpeciesErrorBar = new HashMap<Species, Double>();

	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Number of runs.
	 * 
	 */
	
	static int[] ctrRuns = new int[] {1};

	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Number of reactions that will be generated for each species.
	 * 
	 */
	
	static int[] ctrRes = new int[] {5}; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10

	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * Number of radicals.
	 * 
	 */
	
	static int[] ctrRadicals = new int[] {100}; // 0, 1, 2, 3, 4, 5

	public static void main(String[] args) throws Exception {
	
	/**
	 * Folder that contains Gaussian files.
	 */
//	String srcCompoundsRef = args[0]; //"g09/";
	
	/**
	 * File that contains information about target species.
	 */
//	String srcRefPool = args[1]; //"csv/ref_scaled_kJperMols_v8.csv";
	
	/**
	 * Folder that stores all results of cross validation calculations: chemical reactions, enthalpy, valid species, invalid species, etc..
	 */
//	String destRList = args[2]; //"ti_isg/";
	
	/**
	 * Path to .temp folder that stores all  files used by GLPK solver.
	 */
//	String tempFolder = args[3]; //"LeaveOneOutCrossValidation_temp/"
	
    JSONArray listOfJsonSpeciesData = new JSONArray();
    
    JSONObject speciesJsonObject = new JSONObject();
    
    
    
	BufferedWriter printedResultsTxtFile = new BufferedWriter(new FileWriter(destRList+"/" + "printed_results" + ".txt", true));
	
	    /**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * The start current time in milliseconds.
		 *  
		 * */
	
    long startTime = System.currentTimeMillis();

	LoadSpecies ls = new LoadSpecies();

	List<Species> refSpecies = ls.loadSpeciesProperties(ls.loadReferenceSpeciesFiles(srcRefPool), spinMultiplicity,srcCompoundsRef, mapElPairing, printedResultsTxtFile);

	System.out.println("refSpecies.isEmpty() before solver (main method): " + refSpecies.isEmpty());
	
	printedResultsTxtFile.write("refSpecies.isEmpty() before solver (main method): " + refSpecies.isEmpty());
	
	printedResultsTxtFile.write("\n");
	
	LoadSolver lSolver = new LoadSolver();

	DataPreProcessing dpp = new DataPreProcessing();

	ErrorBarCalculation errorBarCalculation = new ErrorBarCalculation();

    System.out.println("- - - - - - - - - - - - - - - - Pre-processing step - - - - - - - - - - - - - - - -");
    
	printedResultsTxtFile.write("- - - - - - - - - - - - - - - - Pre-processing step - - - - - - - - - - - - - - - -");
	
	printedResultsTxtFile.write("\n");
	
		/**
		 * 
		 * Data pre-processing step in cross validation algorithm. Determine the error
		 * metrics for reactions and species. Recommends a list of rejected species and the list of valid species.
		 * 
		 */
    
	dpp.getPreProcessingCorssValidation(1500, 20, destRList, ctrRadicals, ctrRuns, ctrRes, refSpecies,
				spinMultiplicity, lSolver.loadLPSolver(mapElPairing, 15000, tempFolder), validSpecies, invalidSpecies,
				validReaction, invalidReaction,printedResultsTxtFile);

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * After completing pre-processing step, it prints valid reactions in txt file and on console.
		 * 
		 */
	
//	BufferedWriter validReactionFile = new BufferedWriter(new FileWriter(destRList + "data-pre-processing" + "\\"+ "valid_reactions" + ".txt", true));

	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	BufferedWriter validReactionFile = new BufferedWriter(new FileWriter(destRList + "/" +"data-pre-processing" + "/"+ "valid_reactions" + ".txt", true));

	System.out.println("Valid reactions writing . . . ");
	
	printedResultsTxtFile.write("Valid reactions writing . . . ");
	printedResultsTxtFile.write("\n");
	

	errorBarCalculation.generateInitialReactionListFile(validReactionFile, printedResultsTxtFile,validReaction);

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Printing invalid reactions in txt file and on console.
		 * 
		 */
	
	BufferedWriter invalidReactionFile = new BufferedWriter(
	
//  new FileWriter(destRList + "data-pre-processing" + "\\"+ "invalid_reactions" + ".txt", true));
    
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
			
	new FileWriter(destRList + "/" +"data-pre-processing" + "/"+ "invalid_reactions" + ".txt", true));

	System.out.println("Invalid reactions writing . . .");
	
	printedResultsTxtFile.write("Invalid reactions writing . . .");
	printedResultsTxtFile.write("\n");
	
	errorBarCalculation.generateInitialReactionListFile(invalidReactionFile, printedResultsTxtFile,invalidReaction);

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Printing valid species in txt file and on console.
		 * 
		 */
	
	System.out.println("Valid species writing . . . ");
	
	printedResultsTxtFile.write("Valid species writing . . . ");
	printedResultsTxtFile.write("\n");
	

	BufferedWriter validSpeciesFile = new BufferedWriter(
	
//	new FileWriter(destRList + "data-pre-processing" + "\\"+ "valid_species" + ".txt", true));
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	new FileWriter(destRList + "/"+"data-pre-processing" + "/"+ "valid_species" + ".txt", true));

	errorBarCalculation.generateInitialValidSpeciesFile(validSpeciesFile, printedResultsTxtFile,validSpecies);

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Printing invalid species in txt file and on console.
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
//	BufferedWriter invalidSpeciesFile = new BufferedWriter(new FileWriter(destRList + "data-pre-processing" + "\\"+ "invalid_species" + ".txt", true));
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	
	BufferedWriter invalidSpeciesFile = new BufferedWriter(new FileWriter(destRList+"/" + "data-pre-processing" + "/"+ "invalid_species" + ".txt", true));

	errorBarCalculation.generateInitialInvalidSpeciesFile(invalidSpeciesFile, printedResultsTxtFile, invalidSpecies, validSpecies);

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
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Sorted hash map in decreasing order comparing by error bar value in Java 1.8.
		 * 
		 */
		
		Map<Species, Double> sortedInvalidSpeciesErrorBar = Sort.sortingSpeciesMapComparingByValue(invalidSpeciesErrorBar);

		System.out.println("Sorted species compared by error bars:");
		
		printedResultsTxtFile.write("Sorted species compared by error bars:");
		printedResultsTxtFile.write("\n");

		for (Map.Entry<Species, Double> ss : sortedInvalidSpeciesErrorBar.entrySet()) {

		System.out.println(ss.getKey().getRef() + " " +  ss.getValue());

		printedResultsTxtFile.write(ss.getKey().getRef() + " " +  ss.getValue());
		printedResultsTxtFile.write("\n");

		
		}

		System.out.println("- - - - - - - - - - - - - - - - Initial Analysis step - - - - - - - - - - - - - - - -");
		
		printedResultsTxtFile.write("- - - - - - - - - - - - - - - - Initial Analysis step - - - - - - - - - - - - - - - -");
		printedResultsTxtFile.write("\n");

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * The code below runs initial data analysis part of cross validation algorithm.
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
		 */
		
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
		 * Iteration that terminates when there will be not species added to the set of valid species.
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
				
				initialDataAnalysis.getInitialDataAnalysisCrossValidation(loop, addedSpeciesToValidSet,iteration, destRList, ctrRadicals, ctrRuns, ctrRes, srcRefPoolSpecies, 
						targetSpecies, srcCompoundsRef, spinMultiplicity, mapElPairing, tempFolder, invalids, all, validSpecies, targetSpeciesEnthalpy, currentErrorBar,printedResultsTxtFile);
				
				iteration++;
			}
		
	System.out.println("Updated valid set of species after ("  + loop + ".) loop through the set of invalid species in initial analysis:");
	
	printedResultsTxtFile.write("Updated valid set of species after ("  + loop + ".) loop through the set of invalid species in initial analysis:");
	
	printedResultsTxtFile.write("\n");
	
	int num = 1;
	
	JSONArray validSpeciesJsonList = new JSONArray();
	
	BufferedWriter printedJsonFileValidSpecies = new BufferedWriter(new FileWriter(destRList+"/" + "printed_valid_species_loop_"+loop +".json", true));

	for(Species s: validSpecies) {

    JSONObject jsonValidSpecies = new JSONObject();
    JSONObject jsonAllValidSpecies = new JSONObject();
    
	System.out.println( num + ". species name: " + s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + " , " + s.getBondTypeMultiset());

	printedResultsTxtFile.write( num + ". species name: " + s.getRef() + " , " + s.getHf() + " , " + s.getTotalEnergy() + " , " + s.getAtomMap() + " , " + s.getBondTypeMultiset());
	
	printedResultsTxtFile.write("\n");
	
	/**
	 * Store information about valid species in JSON file.
	 */
	jsonValidSpecies.put("speciesName",s.getRef().toString());
	
	jsonValidSpecies.put("validity","true");
	
	jsonAllValidSpecies.put("species", jsonValidSpecies);
	
	validSpeciesJsonList.add(jsonAllValidSpecies);
	
	num++;
	
	}
	
	/**
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
//	BufferedWriter invalidSpeciesFileAfterInitialAnalysis = new BufferedWriter(new FileWriter(destRList + "initial-analysis" + "\\" + "loop_" + loop +"\\"+ "invalid_species_after_"+loop+"._loop" + ".txt", true));
	
	/**
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * HPC settings
	 * 
	 */
	BufferedWriter invalidSpeciesFileAfterInitialAnalysis = new BufferedWriter(new FileWriter(destRList +"/" + "initial-analysis" + "/" + "loop_" + loop +"/"+ "invalid_species_after_"+loop+"._loop" + ".txt", true));
	
	errorBarCalculation.generateInvalidSpeciesFileAfterInitialAnalysis(loop, invalidSpeciesFileAfterInitialAnalysis, tempInvalidSetOfSpecies, sortedInvalidSpeciesErrorBar,invalidSpecies, validSpecies,printedResultsTxtFile);
	
	/**
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
	
	
    JSONArray invalidSpeciesJsonList = new JSONArray();
	
	BufferedWriter printedJsonFileInvalidSpecies = new BufferedWriter(new FileWriter(destRList+"/" + "printed_invalid_species_loop_"+loop +".json", true));
	
	for(Species s: tempInvalidSetOfSpecies) {
	
		System.out.println(s.getRef() + " " + s.getHf());
		
		printedResultsTxtFile.write(s.getRef() + " " + s.getHf());
		
		printedResultsTxtFile.write("\n");
		
		JSONObject jsonInvalidSpecies = new JSONObject();
	    JSONObject jsonAllInvalidSpecies = new JSONObject();
	    
	    
	    /**
		 * Store information about invalid species in JSON file.
		 */
		jsonInvalidSpecies.put("speciesName",s.getRef().toString());
		
		jsonInvalidSpecies.put("validity","false");
		
		jsonAllInvalidSpecies.put("species", jsonInvalidSpecies);
		
		invalidSpeciesJsonList.add(jsonAllInvalidSpecies);
		
	}
	
	
	/**
	 * Save invalid species list as JSON file.
	 */
	printedJsonFileInvalidSpecies.write(invalidSpeciesJsonList.toJSONString());
	printedJsonFileInvalidSpecies.close();
	
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
		
		printedResultsTxtFile.write("Cross Validattion algorithm terminated (completed) in " + runningTime);
		
		printedResultsTxtFile.write("\n");
		
		break;
	}
	
	};
	
	printedResultsTxtFile.close();
	
		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * Terminates program
		 * 
		 */
	
	     System.exit(0);
	
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