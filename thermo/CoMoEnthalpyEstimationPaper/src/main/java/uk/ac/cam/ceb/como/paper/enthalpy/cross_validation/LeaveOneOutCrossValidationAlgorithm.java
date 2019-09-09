package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.Collection;
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

public class LeaveOneOutCrossValidationAlgorithm {

	public static Collection<Species> invalids = new HashSet<Species>();

	public static Map<String, Integer[]> mapElPairing = new HashMap<>();

	public static Map<Species, Integer> spinMultiplicity = new HashMap<>();

	public static Set<Species> validSpecies = new HashSet<Species>();

	public static Set<Species> invalidSpecies = new HashSet<Species>();

	public static Map<Reaction, Double> validReaction = new HashMap<Reaction, Double>();

	public static Map<Reaction, Double> invalidReaction = new HashMap<Reaction, Double>();

	static int[] ctrRuns = new int[] { 1 };

	static int[] ctrRes = new int[] { 5 }; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10

	static int[] ctrRadicals = new int[] { 100 }; // 0, 1, 2, 3, 4, 5

	public static void main(String[] args) throws Exception {

		LoadSpecies ls = new LoadSpecies();

		List<Species> refSpecies = ls.loadSpeciesProperties(
				ls.loadReferenceSpeciesFiles(
						"C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv"),
				spinMultiplicity, "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\\", mapElPairing, invalids);

		LoadSolver lSolver = new LoadSolver();

		DataPreProcessing dpp = new DataPreProcessing();

		dpp.getPreProcessingCorssValidation(1500, 20, "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg\\",
				ctrRadicals, ctrRuns, ctrRes, refSpecies, spinMultiplicity,
				lSolver.loadLPSolver(mapElPairing, 15000, "D:\\Data-Philip\\LeaveOneOutCrossValidation_temp\\"),
				validSpecies, invalidSpecies, validReaction, invalidReaction);
				
		BufferedWriter validReactionFile = new BufferedWriter(new FileWriter("C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg" + "\\" + "valid_reactions" + ".txt", true));
		
		ErrorBarCalculation errorBarCalculation = new ErrorBarCalculation();
		
		System.out.println("Valid reactions writing . . . ");
		
		errorBarCalculation.generateInitialReactionListFile(validReactionFile, validReaction);
		
		BufferedWriter invalidReactionFile = new BufferedWriter(new FileWriter("C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg" + "\\" + "invalid_reactions" + ".txt", true));
		
		System.out.println("Invalid reactions writing . . .");
		
		errorBarCalculation.generateInitialReactionListFile(invalidReactionFile, invalidReaction);
		
		System.out.println("Valid species writing . . . ");
		
		BufferedWriter validSpeciesFile = new BufferedWriter(new FileWriter("C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg" + "\\" + "valid_species" + ".txt", true));
		
		errorBarCalculation.generateInitialValidSpeciesFile(validSpeciesFile, validSpecies);
		
		System.out.println("Invalid species writing . . .");
		
		BufferedWriter invalidSpeciesFile = new BufferedWriter(new FileWriter("C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg" + "\\" + "invalid_species" + ".txt", true));

		errorBarCalculation.generateInitialInvalidSpeciesFile(invalidSpeciesFile, invalidSpecies,validSpecies);
		
	}

}
