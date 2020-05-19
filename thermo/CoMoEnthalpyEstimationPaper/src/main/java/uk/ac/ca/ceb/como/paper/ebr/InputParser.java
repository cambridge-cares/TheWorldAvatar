package uk.ac.ca.ceb.como.paper.ebr;

import com.jayway.jsonpath.JsonPath;

/**
 * Extracts required parameters from the JSON input file that indicates whether<br>
 * to run cross-validation or the calculation of eof, what reaction class to<br>
 * use and where to find the reference set of species, etc. 
 * 
 * @author msff2
 *
 */
public class InputParser {
	/**
	 * The base path to files each containing ground state geometries and<br>
	 * vibrational frequencies for all species involved in the current<br>
	 * EBR calculation.   
	 */
	public static String getSrcCompoundsRef(String jsonString){
		return JsonPath.read(jsonString, "$.srcCompoundsRef");
	}
	/**
	 * The relative path to the file that has data of the initial set of <br>
	 * reference species, e.g. the experimental/estimated value of enthalpy<br>
	 * of formation, electronic energy and molar connectivity.
	 */
	public static String getSrcRefPool(String jsonString){
		return JsonPath.read(jsonString, "$.srcRefPool");
	}
	/**
	 * The relative path to the file that has the list of species whose<br>
	 * enthalpy of formation is to be calculated.
	 */
	public static String getSrcTargetPool(String jsonString){
		return JsonPath.read(jsonString, "$.srcTargetPool");
	}
	/**
	 * The folder that has all identified error-cancelling balanced<br>
	 * reactions used either in cross-validation or in the estimation of eof.
	 * It also has the results produced in either of the these two actions.
	 */
	public static String getDestRList(String jsonString){
		return JsonPath.read(jsonString, "$.destRList");
	}
	/**
	 * The folder under which all temporary files are stored during the<br>
	 * EBR calculation.
	 */
	public static String getTempFolder(String jsonString){
		return JsonPath.read(jsonString, "$.tempFolder");
	}
	/**
	 * This is the class of reaction user selects to run either cross-vali-<br>
	 * dation or the estimation of eof.
	 */
	public static String getReactionType(String jsonString){
		return JsonPath.read(jsonString, "$.reactionType");
	}
	/**
	 * A user selected number indicating how many times to run either the cross-<br>
	 * validation code or the eof estimation code to produce the final results. 
	 */
	public static int getCtrRuns(String jsonString){
		return JsonPath.read(jsonString, "$.ctrRuns");
	}
	/**
	 * User opts the number of reactions to identify while running the<br>
	 * cross-validation code or the eof estimation code.
	 */
	public static int getCtrRes(String jsonString){
		return JsonPath.read(jsonString, "$.ctrRes");
	}	
	/**
	 * A user selected number indicating how many radicals to use while<br>
	 * performing either cross-validation or the eof calculation. 
	 */
	public static int getCtrRadicals(String jsonString){
		return JsonPath.read(jsonString, "$.ctrRadicals");
	}	
	/**
	 * The name of the zip file including the extension (e.g. input.zip)<br>
	 * that contains all input files required to run either cross-validation<br>
	 * or the eof calculation. Some example input files are:
	 * - electronic structure calculation file.
	 * - reference species file with the experimental/calculated value eof.
	 */
	public static String getInputZipFile(String jsonString){
		return JsonPath.read(jsonString, "$.inputZipFile");
	}
	/**
	 * This method extracts a value that indicates whether to run the<br>
	 * cross-validation process or eof calculation. 
	 * 
	 * @param jsonString
	 * @return
	 */
	public static String getWhichProcessToRun(String jsonString){
		return JsonPath.read(jsonString, "$.whichProcessToRun");
	}
	/**
	 * If for a process the reference set and target set of species are the<br>
	 * same, the value of this parameter will be set true. It will be set<br>
	 * to false, otherwise.
	 * 
	 * @param jsonString
	 * @return
	 */
	public static String isRefAndTargetSetSame(String jsonString){
		return JsonPath.read(jsonString, "$.isRefAndTargetSetSame");
	}		
}
