package com.cmclinnovations.jps.agent.json.parser;

import java.util.List;
import java.util.Map;


import com.jayway.jsonpath.JsonPath;
/**
 * This file reads all properties from the JSON input file, that is to say it reads:
 * - reference species
 * - target species (if available)
 * - the relative path to DFT calculation files
 * - the name of the destination folder where all outputs including EBRs<br>
 * and the EoF of target species are stored
 * - the name of the temporary folder
 * - the reaction class
 * - the number of runs
 * - the number of EBRs to identify
 * - the number of radicals to use
 * - the name of the zip file including the extension where all inputs are available
 * - the input indicating whether to run cross-validation or the EoF calculation
 * - the boolean value indicating if the target set of species is the same<br>
 * as the reference set. In this case, user needs to provide reference set.<br>
 * EBR Agent will assume that the reference set can be reused as the target set. 
 * 
 * @author msff2
 *
 */
public class JSonRequestParser {
	
	public static List<Map<String, Object>> getAllReferenceSpeciesIRI(String jsonString){
		return JsonPath.parse(jsonString).read("$.job.referenceSpecies[*]");
	}
	
	public static List<Map<String, Object>> getAllTargetSpeciesIRI(String jsonString){
		boolean booleanIndicator = JsonPath.parse(jsonString).read("$.isRefAndTargetSetSame");
		if(booleanIndicator==true) {
			return JsonPath.parse(jsonString).read("$.job.referenceSpecies[*]");
		} else {
			return JsonPath.parse(jsonString).read("$.job.targetSpecies[*]");
		}
	}

	public static String getDFTCalculationPath(String jsonString) {
		return JsonPath.read(jsonString, "$.srcCompoundsRef");
	}
	
	public static String getReferenceSpeciesPool(String jsonString){		
		return JsonPath.read(jsonString, "$.srcRefPool");
	}
	
	public static String getTargetSpeciesPool(String jsonString){		
		return JsonPath.read(jsonString, "$.srcTargetPool");
	}

	public static String getOutputFolderName(String jsonString){	
		return JsonPath.read(jsonString, "$.destRList");
	}

	public static String getTemporaryFolderName(String jsonString){	
		return JsonPath.read(jsonString, "$.tempFolder");
	}

	public static String getReactionClass(String jsonString){	
		return JsonPath.read(jsonString, "$.reactionType");
	}

	public static int getNumberOfRuns(String jsonString){	
		return JsonPath.read(jsonString, "$.ctrRuns");
	}

	public static int getNumberOfEBRsToFind(String jsonString){	
		return JsonPath.read(jsonString, "$.ctrRes");
	}

	public static int getNumberOfRadicalsToUse(String jsonString){	
		return JsonPath.read(jsonString, "$.ctrRadicals");
	}
	
	public static String getInputZipFile(String jsonString){	
		return JsonPath.read(jsonString, "$.inputZipFile");
	}

	public static String getWhichProcessToRun(String jsonString){	
		return JsonPath.read(jsonString, "$.whichProcessToRun");
	}

	public static boolean getIsRefAndTargetSetSame(String jsonString){	
		return JsonPath.read(jsonString, "$.isRefAndTargetSetSame");
	}
}