package com.cmclinnovations.jps.agent.ebr.test;

import java.io.BufferedReader;
import java.io.File;
import java.util.List;
import java.util.Map;
import org.junit.Test;

import com.cmclinnovations.jps.agent.ebr.EBRAgent;
import com.cmclinnovations.jps.agent.ebr.Utils;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
/**
 * This JUnit test verifies the creation of input set of files to EBR Agent<br>
 * under {user.home} by taking the reference and target sets of species from<br>
 * the input.json file which is under the path of java/test/resources.
 * 
 * @author msff2
 *
 */
public class EBRAgentInputFilesCreationTest {
	@Test
	public void testInputFolderCreation() throws Exception{
		BufferedReader br = Utils.openSourceFile(getClass().getClassLoader()
				.getResource("input.json").getPath());
		String line;
		String jsonInput = "";
		while((line=br.readLine())!=null){
			jsonInput = jsonInput.concat(line);
		}
		
		List<Map<String, Object>> refSpecies = JSonRequestParser.getAllTargetSpeciesIRI(jsonInput);
		for(Map map:refSpecies){
			System.out.println(map.get("ontocompchemIRI") +"<<>>"+map.get("ontospeciesIRI"));
		}
		System.out.println("srcCompoundsRef:"+JSonRequestParser.getDFTCalculationPath(jsonInput));
		System.out.println("srcRefPool:"+JSonRequestParser.getReferenceSpeciesPool(jsonInput));
		System.out.println("srcTargetPool:"+JSonRequestParser.getTargetSpeciesPool(jsonInput));
		System.out.println("destRList:"+JSonRequestParser.getOutputFolderName(jsonInput));
		System.out.println("tempFolder:"+JSonRequestParser.getTemporaryFolderName(jsonInput));
		System.out.println("reactionType:"+JSonRequestParser.getReactionClass(jsonInput));
		System.out.println("ctrRuns:"+JSonRequestParser.getNumberOfRuns(jsonInput));
		System.out.println("ctrRes:"+JSonRequestParser.getNumberOfEBRsToFind(jsonInput));
		System.out.println("ctrRadicals:"+JSonRequestParser.getNumberOfRadicalsToUse(jsonInput));
		System.out.println("InputZipFile:"+JSonRequestParser.getInputZipFile(jsonInput));
		System.out.println("whichProcessToRun:"+JSonRequestParser.getWhichProcessToRun(jsonInput));
		System.out.println("isRefAndTargetSetSame:"+JSonRequestParser.getIsRefAndTargetSetSame(jsonInput));
		
		EBRAgent ebrAgent = new EBRAgent();
		File inputZipFile = ebrAgent.getInputFile(jsonInput);
		System.out.println("Zip File Name:"+inputZipFile.getName());
	}	
}
