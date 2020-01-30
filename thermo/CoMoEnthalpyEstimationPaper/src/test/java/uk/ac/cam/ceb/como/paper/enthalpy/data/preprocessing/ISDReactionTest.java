package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
import java.util.HashMap;

import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;

public class ISDReactionTest {
	String folderName;
	String validTestResults;
	
	@Before
	public void generateTestResults() throws Exception{
		validTestResults = Folder.VALID_TEST_RESULT_ISD_HCO_110.getFolderName();
		
		folderName = new FolderUtils().generateUniqueFolderName(Folder.ISD_HCO_110.getFolderName());
		
		DataPreProcessing dataPreProcessingISD = new DataPreProcessing();
		
		ISDReactionType isdReactionTypePreProcessing = new ISDReactionType();

		dataPreProcessingISD.getPreProcessingErrorBalanceReaction(folderName, Folder.COMPOUNDS_REF_HCO.getFolderName(), Folder.REF_POOL_HCO.getFolderName(), Folder.REACTIONS_HCO_ISD.getFolderName(), Folder.CROSS_VALIDATION.getFolderName(), Utils.ctrRuns, Utils.ctrRes, Utils.ctrRadicals_0, isdReactionTypePreProcessing, validTestResults);
	}
	
	@Test
	public void comparisonTest() {
		HashMap<String, String> resultFileMap = (new Utils()).generatePairedFileList(folderName, Folder.REACTIONS_HCO_ISD.getFolderName(), validTestResults);
		for(String srcFilePath:resultFileMap.keySet()){
			File sourceFile = new File(srcFilePath);
			File targetFile = new File (resultFileMap.get(srcFilePath));
			(new Utils()).compareFiles(srcFilePath, sourceFile, targetFile, resultFileMap);
		}
	}
}
