package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
import java.util.HashMap;

import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HDReactionType;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;

public class HDReactionTest {
	String folderName;
	String validTestResults;
	
	@Before
	public void generateTestResults() throws Exception{
		validTestResults = Folder.VALID_TEST_RESULT_HD_HCO_110.getFolderName();
		
		folderName = new FolderUtils().generateUniqueFolderName(Folder.HD_HCO_110.getFolderName());
		
		DataPreProcessing dataPreProcessingHD = new DataPreProcessing();
		
		HDReactionType hdReactionTypePreProcessing = new HDReactionType();

		//Feroz, update this to call new method.
//		dataPreProcessingHD.getPreProcessingErrorBalanceReaction(folderName, Folder.COMPOUNDS_REF_HCO_HD.getFolderName(), Folder.REF_POOL_HCO_HD.getFolderName(), Folder.REACTIONS_HCO_HD.getFolderName(), Folder.CROSS_VALIDATION.getFolderName(), Utils.ctrRuns, Utils.ctrRes, Utils.ctrRadicals_0, hdReactionTypePreProcessing, validTestResults);
		
	}
	
	@Test
	public void comparisonTest() {
		HashMap<String, String> resultFileMap = (new Utils()).generatePairedFileList(folderName, Folder.REACTIONS_HCO_HD.getFolderName(), validTestResults);
		for(String srcFilePath:resultFileMap.keySet()){
			File sourceFile = new File(srcFilePath);
			File targetFile = new File (resultFileMap.get(srcFilePath));
			(new Utils()).compareFiles(srcFilePath, sourceFile, targetFile, resultFileMap);
		}
	}
}
