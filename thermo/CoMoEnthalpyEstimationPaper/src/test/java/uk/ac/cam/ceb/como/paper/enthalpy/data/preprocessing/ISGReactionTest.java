package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
import java.util.HashMap;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing.DataPreProcessing;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;

public class ISGReactionTest {
	
	String folderName;
	String validTestResults;
	
	@Before
	public void generateTestResults() throws Exception{
		validTestResults = Folder.VALID_TEST_RESULT_ISG_TI_115.getFolderName();
		
		folderName = new FolderUtils().generateUniqueFolderName(Folder.ISG_TI_115.getFolderName());
		
		DataPreProcessing dataPreProcessingISG = new DataPreProcessing();
		
		ISGReactionType isgReactionTypePreProcessing = new ISGReactionType(true);

		dataPreProcessingISG.getPreProcessingErrorBalanceReaction(folderName, Folder.COMPOUNDS_REF_TI.getFolderName(), Folder.REF_POOL_TI.getFolderName(), Folder.REACTIONS_TI_ISG.getFolderName(), Folder.CROSS_VALIDATION.getFolderName(), Utils.ctrRuns, Utils.ctrRes, Utils.ctrRadicals_5, isgReactionTypePreProcessing, validTestResults);
	}
	
	@Test
	public void comparisonTest() {
		HashMap<String, String> resultFileMap = (new Utils()).generatePairedFileList(folderName, Folder.REACTIONS_TI_ISG.getFolderName(), validTestResults);
		for(String srcFilePath:resultFileMap.keySet()){
			File sourceFile = new File(srcFilePath);
			File targetFile = new File (resultFileMap.get(srcFilePath));
			(new Utils()).compareFiles(srcFilePath, sourceFile, targetFile, resultFileMap);
		}
	}	
}
