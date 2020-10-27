package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.paper.enthalpy.cross_validation.LeaveOneOutCrossValidationAlgorithm;
import uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing.DataPreProcessing;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;

public class TiISGReaction5RadicalTest {
	
	String folderName;
	String validTestResults;
	
	@Before
	public void generateTestResults() throws Exception{
		validTestResults = Folder.VALID_TEST_RESULT_ISG_TI_115.getFolderName();
		folderName = new FolderUtils().generateUniqueFolderName(Folder.ISG_TI_115.getFolderName());
		LeaveOneOutCrossValidationAlgorithm leaveOneOutCrossValidationAlgorithm = new LeaveOneOutCrossValidationAlgorithm();
		ISGReactionType isgReactionType = new ISGReactionType(true);
		Files.createDirectories(Paths.get(Folder.REACTIONS_TI_ISG.getFolderName()+folderName));
		leaveOneOutCrossValidationAlgorithm.runGlobalCrossValidation(Folder.COMPOUNDS_REF_TI.getFolderName(), Folder.REF_POOL_TI.getFolderName(), Folder.REACTIONS_TI_ISG.getFolderName()+folderName, Utils.ctrRuns, Utils.ctrRes, Utils.ctrRadicals_5, isgReactionType, Folder.TEMP_FOLDER.getFolderName());
	}
	
	@Test
	public void comparisonTest() {
		HashMap<String, String> resultFileMap = (new Utils()).generatePairedFileList(folderName, Folder.REACTIONS_TI_ISG.getFolderName(),  validTestResults);
		for(String srcFilePath:resultFileMap.keySet()){
			File sourceFile = new File(srcFilePath);
			File targetFile = new File (resultFileMap.get(srcFilePath));
			(new Utils()).compareFiles(srcFilePath, sourceFile, targetFile, resultFileMap);
		}
	}	
}
