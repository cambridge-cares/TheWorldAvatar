package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;

import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HDReactionType;
import uk.ac.cam.ceb.como.paper.enthalpy.cross_validation.LeaveOneOutCrossValidationAlgorithm;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;

public class HCOHDReaction0RadicalTest {
	String folderName;
	String validTestResults;
	
	@Before
	public void generateTestResults() throws Exception{
		validTestResults = Folder.VALID_TEST_RESULT_HD_HCO_110.getFolderName();
		folderName = new FolderUtils().generateUniqueFolderName(Folder.HD_HCO_110.getFolderName());
		LeaveOneOutCrossValidationAlgorithm leaveOneOutCrossValidationAlgorithm = new LeaveOneOutCrossValidationAlgorithm();
		HDReactionType hdReactionTypePreProcessing = new HDReactionType();
		Files.createDirectories(Paths.get(Folder.REACTIONS_HCO_HD.getFolderName()+folderName));
		leaveOneOutCrossValidationAlgorithm.runGlobalCrossValidation(Folder.COMPOUNDS_REF_HCO_HD.getFolderName(), Folder.REF_POOL_HCO_HD.getFolderName(), Folder.REACTIONS_HCO_HD.getFolderName()+folderName, Utils.ctrRuns, Utils.ctrRes, Utils.ctrRadicals_0, hdReactionTypePreProcessing, Folder.TEMP_FOLDER.getFolderName());
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
