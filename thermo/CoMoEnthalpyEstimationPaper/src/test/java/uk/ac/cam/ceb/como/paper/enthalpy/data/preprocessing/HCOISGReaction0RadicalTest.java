package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;

import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.paper.enthalpy.cross_validation.LeaveOneOutCrossValidationAlgorithm;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;

public class HCOISGReaction0RadicalTest {
	
	String folderName;
	String validTestResults;
	
	@Before
	public void generateTestResults() throws Exception{
		validTestResults = Folder.VALID_TEST_RESULT_ISG_HCO_110.getFolderName();
		folderName = new FolderUtils().generateUniqueFolderName(Folder.ISG_HCO_110.getFolderName());
		LeaveOneOutCrossValidationAlgorithm leaveOneOutCrossValidationAlgorithm = new LeaveOneOutCrossValidationAlgorithm();
		ISGReactionType isgReactionTypePreProcessing = new ISGReactionType(true);
		Files.createDirectories(Paths.get(Folder.REACTIONS_HCO_ISG.getFolderName()+folderName));
		leaveOneOutCrossValidationAlgorithm.runGlobalCrossValidation(Folder.COMPOUNDS_REF_HCO.getFolderName(), Folder.REF_POOL_HCO.getFolderName(), Folder.REACTIONS_HCO_ISG.getFolderName()+folderName, Utils.ctrRuns, Utils.ctrRes, Utils.ctrRadicals_0, isgReactionTypePreProcessing, Folder.TEMP_FOLDER.getFolderName());
	}
	
	@Test
	public void comparisonTest() {
		HashMap<String, String> resultFileMap = (new Utils()).generatePairedFileList(folderName, Folder.REACTIONS_HCO_ISG.getFolderName(), validTestResults);
		for(String srcFilePath:resultFileMap.keySet()){
			File sourceFile = new File(srcFilePath);
			File targetFile = new File (resultFileMap.get(srcFilePath));
			(new Utils()).compareFiles(srcFilePath, sourceFile, targetFile, resultFileMap);
		}
	}
}
