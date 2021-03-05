package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import java.io.File;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FileUtils;

public class GlobalCrossValidation {

	public static void main(String[] args) throws JSONException, Exception {
	// TODO Auto-generated method stub
		
		String jText = new String(Files.readAllBytes(Paths.get(new File(args[0]).getAbsolutePath())), StandardCharsets.UTF_8);		
		JSONObject jObject = new JSONObject(jText);	        
//      String folderName = new FolderUtils().generateUniqueFolderName(jObject.get("destRList").toString());
        
		/**
         * 
         * Find input.zip and unzip it before running global cross validation. File path is given in josn file "input.zip".
         *  
         */
	  FileUtils.getUnzipFolder(jObject.getString("inputZipFile"));
	  
      LeaveOneOutCrossValidationAlgorithm leaveOneOutCrossValidationAlgorithm = new LeaveOneOutCrossValidationAlgorithm();
      leaveOneOutCrossValidationAlgorithm.runGlobalCrossValidation(jObject.get("srcCompoundsRef").toString()+"/", jObject.get("srcRefPool").toString(), jObject.get("destRList").toString(), EvaluationUtils.getCtrRuns(jObject.getInt("ctrRuns")), EvaluationUtils.getCtrRuns(jObject.getInt("ctrRes")), EvaluationUtils.getCtrRuns(jObject.getInt("ctrRadicals")),EvaluationUtils.getReactionType(jObject.get("reactionType").toString()), jObject.get("tempFolder").toString()+"/");
	}
}