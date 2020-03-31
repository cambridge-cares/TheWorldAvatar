package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;

import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;

public class GlobalCrossValidation {

	public static void main(String[] args) throws JSONException, Exception {
	// TODO Auto-generated method stub
		
		String jText = new String(Files.readAllBytes(Paths.get(new File(args[0]).getAbsolutePath())), StandardCharsets.UTF_8);
		
		JSONObject jObject = new JSONObject(jText);
		
//        JSONArray jArray = jObject.getJSONArray("job");
        
//        System.out.println(jObject.get("job"));
        
        System.out.println(jObject.get("srcCompoundsRef"));
        System.out.println(jObject.get("srcRefPool"));
        System.out.println(jObject.get("destRList"));
        System.out.println(jObject.get("tempFolder"));
        System.out.println(jObject.get("reactionType"));
        System.out.println(jObject.get("ctrRuns"));
        System.out.println(jObject.get("ctrRes"));
        System.out.println(jObject.get("ctrRadicals"));
        
        String folderName = new FolderUtils().generateUniqueFolderName(jObject.get("destRList").toString());
        
        LeaveOneOutCrossValidationAlgorithm leaveOneOutCrossValidationAlgorithm = new LeaveOneOutCrossValidationAlgorithm();
        
        leaveOneOutCrossValidationAlgorithm.runGlobalCrossValidation(jObject.get("srcCompoundsRef").toString(), jObject.get("srcRefPool").toString(), jObject.get("destRList").toString(), EvaluationUtils.getCtrRuns(jObject.get("ctrRuns").toString()), EvaluationUtils.getCtrRuns(jObject.get("ctrRes").toString()), EvaluationUtils.getCtrRuns(jObject.get("ctrRadicals").toString()),EvaluationUtils.getReactionType(jObject.get("reactionType").toString()), jObject.get("tempFolder").toString());
        
	}
}