package uk.ac.ca.ceb.como.paper.ebr;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import uk.ac.cam.ceb.como.paper.enthalpy.cross_validation.LeaveOneOutCrossValidationAlgorithm;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;

/**
 * This class reads user requirements from a JSON input file to decide<br>
 * whether to perform cross-validation or the enthalpy of formation calculation.
 * 
 * @author msff2
 *
 */
public class EBRApproach {
	/**
	 * The main method to read user requirements from the JSON file that is<br>
	 * provided as the first argument. An example name of the JSON file<br>
	 * can be input.json.
	 * 
	 * @param args the name of the input json file
	 */
	public static void main(String[] args) throws Exception{
		try {
			if (args.length > 0) {
				EBRApproach ebrApproach = new EBRApproach();
				ebrApproach.callEBRProcess(args[0]);
			}
		} catch (IOException e) {
			throw new IOException("EBRCode: IOException occurred.");
		}
	}
	
	/**
	 * This method decides which of the two processes (cross-validation and<br>
	 * eof estimation) should run.
	 * 
	 * @param jsonInputFileName the JSON input file name including the extension  
	 */
	public void callEBRProcess(String jsonInputFileName) throws Exception{
		String jsonInput = null;
		EBRApproach ebrApproach = new EBRApproach();
		jsonInput = ebrApproach.readJsonFile(jsonInputFileName);
		ebrApproach.callEBRProcess(jsonInput);
		if (jsonInput.isEmpty()) {
			throw new IOException("EBRCode: The JSON input file is empty.");
		}
		if (InputParser.getWhichProcessToRun(jsonInput)
				.equalsIgnoreCase(Property.EBR_PROCESS_CROSS_VALIDATION.getName())) {
			runCrossValidation(jsonInput);
		}
		if (InputParser.getWhichProcessToRun(jsonInput)
				.equalsIgnoreCase(Property.EBR_PROCESS_EOF_ESTIMATION.getName())) {
			calculateEoF(jsonInput);
		}
	}
	
	/**
	 * Sets all parameters required to run the cross-validation process. 
	 *  
	 * @param jsonInput
	 */
	private void runCrossValidation(String jsonInput) throws Exception{
		LeaveOneOutCrossValidationAlgorithm leaveOneOutCrossValidationAlgorithm = new LeaveOneOutCrossValidationAlgorithm();
		leaveOneOutCrossValidationAlgorithm.runGlobalCrossValidation(
				InputParser.getSrcCompoundsRef("srcCompoundsRef").toString() + "/",
				InputParser.getSrcRefPool("srcRefPool").toString(), InputParser.getDestRList("destRList").toString(),
				EvaluationUtils.getCtrRuns(InputParser.getCtrRuns("ctrRuns")),
				EvaluationUtils.getCtrRuns(InputParser.getCtrRes("ctrRes")),
				EvaluationUtils.getCtrRuns(InputParser.getCtrRadicals("ctrRadicals")),
				EvaluationUtils.getReactionType(InputParser.getReactionType("reactionType")),
				InputParser.getTempFolder("tempFolder") + "/");
	}
	
	/**
	 * Sets all parameters required to run the enthalpy of formation of species. 
	 * 
	 * @param jsonInput
	 */
	private void calculateEoF(String jsonInput){
		
	}
	
	/**
	 * Reads the json content and returns this as a string.
	 *  
	 * @param file the name of file.
	 * @return
	 */
	private String readJsonFile(String file) throws IOException{
		String fileContent = "";
		if(!new File(file).exists()){
			throw new IOException("EBRCode: the input JSON file does not exist.");
		}
		if(new File(file).isFile()){
			BufferedReader br = openSourceFile(new File(file).getAbsolutePath());
			String line;
			while((line=br.readLine())!=null){
				fileContent = fileContent.concat(line);
			}
		}
		return fileContent;
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	public BufferedReader openSourceFile(String filePathPlusName)
			throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(
				filePathPlusName), "UTF-8"));
	}

}
