package uk.ac.cam.cares.jps.agent.file_management.marshallr.yrt23;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.IMoDSMarshaller;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSFileManagement;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;

public class MoDS4yrt23 extends MoDSFileManagement {
	private MoDSMechCalibAgentProperty modsMechCalibAgentProperty;
	
	public MoDS4yrt23(MoDSMechCalibAgentProperty modsMechCalibAgentProperty) {
		super(modsMechCalibAgentProperty);
		this.modsMechCalibAgentProperty = modsMechCalibAgentProperty;
	}
	
//	public static void main(String[] args) throws IOException, MoDSMechCalibAgentException {
//		MoDS4yrt23 fileMagt = new MoDS4yrt23();
//		
//		// including flame speed
////		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-JPS\\Data\\For_yrt23\\JsonInput\\usc_orig_mech.json");
//
//		// the owl file need to be regenerated with transport data
//		// then the updated owl file need to be uploaded to the local host
//		// then get the reaction mechanism IRI from query/sublime text lookup
//		// the mechanism IRI inside below json need to be changed
//		// the name of below json need to be changed in the folder
////		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-JPS\\Data\\For_yrt23\\JsonInput\\usc_dmc_with_tran.json");
////		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-JPS\\Data\\For_yrt23\\JsonInput\\usc_dmm_with_tran.json");
//		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-JPS\\Data\\For_yrt23\\JsonInput\\usc_ipa_with_tran.json");
//		
//		String jsonString = new String();
//		String line = new String();
//		BufferedReader br = new BufferedReader(new FileReader(jsonFile)); 
//		while ((line = br.readLine()) != null) {
//			jsonString = jsonString.concat(line);
//		}
//		
//		String jobFolderName = "login-cpu.hpc.cam.ac.uk_"+jsonFile.getName().replace(".json", "");
//		
//		fileMagt.createMoDSJob(jsonString, jobFolderName);
//	}
	
	@Override
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSMechCalibAgentException {
		
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = new ArrayList<>(); // not in use, just for the arg place
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshaller4yrt23(modsMechCalibAgentProperty);
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList, jsonString);
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList, jsonString);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		return jobFolderPath;
	}
}
