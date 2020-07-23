package com.cmclinnovations.jps.agent.file_management.marshallr.yrt23;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.cmclinnovations.jps.agent.file_management.marshallr.IMoDSMarshaller;
import com.cmclinnovations.jps.agent.file_management.marshallr.MoDSFileManagement;
import com.cmclinnovations.jps.agent.file_management.marshallr.MoDSMarshaller;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public class MoDS4yrt23 extends MoDSFileManagement {
	
	public static void main(String[] args) throws IOException, MoDSAgentException {
		MoDS4yrt23 fileMagt = new MoDS4yrt23();
		
		// for ignition delay only
//		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-MoDSAgent\\Data\\For_yrt23\\JsonInput\\usc_dmc_yrt23_mech.json");
//		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-MoDSAgent\\Data\\For_yrt23\\JsonInput\\usc_dmm_yrt23_mech.json");
//		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-MoDSAgent\\Data\\For_yrt23\\JsonInput\\usc_ipa_yrt23_mech.json");
		
		// including flame speed
//		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-MoDSAgent\\Data\\For_yrt23\\JsonInput\\usc_orig_mech.json");

		// the owl file need to be regenerated with transport data
		// then the updated owl file need to be uploaded to the local host
		// then get the reaction mechanism IRI from query/sublime text lookup
		// the mechanism IRI inside below json need to be changed
		// the name of below json need to be changed in the folder
		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-MoDSAgent\\Data\\For_yrt23\\JsonInput\\usc_dmc_with_tran.json");
//		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-MoDSAgent\\Data\\For_yrt23\\JsonInput\\usc_dmm_mech.json");
//		File jsonFile = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-MoDSAgent\\Data\\For_yrt23\\JsonInput\\usc_ipa_mech.json");
		
		String jsonString = new String();
		String line = new String();
		BufferedReader br = new BufferedReader(new FileReader(jsonFile)); 
		while ((line = br.readLine()) != null) {
			jsonString = jsonString.concat(line);
		}
		
		String jobFolderName = "login-cpu.hpc.cam.ac.uk_"+jsonFile.getName().replace(".json", "");
		
		fileMagt.createMoDSJob(jsonString, jobFolderName);
	}
	
	@Override
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSAgentException {
		
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = new ArrayList<>(); // not in use, just for the arg place
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshaller4yrt23();
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList);
		// comment out plugInCantera if only do ignition delay times
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		placeMoDSSlurmScript(jobFolderPath);
		return jobFolderPath;
	}
}
