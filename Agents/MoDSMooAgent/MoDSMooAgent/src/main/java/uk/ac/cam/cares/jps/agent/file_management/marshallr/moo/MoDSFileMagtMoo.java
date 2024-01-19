package uk.ac.cam.cares.jps.agent.file_management.marshallr.moo;
// package uk.ac.cam.cares.jps.base.util.MatrixConverter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.IMoDSMarshaller;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSFileManagement;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;

public class MoDSFileMagtMoo extends MoDSFileManagement {
	private MoDSMooAgentProperty MoDSMooAgentProperty;
	
	public MoDSFileMagtMoo(MoDSMooAgentProperty MoDSMooAgentProperty) {
		super(MoDSMooAgentProperty);
		this.MoDSMooAgentProperty = MoDSMooAgentProperty;
	}
	
	@Override
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSMooAgentException {
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = new ArrayList<>(); // not in use, just for the arg place
		List<String> dataVar = new ArrayList<>(); // not in use, just for the arg place

		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshallerMoo(MoDSMooAgentProperty);
		iMoDSMarshaller.initialise(jobFolderName);
		
				
		//iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList, jsonString);
		iMoDSMarshaller.plugInModelMoo(dataVar, mechanismIRI, reactionIRIList, mechanismIRI);
		//iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList, jsonString);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		return jobFolderPath;
	}

	private List<String> getRecordFromLine(String nextLine) {
		// TODO Auto-generated method stub
		return null;
	}
	
}
