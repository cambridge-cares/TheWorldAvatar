package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.io.BufferedReader;
import java.io.BufferedWriter;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public class MoDSFileManagement extends MoDSMarshaller {
//	public static RepositoryManager repoManager = new RepositoryManager();
	Logger logger = Logger.getLogger(MoDSFileManagement.class);
//	public static List<String[]> dataLines;
	public static String jobFolderName;
	public static List<String> activeParameters_1 = new ArrayList<>();
	public static List<String> passiveParameters_1 = new ArrayList<>();
	public static String outputResponse_1 = new String();
	public static List<String> activeParameters_2 = new ArrayList<>();
	public static List<String> passiveParameters_2 = new ArrayList<>();
	public static String outputResponse_2 = new String();
	
	public static void main(String[] args) throws IOException, MoDSAgentException {
		MoDSFileManagement fileMagt = new MoDSFileManagement();

		String jobFolderName = "login-skylake.hpc.cam.ac.uk_1186309217579500";
		String jsonString = "{\"json\":{\"ontochemexpIRI\":{\"ignitionDelay\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600\"],\"flameSpeed\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400\"]},\"ontokinIRI\":{\"reactionList\":[\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570512_48\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570503_39\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570639_175\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570640_176\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570509_45\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570499_35\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570607_143\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570631_167\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570634_170\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570633_169\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570504_40\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570502_38\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570618_154\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570505_41\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570638_174\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570517_53\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570604_140\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570624_160\"],\"mechanism\":\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237\"}}}";
		
		fileMagt.createMoDSJob(jsonString, jobFolderName);
	}
	
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSAgentException {
		
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = JSonRequestParser.getOntoKinReactionsIRI(jsonString);
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshaller();
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList);
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		placeMoDSSlurmScript(jobFolderPath);
		return jobFolderPath;
	}
	
	protected void placeMoDSSlurmScript(String jobFolderPath) throws IOException, MoDSAgentException {
		// TODO
		File sourceSlurmScript = new File(getClass().getClassLoader().getResource(FILE_MODS_SLURM_SCRIPT).getFile());
		File modsSlurmScript = new File(jobFolderPath.concat(FRONTSLASH+FILE_MODS_SLURM_SCRIPT));
		
		// create the BufferedReader and BufferedWriter to read and write files
		BufferedReader br = null;
		BufferedWriter bw = null;
		
		// copy the modsslurm_como.sh script
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(sourceSlurmScript)));
	        bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(modsSlurmScript)));
	        String line = new String();
	        while ((line = br.readLine()) != null) {
	        	bw.write(line.concat("\n"));
	        }
	        bw.close();
	        br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
}
