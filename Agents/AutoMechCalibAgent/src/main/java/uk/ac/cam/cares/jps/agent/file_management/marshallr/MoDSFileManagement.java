package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.BufferedReader;
import java.io.BufferedWriter;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.agent.configuration.AutoMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.Utils;

public class MoDSFileManagement extends MoDSMarshaller {
//	public static RepositoryManager repoManager = new RepositoryManager();
	Logger logger = Logger.getLogger(MoDSFileManagement.class);
	private AutoMechCalibAgentProperty autoMechCalibAgentProperty;
	
//	public static List<String[]> dataLines;
	public static String jobFolderName;
	public static List<String> activeParameters_1 = new ArrayList<>();
	public static List<String> passiveParameters_1 = new ArrayList<>();
	public static String outputResponse_1 = new String();
	public static List<String> activeParameters_2 = new ArrayList<>();
	public static List<String> passiveParameters_2 = new ArrayList<>();
	public static String outputResponse_2 = new String();
	
	public MoDSFileManagement(AutoMechCalibAgentProperty autoMechCalibAgentProperty) {
		super(autoMechCalibAgentProperty);
		this.autoMechCalibAgentProperty = autoMechCalibAgentProperty;
	}
	
//	public static void main(String[] args) throws IOException, AutoMechCalibAgentException {
//		MoDSFileManagement fileMagt = new MoDSFileManagement();
//		String jsonString = "{\"json\":{\"ontochemexpIRI\":{\"ignitionDelay\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600\"],\"flameSpeed\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400\"]},\"ontokinIRI\":{\"mechanism\":\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ReactionMechanism_73656018231261\",\"reactionList\":[\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264155_173\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264148_166\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264020_38\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264017_35\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264156_174\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264154_172\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264157_175\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264152_170\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264053_71\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264158_176\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264104_122\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264135_153\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264142_160\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264134_152\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264165_183\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264137_155\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264159_177\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264136_154\"]},\"mods\":{\"ignDelayOption\":{\"method\":\"1\",\"species\":\"AR\"},\"flameSpeedOption\":{\"tranModel\":\"mix-average\"},\"sensAna\":{\"topN\":\"10\",\"relPerturbation\":\"1e-3\"}}}}";
//		String jobFolderName = "MoDSMechCalibAgentTest-login-cpu.hpc.cam.ac.uk_"+Utils.getTimeStamp();
//		fileMagt.createMoDSJob(jsonString, jobFolderName);
//	}
	
	
	public String getRxnIRIList(File sensAnaResultsFile) throws IOException, AutoMechCalibAgentException {
		String rxnIRIList = new String();
		if (sensAnaResultsFile.isFile()) {
			BufferedReader br = new BufferedReader(new FileReader(sensAnaResultsFile));
			String[] header = br.readLine().split(",");
			int iriLocate = -1;
			for (int i = 0; i < header.length; i++) {
				if (header[i].contains("IRI")) {
					iriLocate = i;
				}
			}
			String line = new String();
			while ((line = br.readLine()) != null) {
				String[] data = line.split(",");
				rxnIRIList = rxnIRIList.concat(",\"").concat(data[iriLocate]).concat("\"");
			}
			br.close();
		}
		return rxnIRIList.substring(1);
	}
	
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, AutoMechCalibAgentException {
		
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = JSonRequestParser.getOntoKinReactionsIRI(jsonString);
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshaller(autoMechCalibAgentProperty);
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList, jsonString);
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList, jsonString);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		logger.info("The requested MoDS job was created.");
		return jobFolderPath;
	}
}
