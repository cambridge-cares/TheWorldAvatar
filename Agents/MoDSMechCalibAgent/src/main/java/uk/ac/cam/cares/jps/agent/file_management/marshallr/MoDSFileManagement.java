package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.IOException;
import java.util.List;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;

public class MoDSFileManagement extends MoDSMarshaller {
	Logger logger = Logger.getLogger(MoDSFileManagement.class);
	private MoDSMechCalibAgentProperty modsMechCalibAgentProperty;
	
	public MoDSFileManagement(MoDSMechCalibAgentProperty modsMechCalibAgentProperty) {
		super(modsMechCalibAgentProperty);
		this.modsMechCalibAgentProperty = modsMechCalibAgentProperty;
	}
	
//	public static void main(String[] args) throws IOException, MoDSMechCalibAgentException {
//		MoDSFileManagement fileMagt = new MoDSFileManagement();
//		String jsonString = "{\"json\":{\"ontochemexpIRI\":{\"ignitionDelay\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600\"],\"flameSpeed\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400\"]},\"ontokinIRI\":{\"mechanism\":\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ReactionMechanism_73656018231261\",\"reactionList\":[\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264155_173\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264148_166\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264020_38\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264017_35\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264156_174\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264154_172\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264157_175\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264152_170\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264053_71\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264158_176\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264104_122\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264135_153\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264142_160\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264134_152\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264165_183\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264137_155\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264159_177\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264136_154\"]},\"mods\":{\"ignDelayOption\":{\"method\":\"1\",\"species\":\"AR\"},\"flameSpeedOption\":{\"tranModel\":\"mix-average\"},\"samplingAlg\":{\"sobolPoints\":\"100000\"},\"calibrationAlg\":{\"initPoints\":\"1\"},\"sensAna\":{\"topN\":\"10\",\"relPerturbation\":\"1e-3\"}}}}";
//		String jobFolderName = "MoDSMechCalibAgentTest-login-cpu.hpc.cam.ac.uk_"+Utils.getTimeStamp();
//		fileMagt.createMoDSJob(jsonString, jobFolderName);
//	}
	
	/**
	 * Create all input files that required by a MoDS job. 
	 * 
	 * @param jsonString
	 * @param jobFolderName
	 * @return
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSMechCalibAgentException {
		
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = JSonRequestParser.getOntoKinReactionsIRI(jsonString);
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshaller(modsMechCalibAgentProperty);
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList, jsonString);
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList, jsonString);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		logger.info("The requested MoDS job was created.");
		return jobFolderPath;
	}
}
