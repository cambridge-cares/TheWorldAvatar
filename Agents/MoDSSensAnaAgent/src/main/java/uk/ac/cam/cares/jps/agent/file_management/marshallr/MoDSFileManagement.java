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

import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentProperty;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.Utils;

public class MoDSFileManagement extends MoDSMarshaller {
//	public static RepositoryManager repoManager = new RepositoryManager();
	Logger logger = Logger.getLogger(MoDSFileManagement.class);
//	public static List<String[]> dataLines;
	private MoDSSensAnaAgentProperty modsSensAnaAgentProperty;
//	public static String jobFolderName;
//	public static List<String> activeParameters_1 = new ArrayList<>();
//	public static List<String> passiveParameters_1 = new ArrayList<>();
//	public static String outputResponse_1 = new String();
//	public static List<String> activeParameters_2 = new ArrayList<>();
//	public static List<String> passiveParameters_2 = new ArrayList<>();
//	public static String outputResponse_2 = new String();
	
	public MoDSFileManagement(MoDSSensAnaAgentProperty modsSensAnaAgentProperty) {
		super(modsSensAnaAgentProperty);
		this.modsSensAnaAgentProperty = modsSensAnaAgentProperty;
	}
	
	public String getRxnIRIList(File sensAnaResultsFile) throws IOException, MoDSSensAnaAgentException {
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
	
	public String createMoDSJob(String jsonString, String jobFolderName) throws IOException, MoDSSensAnaAgentException {
		
		List<String> ignitionDelayExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(jsonString);
		List<String> flameSpeedExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(jsonString);
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = JSonRequestParser.getOntoKinReactionsIRI(jsonString);
		
		IMoDSMarshaller iMoDSMarshaller = new MoDSMarshaller(modsSensAnaAgentProperty);
		iMoDSMarshaller.initialise(jobFolderName);
		iMoDSMarshaller.plugInKinetics(ignitionDelayExpIRI, mechanismIRI, reactionIRIList, jsonString);
		iMoDSMarshaller.plugInCantera(flameSpeedExpIRI, mechanismIRI, reactionIRIList, jsonString);
		String jobFolderPath = iMoDSMarshaller.marshall();
		
		logger.info("The requested MoDS job was created.");
		return jobFolderPath;
	}
}
