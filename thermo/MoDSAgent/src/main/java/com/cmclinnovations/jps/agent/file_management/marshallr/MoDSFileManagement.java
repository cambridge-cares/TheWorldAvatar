package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.io.BufferedReader;
import java.io.BufferedWriter;

//import static org.junit.Assert.assertTrue; //package doesn't exist

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactoryConfigurationError;

import org.apache.log4j.Logger;
import org.json.JSONObject;
import org.xml.sax.SAXException;

import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;
import com.cmclinnovations.jps.kg.OntoChemExpKG;
import com.cmclinnovations.jps.kg.OntoKinKG;
import com.cmclinnovations.jps.kg.OntoChemExpKG.DataTable;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class MoDSFileManagement {
//	public static RepositoryManager repoManager = new RepositoryManager();
	Logger logger = Logger.getLogger(MoDSFileManagement.class);
//	public static List<String[]> dataLines;
	
	public static void main(String[] args) throws IOException, MoDSAgentException {
		MoDSFileManagement fileMagt = new MoDSFileManagement();
		List<String> experimentIRI = Arrays.asList("https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000", 
				"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800", 
				"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600");
		
		String mechanismIRI = "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237";
		List<String> reactionIRIList = Arrays.asList(
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570512_48", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570503_39", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570639_175", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570640_176", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570509_45", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570499_35", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570607_143", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570631_167", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570634_170", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570633_169", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570504_40", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570502_38", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570618_154", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570505_41", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570638_174", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570517_53", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570604_140", 
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570624_160");
		
		
//		File experimentData = fileMagt.collectExperimentalData(experimentIRI);
//		File inputParams = fileMagt.createKineticsInputFiles(mechanismIRI);
//		fileMagt.createMoDSCasesFiles(mechanismIRI, reactionIRIList);
		fileMagt.generateInputFiles(experimentIRI, mechanismIRI, reactionIRIList);
	}
	
	public String generateInputFiles(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException {
		// this whole generateInputFiles function need to be modified to made single connections between different parts of the IRIs
		
		String jobFolderPath = System.getProperty("user.home").concat("\\Documents").concat("\\JobFolder");
		
		collectExperimentalData(experimentIRI, jobFolderPath);
		createKineticsInputFiles(mechanismIRI, jobFolderPath);
		createMoDSCasesFiles(mechanismIRI, reactionIRIList, jobFolderPath);
		createMoDSInputsFile(jobFolderPath); // this part need to be further parameterised
		
		return jobFolderPath;
	}
	
	
	public File collectExperimentalData(List<String> experimentIRI, String jobFolderPath) throws IOException, MoDSAgentException {
		List<List<String>> headers = new ArrayList<List<String>>();
		List<List<String>> dataCollection = new ArrayList<List<String>>();
		for (String experiment : experimentIRI) {
			OntoChemExpKG ocekg = new OntoChemExpKG();
			DataTable dataTable = ocekg.formatExperimentDataTable(experiment);
			headers.add(dataTable.getTableHeader());
			dataCollection.addAll(dataTable.getTableData());
		}
		if (!headers.get(0).equals(headers.get(1)) || !headers.get(1).equals(headers.get(2))) {
			logger.error("The heasers of all experimental data tables should be consistent.");
		}
		
		List<String[]> dataLines;
		dataLines = new ArrayList<>();
		dataLines.add(headers.get(0).toArray(new String[0]));
		for (List<String> dataSingleLine : dataCollection) {
			dataLines.add(dataSingleLine.toArray(new String[0]));
		}
		
		System.out.println(dataLines);
		
		File jobFolder = new File(jobFolderPath);
		if (!jobFolder.exists()) {
			jobFolder.mkdir();
		}
		File expDataCSV = new File(jobFolderPath.concat("\\ExperimentalData.csv"));
		try (PrintWriter pw = new PrintWriter(expDataCSV)) {
			dataLines.stream()
			.map(this::convertToCSV)
			.forEach(pw::println);
		}
//		    assertTrue(expDataCSV.exists()); // package doesn't exist
		return expDataCSV;
	}
	
	public File createKineticsInputFiles(String mechanismIRI, String jobFolderPath) throws IOException, MoDSAgentException {
		File csvFile = new File(jobFolderPath.concat("\\ExperimentalData.csv"));
		String[] headerLine = null;
		String[] firstData = null;
		
		// experimental data file
		String iniTemp = null;
		String iniTempUnit = null;
		String iniPres = null;
		String iniPresUnit = null;
		
		// pre-set?
		String ignDelayModel = "3";
		String ignDelayDeltaT = "400";
		String ignDelaySpeciesIndex = "CO";
		String ignDelayShowAll = "1";
		
		if (csvFile.isFile()) {
		    // create BufferedReader and read data from csv
			BufferedReader csvReader = new BufferedReader(new FileReader(csvFile));
			headerLine = csvReader.readLine().split(",");
			System.out.println(headerLine);
			firstData = csvReader.readLine().split(",");
			System.out.println(firstData);
//			    String[] data = row.split(",");
			    // do something with the data
			csvReader.close();
		}
		
		for (int i = 0; i < headerLine.length; i++) {
			if (headerLine[i].contains("Temperature")) {
				iniTemp = firstData[i];
			} else if (headerLine[i].contains("unitTemp")) {
				iniTempUnit = firstData[i];
			} else if (headerLine[i].contains("Pressure")) {
				iniPres = firstData[i];
			} else if (headerLine[i].contains("unitPres")) {
				iniPresUnit = firstData[i];
			}
		}
		
		// further parameterise this part
		ArrayList<String> species = new ArrayList<String>(Arrays.asList(headerLine[0], firstData[0], 
				headerLine[1], firstData[1], headerLine[2], firstData[2]));
		
		// mechanism IRI
		String mechFile = "chemical_mechanism.xml"; // download mechanism file -- to be finished
		
		OntoKinKG ontoKinKG = new OntoKinKG();
//		String mechanismIRI = "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237";
		List<List<String>> numOfReactionsResults = ontoKinKG.queryNumOfReactions(mechanismIRI);
		int numOfReactions = Integer.parseInt(numOfReactionsResults.get(1).get(0));
		
		String oxidiser = "Test-All-Fuel-Ox-Mix"; // this name is to be determined, also to be connected to MoDS_Inputs.xml
		
		File all = new File(jobFolderPath.concat("\\All"));
		if (!all.exists()) {
			all.mkdir();
		}
		String filePath = jobFolderPath.concat("\\All\\InputParams.xml");
		
		
		String jsonString = new JSONObject()
				.put("kinetics", 
						new JSONObject()
						.put("filePath", filePath)
						.put("reactor", new JSONObject()
								.put("iniTemp", new JSONObject()
										.put("value", iniTemp)
										.put("unit", iniTempUnit))
								.put("iniPres", new JSONObject()
										.put("value", iniPres)
										.put("unit", iniPresUnit)))
						.put("chemistry", new JSONObject()
								.put("mechFile", mechFile)
								.put("numOfReactions", numOfReactions))
						.put("oxidiser", oxidiser)
						.put("ignDelayPostProcessor", new JSONObject()
								.put("ignDelayModel", ignDelayModel)
								.put("ignDelayDeltaT", ignDelayDeltaT)
								.put("ignDelaySpeciesIndex", ignDelaySpeciesIndex)
								.put("ignDelayShowAll", ignDelayShowAll))
						.put("mixtures", new JSONObject()
								.put("composition", species))).toString();
		
		InputParamsBuilder inputParamsBuilder = new InputParamsBuilder();
		File inputParams = inputParamsBuilder.formInputParamsXML(jsonString);
		
		
		//////////////////////////////////////////////////////// download mechanism file
		String mechanismTargetPath = jobFolderPath.concat("\\All\\").concat(mechFile);
		MechanismDownload mechanismDownload = new MechanismDownload();
		try {
			String mechanismWebPath = mechanismIRI.substring(0, mechanismIRI.indexOf("#"))
					.replace("/kb/", "/data/").replace(".owl", "/mechanism.xml");
			mechanismDownload.obtainMechanism(mechanismWebPath, mechanismTargetPath);
		} catch (SAXException | ParserConfigurationException | TransformerFactoryConfigurationError
				| TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return inputParams;
	}
	
	public File createMoDSCasesFiles(String mechanismIRI, List<String> reactionIRIList, String jobFolderPath) throws IOException, MoDSAgentException {
		File caseFile = new File("");
		
		createPassiveParametersAndOutputsFile(jobFolderPath);
		createActiveParametersFile(mechanismIRI, reactionIRIList, jobFolderPath);
		
		return caseFile;
	}
	
//	mechanism IRI (mechanism download, extract amount of reactions)
//	createKineticsInputFiles
//	createMoDSCasesFiles (active parameters, passive parameters and outputs) -- DONE
//	createMoDSInputsFile
	
	
	public File createPassiveParametersAndOutputsFile(String jobFolderPath) throws IOException, MoDSAgentException {
//		add the caseNames to the start of each line of experimental data file
		BufferedReader br = null;
		BufferedWriter bw = null;
//		final String lineSep=System.getProperty("line.separator");
		File experimentData = new File(jobFolderPath.concat("\\ExperimentalData.csv"));
        File initial = new File(jobFolderPath.concat("\\Initial"));
        if (!initial.exists()) {
        	initial.mkdir();
		}
        File passiveParametersAndOutputs = new File(jobFolderPath.concat("\\Initial\\MODS_SIM_INITFILE_cases.csv"));
		try {
	        br = new BufferedReader(new InputStreamReader(new FileInputStream(experimentData)));
	        bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(passiveParametersAndOutputs), "UTF-8"));
	        // add the column title
	        String line = br.readLine();
	        bw.write("Case name".concat(",").concat(line).concat("\n"));
	        line = br.readLine();
	        bw.write("PODE3_Figure%2D5_Parent%20Case".concat(",").concat(line).concat("\n")); // further parameterise this case name
	        // add the data part
	        int i = 0;
	        while ((line = br.readLine()) != null) {
	        	i += 1;
	        	bw.write("PODE3_Figure%2D5_case_".concat(Integer.toString(i)).concat(",").concat(line).concat("\n")); // further parameterise this case name
			}
			bw.close();
			br.close();
	    } catch (IOException e) {
	        e.printStackTrace();
	    }
		
		return passiveParametersAndOutputs;
	}
	
	public File createActiveParametersFile(String mechanismIRI, List<String> reactionIRIList, String jobFolderPath) throws IOException, MoDSAgentException {
//		add caseNames
//		take equations of reactions from mechanismIRI file as header
//		set all starting parameters to 1
		
		OntoKinKG ontoKinKG = new OntoKinKG();
		List<String> reactionsToOptimiseList = ontoKinKG.queryReactionsToOptimise(mechanismIRI, reactionIRIList);
		
//		System.out.println(reactionsToOptimiseList);
		
		List<String> columnTitle = new ArrayList<>();
		columnTitle.add("Case name");
		columnTitle.addAll(reactionsToOptimiseList);
//		List<String> caseNames = new ArrayList<>();
//		for (int i = 0; i < ; i++) {
//			if (i == 0) {
//				caseNames.add("PODE3_Figure%2D5_Parent%20Case");
//			} else {
//				caseNames.add("PODE3_Figure%2D5_case_".concat(Integer.toString(i)));
//			}
//		}
		List<String> reactionMultipliers = new ArrayList<>();
		for (int i = 0; i < reactionsToOptimiseList.size(); i++) {
			reactionMultipliers.add("1");
		}
		
		BufferedReader br = null;
		BufferedWriter bw = null;
		File experimentData = new File(jobFolderPath.concat("\\ExperimentalData.csv"));
		File initial = new File(jobFolderPath.concat("\\JobFolder\\Initial"));
		if (!initial.exists()) {
			initial.mkdir();
		}
		File activeParameters = new File(jobFolderPath.concat("\\Initial\\MODS_SIM_INITFILE_AIVarInitReadFile.csv"));
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(experimentData)));
	        bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(activeParameters), "UTF-8"));
	        // add the column title
	        br.readLine();
	        bw.write(convertToCSV(columnTitle.toArray(new String[0])).concat("\n"));
	        br.readLine();
	        bw.write("PODE3_Figure%2D5_Parent%20Case".concat(",").concat(convertToCSV(reactionMultipliers.toArray(new String[0]))).concat("\n"));
	        int i = 0;
	        while ((br.readLine()) != null) {
	        	i += 1;
	        	bw.write("PODE3_Figure%2D5_case_".concat(Integer.toString(i)).concat(",").concat(convertToCSV(reactionMultipliers.toArray(new String[0]))).concat("\n"));
	        }
	        bw.close();
			br.close();
		} catch (IOException e) {
	        e.printStackTrace();
	    }
		
//		List<String[]> dataLines;
//		dataLines = new ArrayList<>();
//		
////		dataLines.add(headers.get(0).toArray(new String[0]));
////		for (List<String> dataSingleLine : dataCollection) {
////			dataLines.add(dataSingleLine.toArray(new String[0]));
////		}
//		
//		System.out.println(dataLines);
//		
//		
//		
//		
//		new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-OntologyTools\\Codes\\thermochemistry\\MoDSAgent\\src\\main\\resources\\JobFolder").mkdir();
//		File expDataCSV = new File("C:\\Users\\jb2197\\Documents\\c4e-jb2197-OntologyTools\\Codes\\thermochemistry\\MoDSAgent\\src\\main\\resources\\JobFolder\\ExperimentalData.csv");
//		try (PrintWriter pw = new PrintWriter(expDataCSV)) {
//			dataLines.stream()
//			.map(this::convertToCSV)
//			.forEach(pw::println);
//		}
//		
		return activeParameters;
		
	}
	
	public File createMoDSInputsFile(String jobFolderPath) throws IOException, MoDSAgentException {
		MoDSMarshaller modsMarshaller = new MoDSMarshaller();
		File workingDir = new File(jobFolderPath.concat("\\Working_dir"));
		if (!workingDir.exists()) {
			workingDir.mkdir();
		}
		String filePath = jobFolderPath.concat("\\Working_dir\\MoDS_inputs.xml");
		File modsInputsXML = new File(filePath);
		
		MoDSJson modsJson = new MoDSJson();
		// activa parameters
		HashMap<String, String> activeParameter = new HashMap<String, String>();
		activeParameter.put("DMM3%20%2B%20O2%20%5B%3D%5D%20HO2%20%2B%20DMM3B", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[35]");
		activeParameter.put("DMM3%20%2B%20HO2%20%5B%3D%5D%20H2O2%20%2B%20DMM3B", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[38]");
		activeParameter.put("O2%20%2B%20DMM3B%20%3D%5D%20DMM3BO2", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[39]");
		activeParameter.put("DMM3BO2%20%3D%5D%20O2%20%2B%20DMM3B", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[40]");
		activeParameter.put("DMM3BO2%20%5B%3D%5D%20DMM3_OOH3_5", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[41]");
		activeParameter.put("DMM3_KET35%20%3D%5D%20OH%20%2B%20CH3OCOO%20%2B%20COCOC%2AO", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[45]");
		activeParameter.put("DMM3B%20%5B%3D%5D%20CH3OCH2OCH2%20%2B%20CH3OCHO", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[48]");
		activeParameter.put("CH3OCO%20%5B%3D%5D%20CO2%20%2B%20CH3", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[53]");
		activeParameter.put("OH%20%2B%20HO2%20%5B%3D%5D%20O2%20%2B%20H2O", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[140]");
		activeParameter.put("H2O2%20%28%2B%20M%29%20%5B%3D%5D%20OH%20%28%2B%20M%29", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[143]");
		activeParameter.put("O2%20%2B%20HCO%20%5B%3D%5D%20CO%20%2B%20HO2", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[154]");
		activeParameter.put("HCO%20%2B%20CH3%20%5B%3D%5D%20CO%20%2B%20CH4", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[160]");
		activeParameter.put("OH%20%2B%20CH2O%20%5B%3D%5D%20H2O%20%2B%20HCO", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[167]");
		activeParameter.put("HO2%20%2B%20CH2O%20%5B%3D%5D%20H2O2%20%2B%20HCO", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[169]");
		activeParameter.put("CH2O%20%2B%20CH3%20%5B%3D%5D%20CH4%20%2B%20HCO", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[170]");
		activeParameter.put("O2%20%2B%20CH3%20%5B%3D%5D%20OH%20%2B%20CH2O", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[174]");
		activeParameter.put("HO2%20%2B%20CH3%20%5B%3D%5D%20OH%20%2B%20CH3O", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[175]");
		activeParameter.put("HO2%20%2B%20CH3%20%5B%3D%5D%20O2%20%2B%20CH4", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value[176]");
		
		// passive parameters
		HashMap<String, String> passiveParameter = new HashMap<String, String>();
		passiveParameter.put("DMM3", "//srm_inputs/mixtures[@type='composition']/composition[@name='Test-All-Fuel-Ox-Mix']/value[@species='DMM3']");
		passiveParameter.put("O2", "//srm_inputs/mixtures[@type='composition']/composition[@name='Test-All-Fuel-Ox-Mix']/value[@species='O2']");
		passiveParameter.put("N2", "//srm_inputs/mixtures[@type='composition']/composition[@name='Test-All-Fuel-Ox-Mix']/value[@species='N2']");
		passiveParameter.put("Temperature", "//srm_inputs/property_group[@ref='Reactor']/property[@ref='IniTemp']/value");
		passiveParameter.put("Pressure", "//srm_inputs/property_group[@ref='Reactor']/property[@ref='IniPres']/value");
		
		
		// output response
		String outputs = new String();
		outputs = "Ignition%20time%20%5Bms%5D";
		
		// name of cases
		List<String> nameOfCases = new ArrayList<>();
		nameOfCases.add("PODE3_Figure%2D5_Parent%20Case");
		for (int i = 1; i < 63; i++) {
			nameOfCases.add("PODE3_Figure%2D5_case_" + i);
		}
		
		modsJson.setActiveParameter(activeParameter);
		modsJson.setPassiveParameter(passiveParameter);
		modsJson.setOutputs(outputs);
		modsJson.setNameOfCases(nameOfCases);
		
		// prepare modsJson
		String jsonString = modsMarshaller.collectMoDSInputsJson(modsJson);
		
		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		try {
			JsonNode jsonNode = objectMapper.readTree(jsonString);
			modsMarshaller.formMoDSInputsFile(jsonNode, filePath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return modsInputsXML;
	}
	
	
	private String convertToCSV(String[] data) {
	    return Stream.of(data)
	      .map(this::escapeSpecialCharacters)
	      .collect(Collectors.joining(","));
	}
	
	private String escapeSpecialCharacters(String data) {
	    String escapedData = data.replaceAll("\\R", " ");
	    if (data.contains(",") || data.contains("\"") || data.contains("'")) {
	        data = data.replace("\"", "\"\"");
	        escapedData = "\"" + data + "\"";
	    }
	    return escapedData;
	}
}
