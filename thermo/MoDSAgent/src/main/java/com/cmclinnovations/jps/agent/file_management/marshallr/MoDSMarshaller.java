package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.output.FileWriterWithEncoding;
import org.apache.commons.math3.stat.regression.ModelSpecificationException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.jps.agent.file_management.InitMoDSInputs;
import com.cmclinnovations.jps.agent.file_management.MoDSInputsState;
import com.cmclinnovations.jps.agent.file_management.mods.MoDS;
import com.cmclinnovations.jps.agent.file_management.mods.algorithms.Algorithm;
import com.cmclinnovations.jps.agent.file_management.mods.algorithms.AlgorithmS;
import com.cmclinnovations.jps.agent.file_management.mods.cases.Case;
import com.cmclinnovations.jps.agent.file_management.mods.cases.CaseS;
import com.cmclinnovations.jps.agent.file_management.mods.files.FileS;
import com.cmclinnovations.jps.agent.file_management.mods.functions.Function;
import com.cmclinnovations.jps.agent.file_management.mods.functions.FunctionS;
import com.cmclinnovations.jps.agent.file_management.mods.models.Model;
import com.cmclinnovations.jps.agent.file_management.mods.models.ModelS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.Detail;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.DetailS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.InitialRead;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.Parameter;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.ParameterS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.WorkingRead;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.WorkingWrite;
import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;

public class MoDSMarshaller extends MoDSInputsState implements IMoDSMarshaller {
	public List<String> caseNameList = new ArrayList<>();
	private static Logger logger = LoggerFactory.getLogger(MoDSMarshaller.class);

	public static void main(String[] args) throws MoDSAgentException {
		MoDSMarshaller modsMarshaller = new MoDSMarshaller();
		new File(
				"C:\\Users\\jb2197\\Documents\\c4e-jb2197-OntologyTools\\Codes\\thermochemistry\\MoDSAgent\\src\\main\\resources\\JobFolder\\Working_dir")
						.mkdir();
		String filePath = "C:\\Users\\jb2197\\Documents\\c4e-jb2197-OntologyTools\\Codes\\thermochemistry\\MoDSAgent\\src\\main\\resources\\JobFolder\\Working_dir\\MoDS_inputs.xml";

//		JSONObject detailJsonString = new JSONObject().put("detail",
//				new JSONObject().put("name", "testName").put("content", "testContent"));
//		JSONObject detailJsonString2 = new JSONObject().put("detail",
//				new JSONObject().put("name", "testName2").put("content", "testContent2"));
//		List<JSONObject> listOfDetail = new ArrayList<>();
//		listOfDetail.add(detailJsonString);
//		listOfDetail.add(detailJsonString2);

//		JSONObject algorithmJsonString = new JSONObject().put("algorithm", new JSONObject().put("name", "SamplingAlg")
////						.put("displayName", "only4test")
//				.put("details", new JSONArray(listOfDetail)));
//
//		JSONObject algorithmJsonString2 = new JSONObject().put("algorithm",
//				new JSONObject().put("name", "CalibrationAlg").put("displayName", "only4test").put("details",
//						new JSONArray(listOfDetail)));
//		List<JSONObject> listOfAlgorithm = new ArrayList<>();
//		listOfAlgorithm.add(algorithmJsonString);
//		listOfAlgorithm.add(algorithmJsonString2);

//		JSONObject modelJsonString = new JSONObject()
//				.put("model", new JSONObject()
//						.put("name", "kinetics_SRM")
////						.put("ref", "kinetics_SRM")
//						.put("details", new JSONArray(listOfDetail)));
//
//		JSONObject modelJsonString2 = new JSONObject()
//				.put("model",	new JSONObject()
////						.put("name", "Cantera")
//						.put("ref", "Cantera")
//						);
////						.put("details",	new JSONArray(listOfDetail)));
//		List<JSONObject> listOfModel = new ArrayList<>();
//		listOfModel.add(modelJsonString);
//		listOfModel.add(modelJsonString2);
//		
//		JSONObject caseJsonString = new JSONObject()
//				.put("case", new JSONObject()
//						.put("name", "PODE3_Figure")
////						.put("ref", "PODE3_Figure")
//						.put("models", new JSONArray(listOfModel)));
//		JSONObject caseJsonString2 = new JSONObject()
//				.put("case", new JSONObject()
////						.put("name", "PODE3_Figure")
//						.put("ref", "PODE3_Figure")
//						);
////						.put("models", new JSONArray(listOfModel)));
//		List<JSONObject> listOfCase = new ArrayList<>();
//		listOfCase.add(caseJsonString);
//		listOfCase.add(caseJsonString2);
		
//		String jsonString = new JSONObject().put("mods", new JSONObject()
//				.put("algorithms", new JSONArray(listOfAlgorithm))
//				.put("models", new JSONArray(listOfModel))
//				.put("cases", new JSONArray(listOfCase)))
//				.toString();
		
		
		MoDSJson modsJson = new MoDSJson();
		
		// active parameters
		LinkedHashMap<String, String> activeParameter = new LinkedHashMap<String, String>();
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
		LinkedHashMap<String, String> passiveParameter = new LinkedHashMap<String, String>();
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
		
		
		
//		prepare modsJson
		String jsonString = modsMarshaller.collectMoDSInputsJson(modsJson);
		
		
		
		
		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		try {
			JsonNode jsonNode = objectMapper.readTree(jsonString);
//			System.out.println(jsonNode);
//			modsMarshaller.formMoDSInputsFile(jsonNode, filePath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public String collectMoDSInputsJson(MoDSJson modsJson) throws MoDSAgentException {
		String jsonString = new String();
		// get active parameters
		LinkedHashMap<String, String> activeParameters = modsJson.getActiveParameter();
		String actives = new String();
		List<String> activeList = new ArrayList<>();
		for (String i : activeParameters.keySet()) {
			actives = actives.concat(" subtype_").concat(i);
			activeList.add(i);
		}
		// get passive parameters
		LinkedHashMap<String, String> passiveParameters = modsJson.getPassiveParameter();
		// get response of the model
		String outputResponse = modsJson.getOutputs();
		// get list of case names
		List<String> caseNameList = modsJson.getNameOfCases();
		
		// algorithms
		LinkedHashMap<String, HashMap<String, String>> algorithms = new LinkedHashMap<String, HashMap<String, String>>();
		LinkedHashMap<String, String> algo1 = new LinkedHashMap<String, String>();
		algo1.put("optimisable_param_subtypes", actives.substring(1));
		algo1.put("response_param_subtypes", "subtype_".concat(outputResponse));
		algo1.put("algorithm_type", "Sobol");
		algo1.put("model_name", "exe");
		algo1.put("objective_function", "NormalisedSumOfSquares");
		algo1.put("output_by_case", "false");
		algo1.put("output_values", "true");
		algo1.put("n_points", "1000");
		algo1.put("seed", "1");
		algo1.put("output_interval", "10");
		algo1.put("previous_algorithm", "Initial");
		LinkedHashMap<String, String> algo2 = new LinkedHashMap<String, String>();
		algo2.put("optimisable_param_subtypes", actives.substring(1));
		algo2.put("response_param_subtypes", "subtype_".concat(outputResponse));
		algo2.put("algorithm_type", "Hooke_Jeeves");
		algo2.put("model_name", "exe");
		algo2.put("objective_function", "NormalisedSumOfSquares");
		algo2.put("output_by_case", "false");
		algo2.put("output_values", "true");
		algo2.put("n_iters", "400");
		algo2.put("n_initial_points", "1");
		algo2.put("constrained", "true");
		algo2.put("rho", "0.5");
		algo2.put("rho_factor", "0.7");
		algo2.put("epsilon", "0.001");
		algo2.put("previous_algorithm", "SamplingAlg");
		algorithms.put("SamplingAlg", algo1);
		algorithms.put("CalibrationAlg", algo2);
		
		// models
		HashMap<String, HashMap<String, String>> models = new HashMap<String, HashMap<String, String>>();
		HashMap<String, String> mod1 = new HashMap<String, String>();
		mod1.put("executable_name", "/home/jb2197/Codes_kinetics/mods-backend/Applications/MoDS/bin/runKineticsSRM.sh");
		mod1.put("working_directory", "");
		models.put("kinetics_SRM", mod1);
        
		// cases
		HashMap<String, List<String>> cases = new HashMap<String, List<String>>();
		List<String> caseModelList = new ArrayList<>();
		caseModelList.add("kinetics_SRM");
		for (String caseName : caseNameList) {
			cases.put(caseName, caseModelList);
		}
		
		// files
		HashMap<String, HashMap<String, String>> files = new HashMap<String, HashMap<String, String>>();
		HashMap<String, String> file1 = new HashMap<String, String>();
		file1.put("file_type", "XML");
		file1.put("XML_namespace", "http://como.cheng.cam.ac.uk/srm");
		HashMap<String, String> file2 = new HashMap<String, String>();
		file2.put("file_type", "DSV");
		file2.put("delimiter", ",");
		HashMap<String, String> file3 = new HashMap<String, String>();
		file3.put("file_type", "DSV");
		file3.put("delimiter", ",");
		HashMap<String, String> file4 = new HashMap<String, String>();
		file4.put("file_type", "DSV");
		file4.put("delimiter", ",");
		files.put("InputParams.xml", file1);
		files.put("OutputCase00001Cyc0001Info.csv", file2);
		files.put("MODS_SIM_INITFILE_cases.csv", file3);
		files.put("MODS_SIM_INITFILE_AIVarInitReadFile.csv", file4);
		
		// parameters
//		List<JSONObject> parameters = collectActiveParameters(activeParameters, caseNameList, caseModelList);
//		parameters.addAll(collectPassiveParameters(passiveParameters, caseNameList, caseModelList));
//		parameters.add(collectOutputParameters(outputResponse, caseNameList, caseModelList));
//        
		/**
		jsonString = new JSONObject().put("mods", new JSONObject()
				.put("algorithms", new JSONArray(collectAlgorithms(algorithms)))
				.put("models", new JSONArray(collectModels(models)))
				.put("cases", new JSONArray(collectCases(cases)))
				.put("files", new JSONArray(collectFiles(files)))
				.put("parameters", new JSONArray(parameters)))
				.toString();
		*/
		
		
		return jsonString;
	}
	
	// to be deleted
	/**
	public List<JSONObject> collectAlgorithms(LinkedHashMap<String, HashMap<String, String>> algorithms) {
		List<JSONObject> algorithmsInJson = new ArrayList<>();
		for (String i : algorithms.keySet()) {
			JSONObject algoJson = new JSONObject().put("algorithm", new JSONObject().put("name", i)
					.put("details", new JSONArray(collectDetails(algorithms.get(i)))));
			algorithmsInJson.add(algoJson);
		}
		return algorithmsInJson;
	}*/
	
	public void collectAlgorithms(LinkedHashMap<String, LinkedHashMap<String, String>> algorithms) throws IOException, MoDSAgentException {
		for (String i : algorithms.keySet()) {
			String algoJson = new JSONObject().put("name", i)
					.put("details", collectDetails(algorithms.get(i))).toString();
			JsonNode locatedNode = modsJsonNode.path("algorithms").path("algorithm");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(algoJson));
		}
	}
	
	public void updateAlgorithms(String detailLocation, String newContent) throws IOException, MoDSAgentException {
		// TODO
		JsonNode algoNodes = modsJsonNode.path("algorithms").path("algorithm");
		for (JsonNode algo : algoNodes) {
			JsonNode details = algo.path("details").path("detail");
			for (JsonNode detail : details) {
				if (detail.get("name").toString().toLowerCase().equalsIgnoreCase("\""+detailLocation+"\"")) {
					String origContent = detail.get("content").toString().substring(1);
					origContent = origContent.substring(0, origContent.length()-1);
					String updatedContent = origContent+" "+newContent;
					((ObjectNode) detail).put("content", updatedContent);
				}
			}
		}
	}
	
	
	
	// to be deleted 
	/**
	public List<JSONObject> collectModels(HashMap<String, HashMap<String, String>> models) {
		// do operation, public void, add things to jsonnode
		List<JSONObject> modelsInJson = new ArrayList<>();
		for (String i : models.keySet()) {
			JSONObject modJson = new JSONObject().put("model", new JSONObject().put("name", i)
					.put("details", new JSONArray(collectDetails(models.get(i)))));
			modelsInJson.add(modJson);
		}
		return modelsInJson;
	}*/
	
	public void collectModels(LinkedHashMap<String, LinkedHashMap<String, String>> models) throws IOException, MoDSAgentException {
		for (String i : models.keySet()) {
			String modJson = new JSONObject().put("name", i)
					.put("details", collectDetails(models.get(i))).toString();
			System.out.println(modsJsonNode.path("models"));
			System.out.println(modsJsonNode.path("models").path("model"));
			JsonNode locatedNode = modsJsonNode.path("models").path("model");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(modJson));	
		}
	}
	
	// for test, to be deleted
	/**
	public void collectModelsTest(LinkedHashMap<String, LinkedHashMap<String, String>> models, JsonNode node) throws IOException, MoDSAgentException {
		for (String i : models.keySet()) {
			String modJson = new JSONObject().put("name", i)
					.put("details", collectDetails(models.get(i))).toString();
			JsonNode locatedNode = node.path("models").path("model");
			ArrayNode addedModelNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(modJson));	
		}
	}*/
	
	
	public JSONObject collectDetails(LinkedHashMap<String, String> details) throws IOException, MoDSAgentException {
		JSONObject detailsInJson = new JSONObject();
		List<JSONObject> detailArray = new ArrayList<>();
		for (String i : details.keySet()) {
			JSONObject detailJson = new JSONObject().put("name", i).put("content", details.get(i));
			detailArray.add(detailJson);
		}
		detailsInJson.put("detail", new JSONArray(detailArray));
		return detailsInJson;
	}
	
	// to be deleted 
	/**
	public List<JSONObject> collectCases(LinkedHashMap<String, List<String>> cases) {
		List<JSONObject> casesInJson = new ArrayList<>();
		for (String i : cases.keySet()) {
			JSONObject caseJson = new JSONObject().put("case", new JSONObject().put("name", i)
					.put("models", new JSONArray(collectModels(cases.get(i)))));
			casesInJson.add(caseJson);
		}
		return casesInJson;
	}*/
	
	public void collectCases(LinkedHashMap<String, List<String>> cases) throws IOException, MoDSAgentException {
		for (String i : cases.keySet()) {
			String caseJson = new JSONObject().put("name", i)
					.put("models", collectSimplifiedModels(cases.get(i))).toString();
			JsonNode locatedNode = modsJsonNode.path("cases").path("case");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(caseJson));
		}
	}
	
	public JSONObject collectSimplifiedModels(List<String> models) throws IOException, MoDSAgentException {
		JSONObject modelsInJson = new JSONObject();
		List<JSONObject> modelArray = new ArrayList<>();
		for (String model : models) {
			JSONObject modJson = new JSONObject().put("name", model);
			modelArray.add(modJson);
		}
		modelsInJson.put("model", new JSONArray(modelArray));
		return modelsInJson;
	}
	
	// to be deleted
	/**
	public List<JSONObject> collectFiles(HashMap<String, HashMap<String, String>> files) {
		List<JSONObject> filesInJson = new ArrayList<>();
		for (String i : files.keySet()) {
			JSONObject fileJson = new JSONObject().put("file", new JSONObject().put("name", i)
					.put("details", new JSONArray(collectDetails(files.get(i)))));
			filesInJson.add(fileJson);
		}
		return filesInJson;
	}*/
	
	public void collectFiles(LinkedHashMap<String, LinkedHashMap<String, String>> files) throws IOException, MoDSAgentException {
		for (String i : files.keySet() ) {
			String fileJson = new JSONObject().put("name", i)
					.put("details", collectDetails(files.get(i))).toString();
			JsonNode locatedNode = modsJsonNode.path("files").path("file");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(fileJson));
		}
	}
	
	
	// to be deleted
	/**
	public List<JSONObject> collectActiveParameters(HashMap<String, String> parameters, List<String> caseNameList, List<String> caseModelList) {
		List<JSONObject> parametersInJson = new ArrayList<>();
		for (String i : parameters.keySet()) {
			JSONObject paraJson = new JSONObject().put("parameter", new JSONObject().put("type", "active_input")
					.put("subtype", "subtype_".concat(i)).put("name", i).put("preserveWhiteSpace", "true")
					.put("scaling", "linear").put("cases", new JSONArray(collectCases(caseNameList)))
					.put("models", collectModels(caseModelList))
					.put("files", new JSONArray(collectActiveParameterFiles(i, parameters.get(i)))));
			parametersInJson.add(paraJson);
		}
		return parametersInJson;
	}*/
	
	
	public void collectFunctions(List<Function> functions) throws IOException, MoDSAgentException {
		for (Function function : functions) {
			String funcJson = new JSONObject()
					.put("name", function.getName())
					.put("usage", function.getUsage())
					.put("details", collectDetails(function.getDetailList())).toString();
			JsonNode locatedNode = modsJsonNode.path("functions").path("function");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(funcJson));
		}
	}
	
	public void collectParameters(List<Parameter> parameters) throws IOException, MoDSAgentException {
		for (Parameter param : parameters) {
			String paramJson = new JSONObject()
					.put("type", param.getType())
					.put("subtype", param.getSubtype())
					.put("name", param.getName())
					.put("caseDetailSep", param.getCaseDetailSep())
					.put("nParamsPerCase", param.getNParamsPerCase())
					.put("preserveWhiteSpace", param.getPreserveWhiteSpace())
					.put("scaling", param.getScaling())
					.put("cases", collectSimplifiedCases(param.getCaseNamesList()))
					.put("models", collectSimplifiedModels(param.getModelList()))
					.put("files", collectParameterFiles(param.getFileHash())).toString();
			JsonNode locatedNode = modsJsonNode.path("parameters").path("parameter");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(paramJson));
		}
	}
	
	public JSONObject collectSimplifiedCases(List<String> cases) throws IOException, MoDSAgentException {
		JSONObject casesInJson = new JSONObject();
		List<JSONObject> caseArray = new ArrayList<>();
		for (String cas : cases) {
			JSONObject casJson = new JSONObject().put("name", cas);
			caseArray.add(casJson);
		}
		casesInJson.put("case", new JSONArray(caseArray));
		return casesInJson;
	}
	
	public JSONObject collectParameterFiles(LinkedHashMap<String, LinkedHashMap<String, String>> files) 
			throws IOException, MoDSAgentException {
		JSONObject filesInJson = new JSONObject();
		for (String i : files.keySet()) {
			String tokens[] = i.split(" ");
			filesInJson.put(tokens[0], new JSONObject().put("name", tokens[1])
							.put("details", collectDetails(files.get(i))));
		}
		return filesInJson;
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	/**
	public List<JSONObject> collectActiveParameterFiles(String parameter, String parameterPath) {
		List<JSONObject> parameterFiles = new ArrayList<>();
		HashMap<String, String> initFileDetails = new HashMap<String, String>();
		initFileDetails.put("column", parameter);
		initFileDetails.put("row", "0");
		initFileDetails.put("read_function", "Get_DSV_double");
		initFileDetails.put("lb_abs", "1.0E-4");
		initFileDetails.put("ub_abs", "1000.0");
		HashMap<String, String> workFileDetails = new HashMap<String, String>();
		workFileDetails.put("path", parameterPath);
		workFileDetails.put("write_function", "Set_XML_double");
		
		JSONObject initFile = new JSONObject().put("initialRead", new JSONObject().put("name", "MODS_SIM_INITFILE_AIVarInitReadFile.csv")
				.put("details", new JSONArray(collectDetails(initFileDetails))));
		JSONObject workFile = new JSONObject().put("workingWrite", new JSONObject().put("name", "InputParams.xml")
				.put("details", new JSONArray(collectDetails(workFileDetails))));
		
		parameterFiles.add(initFile);
		parameterFiles.add(workFile);
		return parameterFiles;
	}
	
	public List<JSONObject> collectPassiveParameters(HashMap<String, String> parameters, List<String> caseNameList, List<String> caseModelList) {
		List<JSONObject> parametersInJson = new ArrayList<>();
		for (String i : parameters.keySet()) {
			JSONObject paraJson = new JSONObject().put("parameter", new JSONObject().put("type", "passive_input")
					.put("subtype", "subtype_".concat(i)).put("name", i).put("caseDetailSep", ";").put("preserveWhiteSpace", "true")
					.put("cases", new JSONArray(collectCases(caseNameList)))
					.put("models", collectModels(caseModelList))
					.put("files", new JSONArray(collectPassiveParameterFiles(i, parameters.get(i)))));
			parametersInJson.add(paraJson);
		}
		return parametersInJson;
	}
	
	public List<JSONObject> collectPassiveParameterFiles(String parameter, String parameterPath) {
		List<JSONObject> parameterFiles = new ArrayList<>();
		HashMap<String, String> initFileDetails = new HashMap<String, String>();
		initFileDetails.put("column", parameter);
		initFileDetails.put("row", "0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62");
		initFileDetails.put("read_function", "Get_DSV_double");
		HashMap<String, String> workFileDetails = new HashMap<String, String>();
		workFileDetails.put("path", parameterPath);
		workFileDetails.put("write_function", "Set_XML_double");
		
		JSONObject initFile = new JSONObject().put("initialRead", new JSONObject().put("name", "MODS_SIM_INITFILE_cases.csv")
				.put("details", new JSONArray(collectDetails(initFileDetails))));
		JSONObject workFile = new JSONObject().put("workingWrite", new JSONObject().put("name", "InputParams.xml")
				.put("details", new JSONArray(collectDetails(workFileDetails))));
		
		parameterFiles.add(initFile);
		parameterFiles.add(workFile);
		return parameterFiles;
	}
	
	public JSONObject collectOutputParameters(String outputResponse, List<String> caseNameList, List<String> caseModelList) {
		JSONObject outputResponseInJson = new JSONObject().put("parameter", new JSONObject().put("type", "active_output")
				.put("subtype", "subtype_".concat(outputResponse)).put("caseDetailSep", ";").put("nParamsPerCase", "1")
				.put("name", outputResponse).put("preserveWhiteSpace", "true")
				.put("schaling", "linear").put("cases", new JSONArray(collectCases(caseNameList)))
				.put("models", collectModels(caseModelList))
				.put("files", new JSONArray(collectOutputParameterFiles(outputResponse))));
//		for (String i : parameters.keySet()) {
//			JSONObject paraJson = new JSONObject().put("parameter", new JSONObject().put("type", "passive_input")
//					.put("subtype", "subtype_".concat(i)).put("name", i).put("caseDetailSep", ";").put("preserveWhiteSpaceTrue", "true")
//					.put("cases", new JSONArray(collectCases(caseNameList)))
//					.put("models", collectModels(caseModelList))
//					.put("files", new JSONArray(collectPassiveParameterFiles(i, parameters.get(i)))));
//			parametersInJson.add(paraJson);
//		}
		return outputResponseInJson;
	}
	
	public List<JSONObject> collectOutputParameterFiles(String outputResponse) {
		List<JSONObject> parameterFiles = new ArrayList<>();
		HashMap<String, String> initFileDetails = new HashMap<String, String>();
		initFileDetails.put("column", outputResponse);
		initFileDetails.put("row", "0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62");
		initFileDetails.put("read_function", "Get_DSV_double");
		initFileDetails.put("lb_factor", "0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8");
		initFileDetails.put("ub_factor", "1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2");
		initFileDetails.put("lb_addend", "-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0");
		initFileDetails.put("ub_addend", "0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0");
		HashMap<String, String> workFileDetails = new HashMap<String, String>();
		workFileDetails.put("column", "Ignition time [ms]");
		workFileDetails.put("row", "0");
		workFileDetails.put("read_function", "Get_DSV_double");
		
		JSONObject initFile = new JSONObject().put("initialRead", new JSONObject().put("name", "MODS_SIM_INITFILE_cases.csv")
				.put("details", new JSONArray(collectDetails(initFileDetails))));
		JSONObject workFile = new JSONObject().put("workingRead", new JSONObject().put("name", "OutputCase00001Cyc0001Info.csv")
				.put("details", new JSONArray(collectDetails(workFileDetails))));
		
		parameterFiles.add(initFile);
		parameterFiles.add(workFile);
		return parameterFiles;
	}
	
	public List<JSONObject> collectModels(List<String> models) {
		List<JSONObject> modelsInJson = new ArrayList<>();
		for (String i : models) {
			JSONObject modJson = new JSONObject().put("model", new JSONObject().put("ref", i));
			modelsInJson.add(modJson);
		}
		return modelsInJson;
	}
	
	public List<JSONObject> collectCases(List<String> cases) {
		List<JSONObject> casesInJson = new ArrayList<>();
		for (String i : cases) {
			JSONObject caseJson = new JSONObject().put("case", new JSONObject().put("ref", i));
			casesInJson.add(caseJson);
		}
		return casesInJson;
	}
	
	*/
	// to be deleted
	/**
	public List<JSONObject> collectDetails(HashMap<String, String> details) {
		List<JSONObject> detailsInJson = new ArrayList<>();
		for (String i : details.keySet()) {
			JSONObject detailJson = new JSONObject().put("detail",
					new JSONObject().put("name", i).put("content", details.get(i)));
			detailsInJson.add(detailJson);
		}
		return detailsInJson;
	}

	public void formMoDSInputsFile(JsonNode jsonNode, String filePath) throws MoDSAgentException {

//		caseNameList.add("PODE3_Figure%2D5_Parent%20Case");
//		for (int i = 1; i < 62; i++) {
//			caseNameList.add("PODE3_Figure%2D5_case_" + i);
//		}

		initMoDSInputs = new InitMoDSInputs();
		initMoDSInputs.init();

		// setUpMoDSInputs (the idea is to call a lower layer)
		JsonNode modsJN = jsonNode.get("mods");
		setUpMoDSInputs(modsJN);

		saveMoDSInputsContent(filePath);
		
		cleanUp(filePath);
//		return filePath;
	}

	public void setUpMoDSInputs(JsonNode modsJN) throws MoDSAgentException {
		mods.setXmlns("http://como.cheng.cam.ac.uk/MoDS");
		mods.setXmlnsXsi("http://www.w3.org/2001/XMLSchema-instance");
		mods.setXsiSchemaLocation("http://como.cheng.cam.ac.uk/MoDS MoDS_inputs.xsd");
		
		System.out.println(modsJN);

		if (modsJN.get("algorithms") != null) {
			setUpAlgorithmS(modsJN.get("algorithms"));
			mods.setAlgorithmS(algorithmS);
		}

		if (modsJN.get("models") != null) {
			setUpModelS(modsJN.get("models"));
			mods.setModelS(modelS);
		}

		if (modsJN.get("cases") != null) {
			setUpCaseS(modsJN.get("cases"));
			mods.setCaseS(caseS);
		}

		if (modsJN.get("files") != null) {
//			System.out.println("files done");
			setUpFileS(modsJN.get("files"));
			mods.setFileS(fileS);
		}

		if (modsJN.get("functions") != null) {
//			System.out.println("functions done");
			setUpFunctionS(modsJN.get("functions"));
			mods.setFunctionS(functionS);
		}
		
		if (modsJN.get("parameters") != null) {
//			System.out.println("parameters done");
			setUpParameterS(modsJN.get("parameters"));
			mods.setParameterS(parameterS);
		}
	}

	private void setUpAlgorithmS(JsonNode algorithmsJN) throws MoDSAgentException {
		algorithmS = new AlgorithmS();
		algorithmList = new ArrayList<Algorithm>();

		for (int i = 0; i < algorithmsJN.size(); i++) {
			if (algorithmsJN.get(i).get("algorithm") != null) {
				setUpAlgorithm(algorithmsJN.get(i).get("algorithm"));
			}
		}

		algorithmS.setAlgorithm(algorithmList);
	}

	private void setUpModelS(JsonNode modelsJN) throws MoDSAgentException {
		modelS = new ModelS();
		modelList = new ArrayList<Model>();

		for (int i = 0; i < modelsJN.size(); i++) {
			if (modelsJN.get(i).get("model") != null) {
				setUpModel(modelsJN.get(i).get("model"));
			}
		}

		modelS.setModel(modelList);
	}
	
	private void setUpCaseS(JsonNode casesJN) throws MoDSAgentException {
		caseS = new CaseS();
		caseList = new ArrayList<Case>();
		
		for (int i = 0; i < casesJN.size(); i++) {
			if (casesJN.get(i).get("case") != null) {
				setUpCase(casesJN.get(i).get("case"));
			}
		}

		caseS.setCase(caseList);
	}
	
	private void setUpFileS(JsonNode filesJN) throws MoDSAgentException {
		fileS = new FileS();
		fileList = new ArrayList<com.cmclinnovations.jps.agent.file_management.mods.files.File>();
		
		for (int i = 0; i < filesJN.size(); i++) {
			if (filesJN.get(i).get("file") != null) {
				setUpFile(filesJN.get(i).get("file"));
			} else if (filesJN.get(i).get("initialRead") != null) {
				setUpInitialRead(filesJN.get(i).get("initialRead"));
				fileS.setInitialRead(initialRead);
			} else if (filesJN.get(i).get("workingRead") != null) {
				setUpWorkingRead(filesJN.get(i).get("workingRead"));
				fileS.setWorkingRead(workingRead);
			} else if (filesJN.get(i).get("workingWrite") != null) {
				setUpWorkingWrite(filesJN.get(i).get("workingWrite"));
				fileS.setWorkingWrite(workingWrite);
			}
		}
		
//		if (filesJN.get(i).get("file") != null) {
//			setUpFile(filesJN.get(i).get("file"));
//		}
		
		fileS.setFile(fileList);
	}
	
	private void setUpFunctionS(JsonNode functionsJN) throws MoDSAgentException {
		functionS = new FunctionS();
		functionList = new ArrayList<Function>();
		
		for (int i = 0; i < functionsJN.size(); i++) {
			if (functionsJN.get(i).get("function") != null) {
				setUpFunction(functionsJN.get(i).get("function"));
			}
		}

		functionS.setFunction(functionList);
	}
	
	private void setUpParameterS(JsonNode parametersJN) throws MoDSAgentException {
		parameterS = new ParameterS();
		parameterList = new ArrayList<Parameter>();
		
		for (int i = 0; i < parametersJN.size(); i++) {
			if (parametersJN.get(i).get("parameter") != null) {
				setUpParameter(parametersJN.get(i).get("parameter"));
			} else if (parametersJN.get(i).get("version") != null) {
				parameterS.setVersion(parametersJN.get(i).get("version").textValue());
			}
		}

		parameterS.setParameter(parameterList);
	}
	
	private void setUpAlgorithm(JsonNode algoJN) throws MoDSAgentException {
		algorithm = new Algorithm();

		if (algoJN.get("name") != null) {
			algorithm.setName(algoJN.get("name").textValue());
		}

		if (algoJN.get("displayName") != null) {
			algorithm.setDisplayName(algoJN.get("displayName").textValue());
		}

		if (algoJN.get("details") != null) {
			setUpDetailS(algoJN.get("details"));
			algorithm.setDetailS(detailS);
		}

		algorithmList.add(algorithm);
	}

	private void setUpModel(JsonNode modJN) throws MoDSAgentException {
		model = new Model();

		if (modJN.get("name") != null) {
			model.setName(modJN.get("name").textValue());
		}

		List<Object> items = new ArrayList<>();
		if (modJN.get("ref") != null) {
			items.add(modJN.get("ref").textValue());
		}

		if (modJN.get("details") != null) {
			setUpDetailS(modJN.get("details"));
			items.add(detailS);
		}
		
//		model.setItems(items);
		modelList.add(model);
	}
	
	private void setUpCase(JsonNode caseJN) throws MoDSAgentException {
		caseA = new Case();
		
		if (caseJN.get("name") != null) {
			caseA.setName(caseJN.get("name").textValue());
		}
		
		List<Object> items = new ArrayList<>();
		if (caseJN.get("ref") != null) {
			items.add(caseJN.get("ref").textValue());
		}

		if (caseJN.get("models") != null) {
			setUpModelS(caseJN.get("models"));
			items.add(modelS);
		}
		
//		caseA.setItems(items);
		caseList.add(caseA);
	}
	
	private void setUpFile(JsonNode fileJN) throws MoDSAgentException {
		file = new com.cmclinnovations.jps.agent.file_management.mods.files.File();
		
		if (fileJN.get("name") != null) {
			file.setName(fileJN.get("name").textValue());
		}
		
		if (fileJN.get("details") != null) {
			setUpDetailS(fileJN.get("details"));
			file.setDetailS(detailS);
		}
		
		fileList.add(file);
	}
	
	private void setUpInitialRead(JsonNode initReadJN) throws MoDSAgentException {
		initialRead = new InitialRead();
		
		if (initReadJN.get("name") != null) {
			initialRead.setFileName(initReadJN.get("name").textValue());
		}
		
		if (initReadJN.get("details") != null) {
			setUpDetailS(initReadJN.get("details"));
			initialRead.setDetailS(detailS);
		}
	}
	
	private void setUpWorkingRead(JsonNode workReadJN) throws MoDSAgentException {
		workingRead = new WorkingRead();
		
		if (workReadJN.get("name") != null) {
			workingRead.setFileName(workReadJN.get("name").textValue());
		}
		
		if (workReadJN.get("details") != null) {
			setUpDetailS(workReadJN.get("details"));
			workingRead.setDetailS(detailS);
		}
	}
	
	private void setUpWorkingWrite(JsonNode workWriteJN) throws MoDSAgentException {
		workingWrite = new WorkingWrite();
		
		if (workWriteJN.get("name") != null) {
			workingWrite.setFileName(workWriteJN.get("name").textValue());
		}
		
		if (workWriteJN.get("details") != null) {
			setUpDetailS(workWriteJN.get("details"));
			workingWrite.setDetailS(detailS);
		}
	}
	
	private void setUpFunction(JsonNode funcJN) throws MoDSAgentException {
		function = new Function();
		
		if (funcJN.get("name") != null) {
			function.setName(funcJN.get("name").textValue());
		}
		
		if (funcJN.get("usage") != null) {
			function.setName(funcJN.get("usage").textValue());
		}
		
		if (funcJN.get("details") != null) {
			setUpDetailS(funcJN.get("details"));
			function.setDetailS(detailS);
		}
		
		functionList.add(function);
	}
	
	private void setUpParameter(JsonNode paraJN) throws MoDSAgentException {
		parameter = new Parameter();
		
		if (paraJN.get("type") != null) {
			parameter.setType(paraJN.get("type").textValue());
		}
		
		if (paraJN.get("subtype") != null) {
			parameter.setSubtype(paraJN.get("subtype").textValue());
		}
		
		if (paraJN.get("caseDetailSep") != null) {
			parameter.setCaseDetailSep(paraJN.get("caseDetailSep").textValue());
		}
		
		if (paraJN.get("detailSep") != null) {
			parameter.setDetailSep(paraJN.get("detailSep").textValue());
		}
		
		if (paraJN.get("nParamsPerCase") != null) {
			parameter.setNParamsPerCase(paraJN.get("nParamsPerCase").textValue());
		}
		
		if (paraJN.get("name") != null) {
			parameter.setName(paraJN.get("name").textValue());
		}
		
		if (paraJN.get("nameSuffixDetail") != null) {
			parameter.setNameSufficDetail(paraJN.get("nameSuffixDetail").textValue());
		}
		
		if (paraJN.get("perspectiveWhiteSpace") != null) {
			parameter.setPerspectiveWhiteSpace(paraJN.get("perspectiveWhiteSpace").textValue());
		}
		
		if (paraJN.get("scaling") != null) {
			parameter.setScaling(paraJN.get("scaling").textValue());
		}
		
		if (paraJN.get("cases") != null) {
			setUpParameterCaseS(paraJN.get("cases"));
//			parameter.setCases(parameterCaseS);
		}
		
		if (paraJN.get("models") != null) {
			setUpParameterModelS(paraJN.get("models"));
//			parameter.setModels(parameterModelS);
		}
		
		if (paraJN.get("files") != null) {
			setUpParameterFileS(paraJN.get("files"));
			parameter.setFiles(parameterFileS);
		}
		parameterList.add(parameter);
	}
	
	private void setUpParameterCaseS(JsonNode parameterCasesJN) throws MoDSAgentException {
		parameterCaseS = new CaseS();
		parameterCaseList = new ArrayList<Case>();
		
		for (int i = 0; i < parameterCasesJN.size(); i++) {
			if (parameterCasesJN.get(i).get("case") != null) {
				setUpParameterCase(parameterCasesJN.get(i).get("case"));
			}
		}

		parameterCaseS.setCase(parameterCaseList);
	}
	
	private void setUpParameterModelS(JsonNode parameterModelsJN) throws MoDSAgentException {
		parameterModelS = new ModelS();
		parameterModelList = new ArrayList<Model>();

		for (int i = 0; i < parameterModelsJN.size(); i++) {
			if (parameterModelsJN.get(i).get("model") != null) {
				setUpParameterModel(parameterModelsJN.get(i).get("model"));
			}
		}

		parameterModelS.setModel(parameterModelList);
	}
	
	private void setUpParameterFileS(JsonNode parameterFilesJN) throws MoDSAgentException {
		parameterFileS = new FileS();
		parameterFileList = new ArrayList<com.cmclinnovations.jps.agent.file_management.mods.files.File>();
		
		for (int i = 0; i < parameterFilesJN.size(); i++) {
			if (parameterFilesJN.get(i).get("file") != null) {
				setUpParameterFile(parameterFilesJN.get(i).get("file"));
			} else if (parameterFilesJN.get(i).get("initialRead") != null) {
				setUpInitialRead(parameterFilesJN.get(i).get("initialRead"));
				parameterFileS.setInitialRead(initialRead);
			} else if (parameterFilesJN.get(i).get("workingRead") != null) {
				setUpWorkingRead(parameterFilesJN.get(i).get("workingRead"));
				parameterFileS.setWorkingRead(workingRead);
			} else if (parameterFilesJN.get(i).get("workingWrite") != null) {
				setUpWorkingWrite(parameterFilesJN.get(i).get("workingWrite"));
				parameterFileS.setWorkingWrite(workingWrite);
			}
		}
		
//		if (filesJN.get(i).get("file") != null) {
//			setUpFile(filesJN.get(i).get("file"));
//		}
		
		parameterFileS.setFile(parameterFileList);
	}
	
	private void setUpParameterModel(JsonNode paraModJN) throws MoDSAgentException {
		parameterModel = new Model();

		if (paraModJN.get("name") != null) {
			parameterModel.setName(paraModJN.get("name").textValue());
		}

		List<Object> items = new ArrayList<>();
		if (paraModJN.get("ref") != null) {
			items.add(paraModJN.get("ref").textValue());
		}

		if (paraModJN.get("details") != null) {
			setUpDetailS(paraModJN.get("details"));
			items.add(detailS);
		}
		
//		parameterModel.setItems(items);
		parameterModelList.add(parameterModel);
	}
	
	private void setUpParameterCase(JsonNode paraCaseJN) throws MoDSAgentException {
		parameterCase = new Case();
		
		if (paraCaseJN.get("name") != null) {
			parameterCase.setName(paraCaseJN.get("name").textValue());
		}
		
		List<Object> items = new ArrayList<>();
		if (paraCaseJN.get("ref") != null) {
			items.add(paraCaseJN.get("ref").textValue());
		}

		if (paraCaseJN.get("models") != null) {
			setUpModelS(paraCaseJN.get("models"));
			items.add(modelS);
		}
		
//		parameterCase.setItems(items);
		parameterCaseList.add(parameterCase);
	}
	
	private void setUpParameterFile(JsonNode paraFileJN) throws MoDSAgentException {
		parameterFile = new com.cmclinnovations.jps.agent.file_management.mods.files.File();
		
		if (paraFileJN.get("name") != null) {
			parameterFile.setName(paraFileJN.get("name").textValue());
		}
		
		if (paraFileJN.get("details") != null) {
			setUpDetailS(paraFileJN.get("details"));
			parameterFile.setDetailS(detailS);
		}
		
		parameterFileList.add(parameterFile);
	}

	private void setUpDetailS(JsonNode detailsJN) throws MoDSAgentException {
		detailS = new DetailS();
		detailList = new ArrayList<Detail>();

		for (int i = 0; i < detailsJN.size(); i++) {
			if (detailsJN.get(i).get("detail") != null) {
				setUpDetail(detailsJN.get(i).get("detail"));
			}
		}

		detailS.setDetail(detailList);
	}

	private void setUpDetail(JsonNode detJN) throws MoDSAgentException {
		detail = new Detail();

		if (detJN.get("name") != null) {
			detail.setName(detJN.get("name").textValue());
		}

		if (detJN.get("content") != null) {
			detail.setContent(detJN.get("content").textValue());
		}

		detailList.add(detail);
	}

	*/
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	/**
	private void setUpAlgorithmS(String jsonString) throws MoDSAgentException {
		algorithmS = new AlgorithmS();
		algorithmList = new ArrayList<Algorithm>();

//		JSONArray arr = obj.getJSONArray("posts");
//        for (int i = 0; i < arr.length(); i++) {
//            String post_id = arr.getJSONObject(i).getString("post_id");
//            System.out.println(post_id);
//        }

//        JSONArray arr = (new JSONObject(jsonString)).getJSONArray("")
//		System.out.println(jsonString);

		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String listOfAlgorithm = JsonPath.read(jsonString, "$.mods.algorithms").toString();
//		listOfAlgorithm.replace("\"", "\\\"");
		System.out.println(listOfAlgorithm);

		try {
			JsonNode testAlgoS = objectMapper.readTree(listOfAlgorithm);
			for (int i = 0; i < testAlgoS.size(); i++) {
				System.out.println(testAlgoS.get(i));
				setUpAlgorithm(testAlgoS.get(i));
			}
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		/*
		 * try { // MoDS mdos = objectMapper.readValue(jsonString, MoDS.class); //
		 * System.out.println(mdos.getAlgorithmS());
		 * 
		 * // JsonNode testAlgoS = objectMapper.readTree(listOfAlgorithm); //
		 * System.out.println(testAlgoS); //
		 * System.out.println(testAlgoS.get(0).get("algorithm")); //
		 * System.out.println("test 1"); //
		 * 
		 * Algorithm[] algorithmTestArray = objectMapper.readValue(listOfAlgorithm,
		 * Algorithm[].class); System.out.println(algorithmTestArray[0].getName());
		 * System.out.println(algorithmTestArray[0].getDisplayName());
		 * System.out.println(algorithmTestArray[0].getDetailS());
		 * System.out.println("Finish test.");
		 * 
		 * 
		 * algorithmList = objectMapper.readValue(listOfAlgorithm, new
		 * TypeReference<List<Algorithm>>() {}); System.out.println(algorithmList);
		 * 
		 * 
		 * 
		 * for (Algorithm singleAlgorithm : algorithmList) {
		 * System.out.println(singleAlgorithm.getName());
		 * System.out.println(singleAlgorithm.getDetailS()); }
		 * 
		 * 
		 * } catch (IOException e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); }
		 */

//		
//		Configuration cf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
//	    DocumentContext ctx = JsonPath.using(cf).parse(jsonString);

//	    int i = 0;
//	    
//	    List<String> listOfAlgorithmNames = ctx.read("$.mods.algorithms.algorithm[*].name");
////	    List<String> listOfAlgorithmDetails = ctx.read("$.mods.algorithms.algorithm[1].details");
//	    String test1 = ctx.read("$.mods.algorithms.algorithm[1].details.detail[0]");
//	    System.out.println(test1);
//	    
//	    String test0 = ctx.read("$.mods.algorithms.algorithm[0].details.detail[*]");
//	    System.out.println(test0);
//	    
//	    String jsonCarArray = 
//	    		  "[{ \"color\" : \"Black\", \"type\" : \"BMW\" }, { \"color\" : \"Red\", \"type\" : \"FIAT\" }]";
//	    		List<Car> listCar = objectMapper.readValue(jsonCarArray, new TypeReference<List<Car>>(){});
//	    

//		JSONArray listOfAlgorithm = JsonPath.read(jsonString, "$.mods.algorithms[*]");
//		System.out.println(listOfAlgorithmDetails);
//	    System.out.println(listOfAlgorithmDetails.get(0));
//		System.out.println(listOfAlgorithmDetails.get(1));

//		for (String singleAlgorithm : listOfAlgorithm) {
//			setUpAlgorithm(singleAlgorithm.toString());
//		}
	
	/**
		algorithmS.setAlgorithm(algorithmList);
		mods.setAlgorithmS(algorithmS);
	}*/

	private void setUpDetailS(String jsonString) throws MoDSAgentException {
		detailS = new DetailS();
		detailList = new ArrayList<Detail>();

		List<JSONObject> listOfDetail = JsonPath.read(jsonString, "$.details.detail");
		for (JSONObject singleDetail : listOfDetail) {
			String name = JsonPath.read(singleDetail, "$.detail.name");
			String content = JsonPath.read(singleDetail, "$.detail.content");
			setUpDetail(name, content);
		}
		detailS.setDetail(detailList);
		algorithm.setDetailS(detailS);
	}

	private void setUpDetail(String name, String content) throws MoDSAgentException {
		detail = new Detail();
		detail.setName(name);
		detail.setContent(content);
		detailList.add(detail);
	}

	private void saveMoDSInputsContent(String filePath) throws MoDSAgentException {
		FileWriterWithEncoding file;
		try {
			file = new FileWriterWithEncoding(filePath, "UTF-8");

			JAXBContext jaxbContext = JAXBContext.newInstance(MoDS.class);
			Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
			jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			jaxbMarshaller.marshal(mods, file);
			file.close();
		} catch (JAXBException e) {
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void cleanUp(String filePath) throws IOException, MoDSAgentException {
		String fileTemp = filePath.replace(".xml", "_temp.xml");
		String fileOrig = filePath;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(fileOrig), "UTF-8"));
			BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileTemp), "UTF-8"));
			String line;
			while ((line = br.readLine()) != null) {
				if (line.contains(INITIALISATION_STRING)) {
				} else {
					bw.write(line.concat("\n"));
				}
			}
			bw.close();
			br.close();
			delete(fileOrig, fileTemp);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void delete(String xmlFileOrig, String xmlFileTemp) throws IOException, MoDSAgentException {
		File fileOriginal = new File(xmlFileOrig);
		if (fileOriginal.delete()) {
			fileOriginal = new File(xmlFileOrig);
			File fileTemp = new File(xmlFileTemp);
			if (fileTemp.renameTo(fileOriginal)) {
			} else {
				logger.error("The temporary MoDS_inputs.xml file could not be renamed.");
			}
		} else {
			logger.error("The generated original MoDS_inputs.xml file could not be deleted.");
		}
	}
	
	
	
	
	public void initialise(String jobFolderName) throws IOException, MoDSAgentException {
		init();
		
		// create the job folder 
		jobFolderPath = FOLDER_ROOT.concat(FRONTSLASH).concat(FOLDER_DOCUMENTS)
				.concat(FRONTSLASH).concat(FOLDER_JOB_FOLDER)
				.concat(FRONTSLASH).concat(jobFolderName);
		File jobFolder = new File(jobFolderPath);
		if (!jobFolder.exists()) {
			jobFolder.mkdir();
		}
		// create the \Temporary folder for file storage
		folderTemporaryPath = jobFolderPath.concat(FRONTSLASH).concat(FOLDER_TEMPORARY);
		File temporary = new File(folderTemporaryPath);
		if (!temporary.exists()) {
			temporary.mkdir();
		}
		// create the \Initial folder
		folderInitialPath = jobFolderPath.concat(FRONTSLASH).concat(FOLDER_INITIAL);
		File initial = new File(folderInitialPath);
		if (!initial.exists()) {
			initial.mkdir();
		}
		// create the \All folder
		folderAllPath = jobFolderPath.concat(FRONTSLASH).concat(FOLDER_ALL);
		File all = new File(folderAllPath);
		if (!all.exists()) {
			all.mkdir();
		}
		// create the \Working_dir folder
		folderWorkingDirPath = jobFolderPath.concat(FRONTSLASH).concat(FOLDER_WORKING_DIR);
		File workingDir = new File(folderWorkingDirPath);
		if (!workingDir.exists()) {
			workingDir.mkdir();
		}
		
		// create algorithms node
		((ObjectNode) modsJsonNode).set("algorithms", new ObjectMapper().readTree(INITIALISATION_STRING_ALGORITHMS));
		// create models node
		((ObjectNode) modsJsonNode).set("models", new ObjectMapper().readTree(INITIALISATION_STRING_MODELS));
		// create cases node
		((ObjectNode) modsJsonNode).set("cases", new ObjectMapper().readTree(INITIALISATION_STRING_CASES));
		// create files node
		((ObjectNode) modsJsonNode).set("files", new ObjectMapper().readTree(INITIALISATION_STRING_FILES));
		// create functions node
		((ObjectNode) modsJsonNode).set("functions", new ObjectMapper().readTree(INITIALISATION_STRING_FUNCTIONS));
		// create parameters node
		((ObjectNode) modsJsonNode).set("parameters", new ObjectMapper().readTree(INITIALISATION_STRING_PARAMETERS));
		
		
		
	}

	@Override
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		ModelKineticsSRM kineticsSRM = new ModelKineticsSRM();
		ExecutableModel exeModel = kineticsSRM.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		kineticsSRM.formFiles(exeModel);
		kineticsSRM.setUpMoDS();
	}

	@Override
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		ModelCanteraLFS canteraLFS = new ModelCanteraLFS();
		ExecutableModel exeModel = canteraLFS.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		canteraLFS.formFiles(exeModel);
		canteraLFS.setUpMoDS();
	}

	@Override
	public String marshall() throws IOException, MoDSAgentException {
		// TODO Auto-generated method stub
		
		
		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true);
		
		System.out.println(objectMapper.writeValueAsString(modsJsonNode));
		
		mods = objectMapper.readValue(objectMapper.writeValueAsString(modsJsonNode), MoDS.class);
		mods.setXmlns("http://como.cheng.cam.ac.uk/MoDS");
		mods.setXmlnsXsi("http://www.w3.org/2001/XMLSchema-instance");
		mods.setXsiSchemaLocation("http://como.cheng.cam.ac.uk/MoDS MoDS_inputs.xsd");
		saveMoDSInputsContent(folderWorkingDirPath.concat(FRONTSLASH+FILE_MODS_INPUTS));
		cleanUp(folderWorkingDirPath.concat(FRONTSLASH+FILE_MODS_INPUTS));
		deleteDirectory(new File(folderTemporaryPath));
		
		return jobFolderPath;
	}
	
	private void init() {
		initMoDSInputs = new InitMoDSInputs();
		initMoDSInputs.init();
	}
	
	private void deleteDirectory(File directoryToBeDeleted) {
	    File[] allContents = directoryToBeDeleted.listFiles();
	    if (allContents != null) {
	        for (File file : allContents) {
	            deleteDirectory(file);
	        }
	    }
	    directoryToBeDeleted.delete();
	}
	
	
	/**
	 * Convert a string array to a string in the format of CSV file. 
	 * 
	 * @param data
	 * @return
	 */
	public String convertToCSV(String[] data) {
	    return Stream.of(data)
	      .map(this::escapeSpecialCharacters)
	      .collect(Collectors.joining(","));
	}
	
	/**
	 * Escape special characters when converting string array to string in the format of CSV file. 
	 * 
	 * @param data
	 * @return
	 */
	public String escapeSpecialCharacters(String data) {
	    String escapedData = data.replaceAll("\\R", " ");
	    if (data.contains(",") || data.contains("\"") || data.contains("'")) {
	        data = data.replace("\"", "\"\"");
	        escapedData = "\"" + data + "\"";
	    }
	    return escapedData;
	}
	
	/**
	 * Check if the given folder path exist, create one if it does not exist. 
	 * 
	 * @param folderPath
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	public void checkFolderPath(String folderPath) throws IOException, MoDSAgentException {
		File folder = new File(folderPath);
		if (!folder.exists()) {
			folder.mkdir();
		}
	}
	
//	public List<JSONObject> collectModel(LinkedHashMap<String, LinkedHashMap<String, String>> model) throws IOException, MoDSAgentException {
//		// setup model 
//		
//		
//		String modelNodeString = new JSONObject().put("name", modelName)
//				.put("details", new JSONObject().put("detail", value));
//		JsonNode locatedNode = modsJsonNode.path("models").path("model");
//		ArrayNode addedModelNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree("{}"))
//		
//		
//		List<JSONObject> modelsInJson = new ArrayList<>();
//		for (String i : models.keySet()) {
//			JSONObject modJson = new JSONObject().put("model", new JSONObject().put("name", i)
//					.put("details", new JSONArray(collectDetails(models.get(i)))));
//			modelsInJson.add(modJson);
//		}
//		return modelsInJson;
//	}
	
	
}
