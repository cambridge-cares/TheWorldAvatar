package com.cmclinnovations.jps.agent.file_management.marshallr;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactoryConfigurationError;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

import com.cmclinnovations.jps.agent.file_management.MoDSInputsState;
import com.cmclinnovations.jps.agent.file_management.mods.models.Model;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.Parameter;
import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;
import com.cmclinnovations.jps.agent.mechanism.calibration.Property;
import com.cmclinnovations.jps.kg.OntoChemExpKG;
import com.cmclinnovations.jps.kg.OntoChemExpKG.DataTable;
import com.cmclinnovations.jps.kg.OntoKinKG;

public class ModelKineticsSRM extends MoDSMarshaller implements IModel {
	private static Logger logger = LoggerFactory.getLogger(ModelKineticsSRM.class);
	private int numOfReactions;
	private String modelName = new String();
	private LinkedHashMap<String, String> activeParameters = new LinkedHashMap<String, String>(); // linkedHashMap? 
	private List<String> passiveParameters = new ArrayList<>();
	private List<String> outputResponses = new ArrayList<>();
	private List<String> expFiles = new ArrayList<>();
	private List<String> modelFiles = new ArrayList<>();
	private List<String> caseNames = new ArrayList<>();
	
	/**
	 * Collect all information required by MoDS to execute the model kineticsSRM. 
	 * The information required: 
	 * 1. model name - the name of the executable;
	 * 2. active parameters - the optimisable parameters of the model;
	 * 3. passive parameters - the experiment settings;
	 * 4. output response - the experimental observations;
	 * 5. exp files - list of files that contains experiment data and model to be calibrated;
	 * 6. case names - list of individual experiment;
	 * Input to this function: 
	 * 1. experimentIRI - get all experimental observations, i.e., passive parameters and output responses. 
	 * Also, set up case name list
	 * 2. mechanismIRI - download mechanism file
	 * 3. reactionIRIList - get list of reactions to be optimised, i.e., active parameters
	 * 
	 * @param experimentIRI
	 * @param mechanismIRI
	 * @param reactionIRIList
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	@Override
	public ExecutableModel formExecutableModel(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList) 
			throws IOException, MoDSAgentException {
		// check if the target folder exist
		checkFolderPath(folderTemporaryPath);
		
		// create ontology kg instance for query
		OntoKinKG ontoKinKG = new OntoKinKG();
		// query active parameters
		LinkedHashMap<String, String> activeParameters = ontoKinKG.queryReactionsToOptimise(mechanismIRI, reactionIRIList);
		// collect experiment information
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
		// form exp data csv file
		List<String[]> dataLines;
		dataLines = new ArrayList<>();
		dataLines.add(headers.get(0).toArray(new String[0]));
		List<String> caseList = new ArrayList<>();
		int i = 0;
		for (List<String> dataSingleLine : dataCollection) {
			dataLines.add(dataSingleLine.toArray(new String[0]));
			// generate the list of cases
			caseList.add(Property.MODEL_KINETICS.getPropertyName().concat("_case_"+i));
			i += 1;
		}
		File expDataCSV = new File(folderTemporaryPath.concat(FRONTSLASH).concat(Property.MODEL_KINETICS.getPropertyName().concat(UNDERSCORE+FILE_MODEL_EXPDATA_SUFFIX)));
		try (PrintWriter pw = new PrintWriter(expDataCSV)) {
			dataLines.stream()
			.map(this::convertToCSV)
			.forEach(pw::println);
		}
		
		// obtain passive parameters and output responses from header of exp data csv file
		List<String> passiveParameters = new ArrayList<>();
		List<String> outputResponses = new ArrayList<>();
		for (String param : headers.get(0)) {
			if (param.contains("Igni") && param.contains("Delay")) {
				outputResponses.add(param);
			} else if (param.contains("Unit")) {
			} else if (param.contains("Phi")) {
			} else {
				passiveParameters.add(param);
			}
		}
		
		// download mechanism owl file, this part should be modified to do the conversion of OWL to XML on the fly
		String mechanismXML = folderTemporaryPath.concat(FRONTSLASH).concat(FILE_MECHANISM);
		MechanismDownload mechanismDownload = new MechanismDownload();
		try {
			String mechanismWebPath = mechanismIRI.substring(0, mechanismIRI.indexOf("#"))
					.replace("/kb/", "/data/").replace(".owl", "/mechanism.xml");
			mechanismDownload.obtainMechanism(mechanismWebPath, mechanismXML);
		} catch (SAXException | ParserConfigurationException | TransformerFactoryConfigurationError
				| TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		// create model instance
		ExecutableModel kineticsSRM = new ExecutableModel();
		
		// set up model name
		kineticsSRM.setModelName(Property.MODEL_KINETICS.getPropertyName());
		
		// set up model active parameters
		kineticsSRM.setActiveParameters(activeParameters);
		
		// set up model passive parameters
		kineticsSRM.setPassiveParameters(passiveParameters);
		
		// set up model output response
		kineticsSRM.setOutputResponses(outputResponses);
		
		// set up model exp files
		List<String> expFiles = new ArrayList<>();
		expFiles.add(expDataCSV.getName());
		expFiles.add(mechanismXML.substring(mechanismXML.lastIndexOf(FRONTSLASH)));
		kineticsSRM.setExpFiles(expFiles);
		// set up model case names
		kineticsSRM.setCaseNames(caseList);
		
		// query number of reactions
		List<List<String>> numOfReactionsResults = ontoKinKG.queryNumOfReactions(mechanismIRI);
		numOfReactions = Integer.parseInt(numOfReactionsResults.get(1).get(0));
		
		return kineticsSRM;
	}
	
	/**
	 * Form all files required by MoDS to execute the model kineticsSRM. 
	 * 
	 * @param exeModel
	 * @param jobFolderPath
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	@Override
	public List<String> formFiles(ExecutableModel exeModel) throws IOException, MoDSAgentException {
		// check if the target folder exist
		checkFolderPath(folderInitialPath);
		checkFolderPath(folderAllPath);
		
		// get the basic information of executable kineticsSRM
		modelName = exeModel.getModelName();
		activeParameters = exeModel.getActiveParameters();
		expFiles = exeModel.getExpFiles();
		caseNames = exeModel.getCaseNames();
		outputResponses = exeModel.getOutputResponses();
		passiveParameters = exeModel.getPassiveParameters();
		
		// process the active parameters to be only the equation of reactions
		List<String> processedActiveParam = new ArrayList<>();
		for (String activeParamNo : activeParameters.keySet()) {
			processedActiveParam.add(activeParameters.get(activeParamNo));
		}
		
		// create list to store all files used/produced when executing kineticsSRM model
		// get the name of files in the initial folder
		List<String> folderInitialFiles = createFolderInitial(processedActiveParam);
		// get the name of files in the all folder
		List<String> folderAllFiles = createFolderAll(processedActiveParam);
		// name the output file of the model
		String outputFile = Property.MODEL_KINETICS_OUTPUT.getPropertyName();
		// append all names to modelFiles
		modelFiles.addAll(folderInitialFiles);
		modelFiles.addAll(folderAllFiles);
		modelFiles.add(outputFile);
		
		return modelFiles;
	}
	
	/**
	 * Create the files used during the 'initial read' when executing the kineticsSRM model. 
	 * 
	 * @param initial
	 * @param modelName
	 * @param processedActiveParam
	 * @param caseNames
	 * @param expFiles
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	@Override
	public List<String> createFolderInitial(List<String> processedActiveParam) throws IOException, MoDSAgentException {
		// set the active parameter csv file and passive parameter csv file path
		File activeParameterFilePath = new File(folderInitialPath
				.concat(FRONTSLASH+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_ACTIVE_SUFFIX));
		File passiveParametersAndOutputsFilePath = new File(folderInitialPath
				.concat(FRONTSLASH+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_PASSIVE_SUFFIX));
				
		// get the filePath of experimental data
		File expData = null;
		for (String expFilePath : expFiles) {
			if (expFilePath.contains(FILE_MODEL_EXPDATA_SUFFIX)) {
				expData = new File(folderTemporaryPath.concat(FRONTSLASH+expFilePath));
			}
		}
		
		// create files in the initial folder
		List<String> initialFiles = new ArrayList<>();
		String initialActiveFile = createActiveParametersFile(activeParameterFilePath, processedActiveParam, expData, caseNames);
		String initialPassiveFile = createPassiveParametersAndOutputsFile(passiveParametersAndOutputsFilePath, expData, caseNames);
		initialFiles.add(initialActiveFile);
		initialFiles.add(initialPassiveFile);
		
		
		return initialFiles;
	}
	
	/**
	 * Create the files used during the 'working write' when executing the kineticsSRM model. 
	 * 
	 * @param initial
	 * @param modelName
	 * @param processedActiveParam
	 * @param caseNames
	 * @param expFiles
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	@Override
	public List<String> createFolderAll(List<String> processedActiveParam) throws IOException, MoDSAgentException {
		// set the mechanism file and inputParams.xml file path
		File copyOfMechanismFilePath = new File(folderAllPath.concat(FRONTSLASH+FILE_MECHANISM));
		File inputParamsFilePath = new File(folderAllPath.concat(FRONTSLASH+FILE_KINETICS_INPUTPARAMS));
		
		// get the filePath of experimental data and mechanism
		File expData = null;
		File mechanism = null;
		for (String expFilePath : expFiles) {
			if (expFilePath.contains(FILE_MODEL_EXPDATA_SUFFIX)) {
				expData = new File(folderTemporaryPath.concat(FRONTSLASH+expFilePath));
			} else if (expFilePath.contains(FILE_MECHANISM)) {
				mechanism = new File(folderTemporaryPath.concat(FRONTSLASH+expFilePath));
			}
		}
		
		// create files in the initial folder
		List<String> allFiles = new ArrayList<>();
		String mechanismFile = copyMechanismFile(copyOfMechanismFilePath, mechanism);
		String inputParamsFile = createInputParamsFile(inputParamsFilePath, expData, mechanismFile);
		allFiles.add(mechanismFile);
		allFiles.add(inputParamsFile);
		
		return allFiles;
	}
	
	
	public void setUpMoDS() throws IOException, MoDSAgentException {
		// set up algorithms
		String active_subtype = new String();
		for (String i : activeParameters.keySet()) {
			active_subtype = active_subtype.concat(" subtype_"+activeParameters.get(i));
		}
		LinkedHashMap<String, LinkedHashMap<String, String>> algorithms = new LinkedHashMap<String, LinkedHashMap<String, String>>();
		LinkedHashMap<String, String> algoSampling = new LinkedHashMap<String, String>();
		algoSampling.put("optimisable_param_subtypes", active_subtype.substring(1));
		algoSampling.put("response_param_subtypes", "subtype_".concat(outputResponses.get(0)));
		algoSampling.put("algorithm_type", "Sobol");
//		algoSampling.put("model_name", "exe");
		algoSampling.put("objective_function", "NormalisedSumOfSquares");
		algoSampling.put("output_by_case", "false");
		algoSampling.put("output_values", "true");
		algoSampling.put("n_points", "1000");
		algoSampling.put("seed", "1");
		algoSampling.put("output_interval", "10");
		algoSampling.put("previous_algorithm", "Initial");
		LinkedHashMap<String, String> algoCalibration = new LinkedHashMap<String, String>();
		algoCalibration.put("optimisable_param_subtypes", active_subtype.substring(1));
		algoCalibration.put("response_param_subtypes", "subtype_".concat(outputResponses.get(0)));
		algoCalibration.put("algorithm_type", "Hooke_Jeeves");
//		algoCalibration.put("model_name", "exe");
		algoCalibration.put("objective_function", "NormalisedSumOfSquares");
		algoCalibration.put("output_by_case", "false");
		algoCalibration.put("output_values", "true");
		algoCalibration.put("n_iters", "400");
		algoCalibration.put("n_initial_points", "1");
		algoCalibration.put("constrained", "true");
		algoCalibration.put("rho", "0.2");
		algoCalibration.put("rho_factor", "0.5");
		algoCalibration.put("epsilon", "0.001");
		algoCalibration.put("previous_algorithm", "SamplingAlg");
		algorithms.put("SamplingAlg", algoSampling);
		algorithms.put("CalibrationAlg", algoCalibration);
		collectAlgorithms(algorithms);
		
		// set up model
		LinkedHashMap<String, LinkedHashMap<String, String>> models = new LinkedHashMap<String, LinkedHashMap<String, String>>();
		LinkedHashMap<String, String> model = new LinkedHashMap<String, String>();
		model.put("executable_name", Property.MODEL_KINETICS_EXE.getPropertyName());
		model.put("working_directory", "");
		models.put(modelName, model);
		collectModels(models);
		
		// set up cases
		LinkedHashMap<String, List<String>> cases = new LinkedHashMap<String, List<String>>();
		List<String> caseModel = new ArrayList<>();
		caseModel.add(modelName);
		for (String caseName : caseNames) {
			cases.put(caseName, caseModel);
		}
		collectCases(cases);
		
		// set up files
		LinkedHashMap<String, LinkedHashMap<String, String>> files = new LinkedHashMap<String, LinkedHashMap<String, String>>();
		for (String modelFile : modelFiles) {
			LinkedHashMap<String, String> file = new LinkedHashMap<String, String>();
			if (modelFile.endsWith(".xml")) {
				file.put("file_type", "XML");
				if (modelFile.contains(FILE_KINETICS_INPUTPARAMS)) {
					file.put("XML_namespace", "http://como.cheng.cam.ac.uk/srm");
				}
				
			} else if (modelFile.endsWith(".csv")) {
				file.put("file_type", "DSV");
				file.put("delimiter", ",");
			}
			files.put(modelFile, file);
		}
		collectFiles(files);
		
		// set up parameters
		List<Parameter> parameters = new ArrayList<>();
		// active parameters
		for (String i : activeParameters.keySet()) {
			Parameter param = new Parameter();
			param.setType("active_input");
			param.setName(activeParameters.get(i));
			param.setSubtype("subtype_"+activeParameters.get(i));
			param.setPreserveWhiteSpace("true");
			param.setScaling("linear");
			param.setCaseNamesList(caseNames);
			param.setModelList(caseModel);
			
			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("column", activeParameters.get(i));
			initialRead.put("row", "0");
			initialRead.put("read_function", "Get_DSV_double");
			initialRead.put("lb_abs", "1.0E-3");
			initialRead.put("ub_abs", "1000.0");
			
			LinkedHashMap<String, String> workingWrite = new LinkedHashMap<String, String>();
			workingWrite.put("path", "//srm_inputs/property_group[@ref='Chemistry']/property[@ref='ReactionRateMultipliers']/value["+i+"]");
			workingWrite.put("write_function", "Set_XML_double");
			
			fileHash.put("initialRead "+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_ACTIVE_SUFFIX, initialRead);
			fileHash.put("workingWrite "+FILE_MECHANISM, workingWrite);
			
			param.setFileHash(fileHash);
			parameters.add(param);
		}
		// passive parameters
		for (String i : passiveParameters) {
			Parameter param = new Parameter();
			param.setType("passive_input");
			param.setName(i);
			param.setCaseDetailSep(";");
			param.setPreserveWhiteSpace("true");
			param.setCaseNamesList(caseNames);
			param.setModelList(caseModel);
			
			String path = new String();
			if (i.contains("Temp")) {
				path = "//srm_inputs/property_group[@ref='Reactor']/property[@ref='IniTemp']/value";
			} else if (i.contains("Pres")) {
				path = "//srm_inputs/property_group[@ref='Reactor']/property[@ref='IniPres']/value";
			} else {
				path = "//srm_inputs/mixtures[@type='composition']/composition[@name='"+NAME_OXIDISER+"']/value[@species='"+i+"']";
			}
			
			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("column", i);
			initialRead.put("row", "0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62");
			initialRead.put("read_function", "Get_DSV_double");
			
			LinkedHashMap<String, String> workingWrite = new LinkedHashMap<String, String>();
			workingWrite.put("path", path);
			workingWrite.put("write_function", "Set_XML_double");
			
			fileHash.put("initialRead "+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_PASSIVE_SUFFIX, initialRead);
			fileHash.put("workingWrite "+FILE_KINETICS_INPUTPARAMS, workingWrite);
			
			param.setFileHash(fileHash);
			parameters.add(param);
		}
		// output response
		for (String i : outputResponses) {
			Parameter param = new Parameter();
			param.setType("active_output");
			param.setSubtype("subtype_"+i);
			param.setName(i);
			param.setCaseDetailSep(";");
			param.setNParamsPerCase("1");
			param.setPreserveWhiteSpace("true");
			param.setScaling("linear");
			param.setCaseNamesList(caseNames);
			param.setModelList(caseModel);
			
			String column = new String();
			if (i.contains("Igni") && i.contains("Delay")) {
				column = "Ignition time [ms]";
			}
			
			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("column", i);
			initialRead.put("row", "0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62");
			initialRead.put("read_function", "Get_DSV_double");
			initialRead.put("lb_factor", "0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8;0.8");
			initialRead.put("ub_factor", "1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2;1.2");
			initialRead.put("lb_addend", "-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0;-0.0");
			initialRead.put("ub_addend", "0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0");
			
			LinkedHashMap<String, String> workingRead = new LinkedHashMap<String, String>();
			workingRead.put("column", column);
			workingRead.put("row", "0");
			workingRead.put("read_function", "Get_DSV_double");
			
			fileHash.put("initialRead "+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_PASSIVE_SUFFIX, initialRead);
			fileHash.put("workingRead "+Property.MODEL_KINETICS_OUTPUT.getPropertyName(), workingRead);
			
			param.setFileHash(fileHash);
			parameters.add(param);
		}
		collectParameters(parameters);
	}
	
	
	
	
	/**
	 * Create case file that contains the active parameters used by executable kineticsSRM. 
	 * 
	 * @param activeParameterFilePath
	 * @param processedActiveParam
	 * @param expData
	 * @param caseNames
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	private String createActiveParametersFile(File activeParameterFilePath, List<String> processedActiveParam, File expData, 
			List<String> caseNames) throws IOException, MoDSAgentException {
		
		
		// construct the title of columns
		List<String> columnTitle = new ArrayList<>();
		columnTitle.add("Case name");
		columnTitle.addAll(processedActiveParam);
		
		// construct the reactionMultipliers, serving the initial read of active parameters
		List<String> reactionMultipliers = new ArrayList<>();
		for (int i = 0; i < processedActiveParam.size(); i++) {
			reactionMultipliers.add("1");
		}
		
		// create the BufferedReader and BufferedWriter to read and write files
		BufferedReader br = null;
		BufferedWriter bw = null;
		
		// create the active parameters csv file
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(expData)));
	        bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(activeParameterFilePath), "UTF-8"));
	        // add the column title
	        br.readLine();
	        bw.write(convertToCSV(columnTitle.toArray(new String[0])).concat("\n"));
	        // add the cases
	        int i = 0;
	        while ((br.readLine()) != null) {
	        	bw.write(caseNames.get(i).concat(",").concat(convertToCSV(reactionMultipliers.toArray(new String[0]))).concat("\n"));
	        	i += 1;
	        }
	        // additional check if all cases are added
	        if (i != caseNames.size()) {
	        	System.out.println("The number of cases does NOT match the number of experimental observations.");
	        }
	        // close files
	        bw.close();
			br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return activeParameterFilePath.getName();
	}
	
	/**
	 * Create case file that contains the passive parameters used by executable kineticsSRM, 
	 * also the output response of executable kineticsSRM. 
	 * 
	 * @param passiveParametersAndOutputsFilePath
	 * @param expData
	 * @param caseNames
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	private String createPassiveParametersAndOutputsFile(File passiveParametersAndOutputsFilePath, File expData, 
			List<String> caseNames) throws IOException, MoDSAgentException {
		// create the BufferedReader and BufferedWriter to read and write files
		// add the caseNames to the start of each line of experimental data file
		BufferedReader br = null;
		BufferedWriter bw = null;
		
		// create the passive parameters and output response csv file
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(expData)));
	        bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(passiveParametersAndOutputsFilePath), "UTF-8"));
	        // add the column title
	        String line = br.readLine();
	        bw.write("Case name".concat(",").concat(line).concat("\n"));
	        // add the data part
	        int i = 0;
	        while ((line = br.readLine()) != null) {
	        	bw.write(caseNames.get(i).concat(",").concat(line).concat("\n"));
	        	i += 1;
	        }
	        // additional check if all cases are added
	        if (i != caseNames.size()) {
	        	logger.error("The number of cases does NOT match the number of experimental observations.");
	        }
	        // close files
	        bw.close();
	        br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return passiveParametersAndOutputsFilePath.getName();
	}
	
	/**
	 * Create the mechanism file used by the executable kineticsSRM. 
	 * 
	 * @param mechanism
	 * @param copyOfMechanism
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	private String copyMechanismFile(File copyOfMechanismFilePath, File mechanism) throws IOException, MoDSAgentException {
		// create the BufferedReader and BufferedWriter to read and write files
		BufferedReader br = null;
		BufferedWriter bw = null;
		
		// copy mechanism file
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(mechanism)));
	        bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(copyOfMechanismFilePath), "UTF-8"));
	        String line = new String();
	        while ((line = br.readLine()) != null) {
	        	bw.write(line+"\n");
	        }
	        bw.close();
	        br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return copyOfMechanismFilePath.getName();
	}
	
	/**
	 * Create the InputParams.xml file required by the executable kineticsSRM. 
	 * 
	 * @param inputParamsFilePath
	 * @param expData
	 * @param mechName
	 * @return
	 * @throws IOException
	 * @throws MoDSAgentException
	 */
	private String createInputParamsFile(File inputParamsFilePath, File expData, String mechName) throws IOException, MoDSAgentException {
		// read the first case of experiment
		String[] headerLine = null;
		String[] firstData = null;
		if (expData.isFile()) {
			BufferedReader expReader = new BufferedReader(new FileReader(expData));
			headerLine = expReader.readLine().split(",");
			firstData = expReader.readLine().split(",");
			expReader.close();
		}
		
		// prepare the variables to be passed to InputParams.xml file
		String iniTemp = null;
		String iniTempUnit = null;
		String iniPres = null;
		String iniPresUnit = null;
		ArrayList<String> species = new ArrayList<String>();
		String ignDelayModel = "3";
		String ignDelayDeltaT = "400";
		String ignDelaySpeciesIndex = "CO";
		String ignDelayShowAll = "1";
		String oxidiser = NAME_OXIDISER; // this name is to be further parameterised, also to be connected to MoDS_Inputs.xml
		for (int i = 0; i < headerLine.length; i++) {
			if (headerLine[i].contains("Temp")) {
				if (headerLine[i].contains("Unit")) {
					iniTempUnit = firstData[i];
				} else {
					iniTemp = firstData[i];
				}
			} else if (headerLine[i].contains("Pres")) {
				if (headerLine[i].contains("Unit")) {
					iniPresUnit = firstData[i];
				} else {
					iniPres = firstData[i];
				}
			} else if (headerLine[i].contains("DMM3") 
					|| headerLine[i].contains("O2") 
					|| headerLine[i].contains("N2")) {
				species.add(headerLine[i]);
				species.add(firstData[i]);
			}
		}
		
		String jsonString = new JSONObject()
				.put("kinetics", 
						new JSONObject()
						.put("filePath", inputParamsFilePath.getAbsolutePath())
						.put("reactor", new JSONObject()
								.put("iniTemp", new JSONObject()
										.put("value", iniTemp)
										.put("unit", iniTempUnit))
								.put("iniPres", new JSONObject()
										.put("value", iniPres)
										.put("unit", iniPresUnit)))
						.put("chemistry", new JSONObject()
								.put("mechFile", mechName)
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
		
		return inputParams.getName();
	}
	
	/**
	 * Convert a string array to a string in the format of CSV file. 
	 * 
	 * @param data
	 * @return
	 */
	private String convertToCSV(String[] data) {
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
	private String escapeSpecialCharacters(String data) {
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
	private void checkFolderPath(String folderPath) throws IOException, MoDSAgentException {
		File folder = new File(folderPath);
		if (!folder.exists()) {
			folder.mkdir();
		}
	}
}
