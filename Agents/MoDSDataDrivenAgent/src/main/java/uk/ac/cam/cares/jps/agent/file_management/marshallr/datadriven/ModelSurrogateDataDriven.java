package uk.ac.cam.cares.jps.agent.file_management.marshallr.datadriven;

import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jayway.jsonpath.internal.Path;

import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.ExecutableModel;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.IModel;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSMarshaller;
import uk.ac.cam.cares.jps.agent.file_management.mods.functions.Function;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.Parameter;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.Property;
import uk.ac.cam.cares.jps.kg.OntoChemExpKG;
import uk.ac.cam.cares.jps.kg.OntoKinKG;
import uk.ac.cam.cares.jps.kg.OntoChemExpKG.DataTable;

public class ModelSurrogateDataDriven extends MoDSMarshaller implements IModel {
	private static final String COMMA_DELIMITER = ",";
	private static Logger logger = LoggerFactory.getLogger(ModelCanteraLFSDataDriven.class);
	private MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty;

	private int numOfReactions;
	private String modelName = new String();
	private LinkedHashMap<String, String> activeParameters = new LinkedHashMap<String, String>(); // linkedHashMap?
	private List<String> passiveParameters = new ArrayList<>();
	private LinkedHashMap<String, String> outputResponses_map = new LinkedHashMap<String, String>();
	private List<String> outputResponses = new ArrayList<>();

	private List<String> expFiles = new ArrayList<>();
	private List<String> modelFiles = new ArrayList<>();
	private List<String> caseNames = new ArrayList<>();
	private String tranModel = "mix-average";
	private List<Double> averageInputVars = new ArrayList<>();
	private List<Double> averageOutputVars = new ArrayList<>();
	
	private List<List<Double>> outputListAveMaxMin_InputVars = new ArrayList<List<Double>>();
	private List<List<Double>> outputListAveMaxMin_OutputVars = new ArrayList<List<Double>>();
		
	public String getTranModel() {
		return tranModel;
	}

	public void setTranModel(String tranModel) {
		this.tranModel = tranModel;
	}

	public ModelSurrogateDataDriven(MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty) {
		super(modsDataDrivenAgentProperty);
		this.modsDataDrivenAgentProperty = modsDataDrivenAgentProperty;
	}

	@Override
	public ExecutableModel formExecutableModel(List<String> experimentIRI, String mechanismIRI,
			List<String> reactionIRIList) throws IOException, MoDSDataDrivenAgentException {
		// check if the target folder exist
		checkFolderPath(folderTemporaryPath);

		// create ontology kg instance for query
		OntoKinKG ontoKinKG = new OntoKinKG(modsDataDrivenAgentProperty);
		List<List<String>> headers = new ArrayList<>();
		List<List<String>> dataCollection = new ArrayList<>();

		// -------------------------------------------Read from csv-----------------------------------------------------

		String csvPathFolder = "C:\\Users\\ddeme\\Desktop\\AM_OT_csv_files\\WorldAvatar_DataDrivenAgent_csv_file";
		String csvFileNameInput = "specific_data_filter_steelmillgrade_toughnessposition_coolingflag_09022021_train_INPUTS_v1.csv";
		String csvFileNameOutput = "specific_data_filter_steelmillgrade_toughnessposition_coolingflag_09022021_train_OUTPUTS_v1.csv";

		String csvPathFileInput = csvPathFolder + "\\" + csvFileNameInput; 
		String csvPathFileOutput = csvPathFolder + "\\" + csvFileNameOutput; 

        String csvFileInput = new QueryBroker().readFileLocal(csvPathFileInput);
        List<String[]> csvArrayInput = MatrixConverter.fromCsvToArray(csvFileInput);
        String csvFileOutput = new QueryBroker().readFileLocal(csvPathFileOutput);
        List<String[]> csvArrayOutput = MatrixConverter.fromCsvToArray(csvFileOutput);
        

		// -----------------  Create the new directories/folders -----------------------------------------------------       
		File dirPathDataAlgorithm = createDir(jobFolderPath, "Data_Algorithm");

		// ----------------   Create the files in the 'Data_Algorithm' directory		
		activeParameters = createDataAlgorithFiles(csvArrayInput, dirPathDataAlgorithm.getAbsolutePath());
		outputResponses_map = createDataAlgorithFiles(csvArrayOutput, dirPathDataAlgorithm.getAbsolutePath());
		
		outputResponses = new ArrayList<String>(outputResponses_map.keySet());
		// ----------------   Create the files in the 'Initial' directory ------------------------------------------------
		
		// Create 'MODS_SIM_INITFILE__AIVarInitReadFile.csv' file
		outputListAveMaxMin_InputVars = createAIVarInitReadFile(csvArrayInput, folderInitialPath);
		averageInputVars = outputListAveMaxMin_InputVars.get(0);
		// Create 'MODS_SIM_INITFILE__cases.csv' file
		outputListAveMaxMin_OutputVars = createCasesFile(csvArrayOutput, folderInitialPath);
		averageOutputVars = outputListAveMaxMin_InputVars.get(0);

		
		modelFiles.add("MODS_SIM_INITFILE__cases.csv");
		modelFiles.add("MODS_SIM_INITFILE__AIVarInitReadFile.csv");

		List<String> caseList = new ArrayList<>();
		caseList.add("CaseGroup_Case");

		// create model instance
		ExecutableModel modsSurrogate = new ExecutableModel();

		// set up model name
		//modsSurrogate.setModelName(Property.MODEL_CANTERA.getPropertyName());
		modsSurrogate.setModelName("Data_model_agent");

		// set up model active parameters
		modsSurrogate.setActiveParameters(activeParameters);

		// set up model passive parameters
		modsSurrogate.setPassiveParameters(passiveParameters);

		// set up model output response
		modsSurrogate.setOutputResponses(outputResponses);

		
		// set the values of the initial file (average values for each variable)
		modsSurrogate.setInputVarsAve(averageInputVars);
		modsSurrogate.setOutputVarsAve(averageOutputVars);

		
		
		// set up model exp files
		modsSurrogate.setExpFiles(expFiles);

		// set up model case names
		modsSurrogate.setCaseNames(caseList);

		logger.info("Executable model DataDriven is prepared. ");
		return modsSurrogate;
	}


	@Override
	public List<String> formFiles(ExecutableModel exeModel, String otherOptions)
			throws IOException, MoDSDataDrivenAgentException {
		// check if the target folder exist
		checkFolderPath(folderInitialPath);
		checkFolderPath(folderAllPath);

		// get the basic information of executable canteraLFS
		modelName = exeModel.getModelName();
		activeParameters = exeModel.getActiveParameters();
		expFiles = exeModel.getExpFiles();
		caseNames = exeModel.getCaseNames();
		outputResponses = exeModel.getOutputResponses();
		passiveParameters = exeModel.getPassiveParameters();
		averageInputVars = exeModel.getInputVarsAve();

		// set up the tranModel
//		String tran = JSonRequestParser.getFlameSpdTranModel(otherOptions);
//		if (tran != null && !tran.isEmpty()) {
//			setTranModel(tran);
//		}

		// process the active parameters to be only the equation of reactions
		List<String> processedActiveParam = new ArrayList<>();
		for (String activeParamNo : activeParameters.keySet()) {
			processedActiveParam.add(activeParamNo);
		}

		// create list to store all files used/produced when executing kineticsSRM model
		// get the name of files in the initial folder
//		List<String> folderInitialFiles = createFolderInitial(processedActiveParam);
		// get the name of files in the all folder
//		List<String> folderAllFiles = createFolderAll(processedActiveParam);
		// name the output file of the model
		//String outputFile = Property.MODEL_CANTERA_OUTPUT.getPropertyName();
		// append all names to modelFiles
//		modelFiles.addAll(folderInitialFiles);
//		modelFiles.addAll(folderAllFiles);
		//modelFiles.add(outputFile);

		logger.info("Files required by " + modelName + " is prepared. ");

		return modelFiles;
	}

	@Override
	public List<String> createFolderInitial(List<String> activeParameters)
			throws IOException, MoDSDataDrivenAgentException {
		// set the passive parameter csv file path
		File passiveParametersAndOutputsFilePath = new File(folderInitialPath.concat(
				FRONTSLASH + FILE_MODS_PREFIX + UNDERSCORE + modelName + UNDERSCORE + FILE_MODS_PASSIVE_SUFFIX));
		// set the base mechanism file path, as Cantera reads active parameters from
		// mechanism file
		File activeParameterBaseMechanismFilePath = new File(
				folderInitialPath.concat(FRONTSLASH + FILE_MECHANISM_BASE));

		// get the filePath of experimental data
		File expData = null;
		for (String expFilePath : expFiles) {
			if (expFilePath.contains(FILE_MODEL_EXPDATA_SUFFIX) && expFilePath.contains(modelName)) {
				expData = new File(folderTemporaryPath.concat(FRONTSLASH + expFilePath));
			}
		}

		// get the filePath of mechanism file
		File mech = new File(folderTemporaryPath.concat(FRONTSLASH + FILE_MECHANISM));

		// create files in the initial folder
		List<String> initialFiles = new ArrayList<>();
		String initialActiveFile = createActiveParametersFile(activeParameterBaseMechanismFilePath, mech);
		//String initialPassiveFile = createPassiveParametersAndOutputsFile(passiveParametersAndOutputsFilePath, expData, caseNames);
		initialFiles.add(initialActiveFile);
		//initialFiles.add(initialPassiveFile);

		logger.info("Folder /Initial required by " + modelName + " is prepared. ");

		return initialFiles;
	}

	@Override
	public List<String> createFolderAll(List<String> processedActiveParam)
			throws IOException, MoDSDataDrivenAgentException {
		// set the mechanism file, element file and lfsSimulation file path
		File copyOfMechanismFilePath = new File(folderAllPath.concat(FRONTSLASH + FILE_MECHANISM_CANTERA));
		File elementData = new File(folderAllPath.concat(FRONTSLASH + FILE_MECHANISM_ELEMENT));
		File lfsSimulationFilePath = new File(folderAllPath.concat(FRONTSLASH + FILE_CANTERA_LFSSIMULATION));

		// get the filePath of experimental data and mechanism
		File expData = null;
		File mechanism = new File(folderTemporaryPath.concat(FRONTSLASH + FILE_MECHANISM));
		for (String expFilePath : expFiles) {
			if (expFilePath.contains(FILE_MODEL_EXPDATA_SUFFIX) && expFilePath.contains(modelName)) {
				expData = new File(folderTemporaryPath.concat(FRONTSLASH + expFilePath));
			}
		}

		// create files in the initial folder
		List<String> allFiles = new ArrayList<>();
		allFiles.addAll(generateCanteraMechanismFile(copyOfMechanismFilePath, elementData, mechanism));
		allFiles.add(createLFSSimulationFile(lfsSimulationFilePath, expData));

		logger.info("Folder /All required by " + modelName + " is prepared. ");

		return allFiles;
	}

	

	/**
	 * Set up the simulation script required for the model to execute.
	 * 
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	@Override
	public void placeScript() throws IOException, MoDSDataDrivenAgentException {
		File srcScript = new File(
				getClass().getClassLoader().getResource(Property.MODEL_CANTERA_SCRIPT.getPropertyName()).getFile());
		File jobScript = new File(jobFolderPath.concat(FRONTSLASH + FILE_CANTERALFS_SCRIPT));

		// create the BufferedReader and BufferedWriter to read and write files
		BufferedReader br = null;
		BufferedWriter bw = null;

		// copy the runCanteraLFS.py script
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(srcScript)));
			bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(jobScript)));
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

	private String createActiveParametersFile(File activeParameterBaseMechanismFilePath, File mech)
			throws IOException, MoDSDataDrivenAgentException {
		// copy the mechanism file to activeParameterBaseMechanismFilePath
		BufferedReader br = null;
		BufferedWriter bw = null;

		// copy the mechanism file
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(mech)));
			bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(activeParameterBaseMechanismFilePath)));
			String line = new String();
			while ((line = br.readLine()) != null) {
				bw.write(line.concat("\n"));
			}
			bw.close();
			br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return activeParameterBaseMechanismFilePath.getName();
	}

	private String createPassiveParametersAndOutputsFile(File passiveParametersAndOutputsFilePath, File expData,
			List<String> caseNames) throws IOException, MoDSDataDrivenAgentException {
		BufferedReader br = null;
		BufferedWriter bw = null;

		// create the passive parameters and output response csv file
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(expData)));
			bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(passiveParametersAndOutputsFilePath)));
			// add the column title
			String line = br.readLine();
			bw.write("Case name".concat(",").concat("Mechanism").concat(",").concat(line).concat("\n")); // TODO further
																											// parameterise
																											// this
			// add the data part
			int i = 0;
			while ((line = br.readLine()) != null) {
				bw.write(caseNames.get(i).concat(",").concat(FILE_MECHANISM_CANTERA).concat(",").concat(line)
						.concat("\n"));
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

	private List<String> generateCanteraMechanismFile(File copyOfMechanismFilePath, File elementData, File mechanism)
			throws IOException, MoDSDataDrivenAgentException {
		// create the BufferedReader and BufferedWriter to read and write files
		BufferedReader br = null;
		BufferedWriter bw = null;

		List<String> mechanismFiles = new ArrayList<>();
		String element = new String();
		// copy the mechanism file
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(mechanism)));
			bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(copyOfMechanismFilePath), "UTF-8"));
			String line = new String();
			while ((line = br.readLine()) != null) {
				if (line.contains("elementData") || line.contains("atomic") || line.contains("element ")) {
					element = element.concat(line + "\n");
				}
				bw.write(line.replace("#element_data", "element_data.xml") + "\n");
			}
			bw.close();
			br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		// convert the CoMo version CTML to Cantera version CTML
		File mechanismFileCanteraCTML = new File(copyOfMechanismFilePath.getPath().replace(".xml", "_temp.xml"));
		try {
			convertCoMoCTMLToCanteraCTML(copyOfMechanismFilePath, mechanismFileCanteraCTML);
		} catch (TransformerException e1) {
			e1.printStackTrace();
		}

		// write element data to file
		try {
			bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(elementData), "UTF-8"));
			bw.write("<ctml>" + "\n");
			bw.write(element);
			bw.write("</ctml>" + "\n");
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		mechanismFiles.add(copyOfMechanismFilePath.getName());
		mechanismFiles.add(elementData.getName());

		return mechanismFiles;
	}

	private String createLFSSimulationFile(File lfsSimulationFilePath, File expData)
			throws IOException, MoDSDataDrivenAgentException {
		// read the first case of experiment
		String[] headerLine = null;
		String[] firstData = null;
		if (expData.isFile()) {
			BufferedReader expReader = new BufferedReader(new FileReader(expData));
			headerLine = expReader.readLine().split(",");
			firstData = expReader.readLine().split(",");
			expReader.close();
		}

		// add the mechanism path part
		List<String> headerList = new ArrayList<String>();
		List<String> firstDataList = new ArrayList<String>();
		headerList.add("Mechanism");
		firstDataList.add(FILE_MECHANISM_CANTERA);
		// remove the part that irrelevant to simulation
		headerList.addAll(Arrays.asList(headerLine));
		firstDataList.addAll(Arrays.asList(firstData));
		for (int i = headerLine.length - 1; i > 0; i--) {
			if (headerLine[i].toLowerCase().contains("case name") || headerLine[i].toLowerCase().contains("laminar")
					|| headerLine[i].toLowerCase().contains("flame") || headerLine[i].toLowerCase().contains("speed")
					|| headerLine[i].toLowerCase().contains("lfs")) {
				headerList.remove(i + 1);
				firstDataList.remove(i + 1);
			}
		}

		// add header and first case of experiment to csv file
		List<String[]> dataLines = new ArrayList<>();
		dataLines.add(headerList.toArray(new String[0]));
		dataLines.add(firstDataList.toArray(new String[0]));

		try (PrintWriter pw = new PrintWriter(lfsSimulationFilePath)) {
			dataLines.stream().map(this::convertToCSV).forEach(pw::println);
		}

		return lfsSimulationFilePath.getName();
	}

	private void convertCoMoCTMLToCanteraCTML(File comoCTML, File canteraCTML)
			throws IOException, MoDSDataDrivenAgentException, TransformerException {
		TransformerFactory factory = TransformerFactory.newInstance();
		Source xslt = new StreamSource(
				new File(getClass().getClassLoader().getResource(FILE_COMO_CANTERA_CTML).getPath()));
		Transformer transformer = factory.newTransformer(xslt);

		Source como = new StreamSource(comoCTML);
		transformer.transform(como, new StreamResult(canteraCTML));

		delete(comoCTML.getPath(), canteraCTML.getPath());
	}

	private File createDir(String path, String dirName) {
		String filePath = path + "\\" + dirName;
		File theDir = new File(filePath);
		if (!theDir.exists()){
			theDir.mkdirs();
		}
		return theDir;
	}
	
	private double calculateAverage(List<String[]> csv_array, int n) {
		
		List<String> column = new ArrayList<String>();

		for(int i = 1; i < csv_array.size(); i++ ) {
		    column.add(csv_array.get(i)[n]);		    
		}
		
		List <Double> values_doubleList = new ArrayList<Double>();
		
		for(String s : column) values_doubleList.add(Double.parseDouble(s));
		
	    if (values_doubleList == null || values_doubleList.isEmpty()) {
	        return 0;
	    }

	    double sum = 0;
	    for (Double mark : values_doubleList) {
	        sum += mark;
	    }

	    return sum / values_doubleList.size();
	}
	
	private List<Double> calculateMaxMin(List<String[]> csv_array, int n) {
		
		List<String> column = new ArrayList<String>();

		for(int i = 1; i < csv_array.size(); i++ ) {
		    column.add(csv_array.get(i)[n]);		    
		}
		
		List<Double> values_doubleList = new ArrayList<Double>();
		
		for(String s : column) values_doubleList.add(Double.parseDouble(s));
		
		Double maxVal = Collections.max(values_doubleList);
		Double minVal = Collections.min(values_doubleList);

		List<Double> listMaxMin = Arrays.asList(maxVal, minVal);
		
	    return listMaxMin;
	}
	
	private LinkedHashMap<String, String> createDataAlgorithFiles(List<String[]> csvArray, String pathDataAlgorithm) throws IOException {
		int numVars = csvArray.get(0).length;
		int numCases = csvArray.size();
		LinkedHashMap<String, String> variable_names = new LinkedHashMap<>();
		
		for (int i = 1; i < numVars; i++) {	
			variable_names.put(csvArray.get(0)[i],String.valueOf(i));
			
			// Create csv file for each variable
			File varFile = new File(pathDataAlgorithm + "\\" + "Data_Algorithm_subtype_" + csvArray.get(0)[i] + ".csv");
			varFile.createNewFile();
			// Write into the file for each variable
		    FileWriter varFileWriter = new FileWriter(varFile.getAbsolutePath());
		    varFileWriter.write("CaseGroup_Case_" + csvArray.get(0)[i] + System.lineSeparator());
		    for(int j = 1; j<numCases; j++ ) {
		    	varFileWriter.write(csvArray.get(j)[i] + System.lineSeparator());
		    }
		    varFileWriter.close();
		}
		
		return variable_names;
	}
	
	
	private List<List<Double>> createAIVarInitReadFile(List<String[]> csvArray, String pathInitial) throws IOException {

		List<Double> average_list = new ArrayList<>();  
		List<Double> max_list = new ArrayList<>(); 
		List<Double> min_list = new ArrayList<>(); 
		
		int numVars = csvArray.get(0).length;
		
		File initialFile_AIVarInitReadFile = new File(pathInitial + "\\" + "MODS_SIM_INITFILE__AIVarInitReadFile.csv");
		initialFile_AIVarInitReadFile.createNewFile();
		
		// Write in 'MODS_SIM_INITFILE__AIVarInitReadFile'
	    FileWriter aiVarInitWriter = new FileWriter(initialFile_AIVarInitReadFile.getAbsolutePath());
		for (int i = 1; i < numVars; i++) {	
			if (i==1) {
			    aiVarInitWriter.write("Case names");	
			}
		    aiVarInitWriter.write("," + csvArray.get(0)[i]);
		}
		for(int i = 1; i<numVars; i++ ) {
			if (i==1) {
			    aiVarInitWriter.write(System.lineSeparator() + "CaseGroup_Case");	
			}
			Object aveVar = calculateAverage(csvArray, i);
		    aiVarInitWriter.write("," + aveVar);
		    average_list.add((Double) aveVar);
		    
		    List<Double> listMaxMin = calculateMaxMin(csvArray, i);
			max_list.add(listMaxMin.get(0));
			min_list.add(listMaxMin.get(1));

	    }
	    aiVarInitWriter.close();
	    
	    List<List<Double>> outputListAveMAxMin = new ArrayList<List<Double>>();
	    
	    outputListAveMAxMin.add(average_list);
	    outputListAveMAxMin.add(max_list);
	    outputListAveMAxMin.add(min_list);

	    return outputListAveMAxMin;
	}
	
	private List<List<Double>> createCasesFile(List<String[]> csvArray, String pathInitial) throws IOException {
		
		List<Double> average_list = new ArrayList<>();  
		List<Double> max_list = new ArrayList<>(); 
		List<Double> min_list = new ArrayList<>(); 
		
		int numVars = csvArray.get(0).length;
		
		File initialFile_cases = new File(pathInitial + "\\" + "MODS_SIM_INITFILE__cases.csv");
		initialFile_cases.createNewFile();
		
		// Write in 'MODS_SIM_INITFILE__AIVarInitReadFile'
	    FileWriter aiVarInitWriter = new FileWriter(initialFile_cases.getAbsolutePath());
		for (int i = 1; i < numVars; i++) {	
			if (i==1) {
			    aiVarInitWriter.write("Case names");	
			}
		    aiVarInitWriter.write("," + csvArray.get(0)[i]);
		}
		for(int i = 1; i<numVars; i++ ) {
			if (i==1) {
			    aiVarInitWriter.write(System.lineSeparator() + "CaseGroup_Case");	
			}
			Object aveVar = calculateAverage(csvArray, i);
		    aiVarInitWriter.write("," + aveVar);
		    average_list.add((Double) aveVar);
		    
		    List<Double> listMaxMin = calculateMaxMin(csvArray, i);
			max_list.add(listMaxMin.get(0));
			min_list.add(listMaxMin.get(1));
	    }
	    aiVarInitWriter.close();
	    
	    List<List<Double>> outputListAveMAxMin = new ArrayList<List<Double>>();
	    
	    outputListAveMAxMin.add(average_list);
	    outputListAveMAxMin.add(max_list);
	    outputListAveMAxMin.add(min_list);

	    return outputListAveMAxMin;
	}

	private void createModsInputsFile(List<String[]> csvArray, String pathWorkingDir) throws IOException {
		File modsInputsFile = new File(pathWorkingDir + "\\" + "MoDS_inputs.xml");
		modsInputsFile.createNewFile();
	}
	
	public void setUpMoDS() throws IOException, MoDSDataDrivenAgentException {
				
		// set up algorithms
		String active_subtype = new String();
		for (String i : activeParameters.keySet()) {
			active_subtype = active_subtype.concat(" subtype_"+i);
		}
		String output_subtype = new String();
		for (String i : outputResponses_map.keySet()) {
			output_subtype = output_subtype.concat(" subtype_"+i);
		}
		String active_and_output_subtype = new String();
		active_and_output_subtype = active_subtype + output_subtype;
		
		 
		LinkedHashMap<String, LinkedHashMap<String, String>> algorithms = new LinkedHashMap<String, LinkedHashMap<String, String>>();
		
		LinkedHashMap<String, String> algoInitial = new LinkedHashMap<String, String>();
		algoInitial.put("algorithm_type", "Initial");
		algoInitial.put("optimisable_param_subtypes", "");
		algoInitial.put("response_param_subtypes", "");
		algoInitial.put("global", "false");

		LinkedHashMap<String, String> algoDataAlgorithm = new LinkedHashMap<String, String>();

		algoDataAlgorithm.put("algorithm_type", "Read_previous");
		algoDataAlgorithm.put("param_subtypes", active_and_output_subtype.substring(1));
		algoDataAlgorithm.put("objective_function", "SumOfSquares");
		algoDataAlgorithm.put("output_by_case", "false");
		algoDataAlgorithm.put("output_values", "false");
		algoDataAlgorithm.put("global", "false");
		algoDataAlgorithm.put("previous_algorithm", "Data_Algorithm");
		
		LinkedHashMap<String, String> algoGenSurrogateAlg = new LinkedHashMap<String, String>();
		algoGenSurrogateAlg.put("algorithm_type", "Surrogate");
		algoGenSurrogateAlg.put("optimisable_param_subtypes", active_subtype.substring(1));
		algoGenSurrogateAlg.put("response_param_subtypes", output_subtype.substring(1));
		algoGenSurrogateAlg.put("global", "false");
		algoGenSurrogateAlg.put("surrogate_type", "HDMR");
		algoGenSurrogateAlg.put("fit_to_coded_responses", "false");
		algoGenSurrogateAlg.put("order", "6");
		algoGenSurrogateAlg.put("HDMRorder", "2");
		algoGenSurrogateAlg.put("r_squared_tol", "0.99999");
		algoGenSurrogateAlg.put("previous_algorithm", "Data_Algorithm");
		
		algorithms.put("Initial", algoInitial);
		algorithms.put("Data_Algorithm", algoDataAlgorithm);
		algorithms.put("GenSurrogateAlg", algoGenSurrogateAlg);
		collectAlgorithms(algorithms);
		
		// set up model
		LinkedHashMap<String, LinkedHashMap<String, String>> models = new LinkedHashMap<String, LinkedHashMap<String, String>>();
		LinkedHashMap<String, String> model = new LinkedHashMap<String, String>();
		model.put("fnames_for_dependent_models", "");
		model.put("args", "");
		model.put("executable_name", "");
		model.put("model_type", "Executable");
		model.put("working_directory", "");

		models.put(modelName, model);

		collectModels(models);
		
		// set up cases
		LinkedHashMap<String, List<String>> cases = new LinkedHashMap<String, List<String>>();
		List<String> caseModel = new ArrayList<>();
		caseModel.add(modelName);
		cases.put("CaseGroup_Case", caseModel);

		collectCases(cases);
		
		// set up files
		for (String modelFile : modelFiles) {

			LinkedHashMap<String, LinkedHashMap<String, String>> files = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> file = new LinkedHashMap<String, String>();
	
			file.put("file_type", "DSV");
			file.put("delimiter", ",");
				
			files.put(modelFile, file);
			
			collectFiles(files);
		
		}
		
//		// set up files
//		LinkedHashMap<String, LinkedHashMap<String, String>> files = new LinkedHashMap<String, LinkedHashMap<String, String>>();
//		for (String modelFile : modelFiles) {
//			LinkedHashMap<String, String> file = new LinkedHashMap<String, String>();
//			if (modelFile.endsWith(".xml")) {
//				file.put("file_type", "XML");
//				if (modelFile.contains(FILE_KINETICS_INPUTPARAMS)) {
//					file.put("XML_namespace", "http://como.cheng.cam.ac.uk/srm");
//				}
//			} else if (modelFile.endsWith(".csv")) {
//				file.put("file_type", "DSV");
//				file.put("delimiter", ",");
//			}
//			files.put(modelFile, file);
//		}
//		collectFiles(files);
		
		// set up parameters
		List<Parameter> parameters = new ArrayList<>();
		
		// active parameters
		for (String i : activeParameters.keySet()) {
			Parameter param = new Parameter();
			param.setType("active_input");
			param.setName(i);
			param.setSubtype("subtype_"+i);
			param.setPreserveWhiteSpace("true");
			param.setScaling("linear");
			param.setCaseNamesList(caseNames);
			param.setModelList(caseModel);
			
			double maxVariableInitial= outputListAveMaxMin_InputVars.get(1).get(Integer.parseInt(activeParameters.get(i)) - 1);
			double minVariableInitial= outputListAveMaxMin_InputVars.get(2).get(Integer.parseInt(activeParameters.get(i)) - 1);

			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("column", i);
			initialRead.put("row", "0");
			initialRead.put("file_name", "MODS_SIM_INITFILE__AIVarInitReadFile.csv");
			initialRead.put("read_function", "Get_DSV_double");
			initialRead.put("lb_abs", String.valueOf(minVariableInitial));
			initialRead.put("ub_abs", String.valueOf(maxVariableInitial));
			//initialRead.put("lb_abs", String.valueOf(averageVariableInitial * 0.9));
			//initialRead.put("ub_abs", String.valueOf(averageVariableInitial * 1.1));
			
			fileHash.put("initialRead "+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_ACTIVE_SUFFIX, initialRead);
			param.setFileHash(fileHash);
			
			parameters.add(param);
		}

		// output response
		for (String i : outputResponses) {
			Parameter param = new Parameter();
			param.setType("active_output");
			param.setName(i);
			param.setSubtype("subtype_"+i);
			param.setPreserveWhiteSpace("true");
			param.setCaseDetailSep(";");
			param.setNParamsPerCase("1");
			param.setScaling("linear");
			param.setCaseNamesList(caseNames);
			param.setModelList(caseModel);
			
			double maxVariableInitial= (outputListAveMaxMin_OutputVars.get(1)).get(outputResponses.indexOf(i));
			double minVariableInitial= (outputListAveMaxMin_OutputVars.get(2)).get(outputResponses.indexOf(i));
			
			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("column", i);
			initialRead.put("row", "0");
			initialRead.put("file_name", "MODS_SIM_INITFILE__cases.csv");
			initialRead.put("read_function", "Get_DSV_double");
			initialRead.put("lb_factor", "1.0");
			initialRead.put("ub_factor", "1.0");
			initialRead.put("lb_addend", String.valueOf(minVariableInitial));
			initialRead.put("ub_addend", String.valueOf(maxVariableInitial));
			
			fileHash.put("initialRead "+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_ACTIVE_SUFFIX, initialRead);
			param.setFileHash(fileHash);
			
			parameters.add(param);
		}
		
		collectParameters(parameters);
		
		logger.info("Information related to "+modelName+" in MoDS_inputs XML file is collected. ");
	}

//	@Override
//	public void setUpMoDS() throws IOException, MoDSDataDrivenAgentException {
//		// TODO Auto-generated method stub
//		
//	}
}
