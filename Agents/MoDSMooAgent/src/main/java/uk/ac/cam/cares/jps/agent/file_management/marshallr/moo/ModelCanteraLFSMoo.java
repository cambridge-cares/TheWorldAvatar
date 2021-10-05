package uk.ac.cam.cares.jps.agent.file_management.marshallr.moo;

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
import java.util.Arrays;
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

import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.ExecutableModel;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.IModel;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSMarshaller;
import uk.ac.cam.cares.jps.agent.file_management.mods.functions.Function;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.Parameter;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.moo.Property;
import uk.ac.cam.cares.jps.kg.OntoChemExpKG;
import uk.ac.cam.cares.jps.kg.OntoKinKG;
import uk.ac.cam.cares.jps.kg.OntoChemExpKG.DataTable;

public class ModelCanteraLFSMoo extends MoDSMarshaller implements IModel {
	private static Logger logger = LoggerFactory.getLogger(ModelCanteraLFSMoo.class);
	private MoDSMooAgentProperty MoDSMooAgentProperty;
	
	private int numOfReactions;
	private String modelName = new String();
	private LinkedHashMap<String, String> activeParameters = new LinkedHashMap<String, String>(); // linkedHashMap? 
	private List<String> passiveParameters = new ArrayList<>();
	private List<String> outputResponses = new ArrayList<>();
	private List<String> expFiles = new ArrayList<>();
	private List<String> modelFiles = new ArrayList<>();
	private List<String> caseNames = new ArrayList<>();
	private String tranModel = "mix-average";
	
	public String getTranModel() {
		return tranModel;
	}

	public void setTranModel(String tranModel) {
		this.tranModel = tranModel;
	}
	
	public ModelCanteraLFSMoo(MoDSMooAgentProperty MoDSMooAgentProperty) {
		super(MoDSMooAgentProperty);
		this.MoDSMooAgentProperty = MoDSMooAgentProperty;
	}

	@Override
	public ExecutableModel formExecutableModel(List<String> experimentIRI, String mechanismIRI,
			List<String> reactionIRIList) throws IOException, MoDSMooAgentException {
		// check if the target folder exist
		checkFolderPath(folderTemporaryPath);
		
		// create ontology kg instance for query
		OntoKinKG ontoKinKG = new OntoKinKG(MoDSMooAgentProperty);
		// query active parameters
		LinkedHashMap<String, String> activeParameters = ontoKinKG.queryAllReactions(mechanismIRI);
		// collect experiment information
		List<List<String>> headers = new ArrayList<List<String>>();
		List<List<String>> dataCollection = new ArrayList<List<String>>();
		for (String experiment : experimentIRI) {
			OntoChemExpKG ocekg = new OntoChemExpKG(MoDSMooAgentProperty);
			DataTable dataTable = ocekg.formatFlameSpeedExpDataTable(experiment);
			headers.add(dataTable.getTableHeader());
			dataCollection.addAll(dataTable.getTableData());
		}
		for (int i = 1; i < headers.size(); i++) {
			if (!headers.get(i).equals(headers.get(i-1))) {
				logger.error("The heasers of all experimental data tables should be consistent.");
			}
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
			caseList.add(Property.MODEL_CANTERA.getPropertyName().concat("_case_"+i));
			i += 1;
		}
		File expDataCSV = new File(folderTemporaryPath.concat(FRONTSLASH).concat(Property.MODEL_CANTERA.getPropertyName().concat(UNDERSCORE+FILE_MODEL_EXPDATA_SUFFIX)));
		try (PrintWriter pw = new PrintWriter(expDataCSV)) {
			dataLines.stream()
			.map(this::convertToCSV)
			.forEach(pw::println);
		}
		
		// obtain passive parameters and output responses from header of exp data csv file
		List<String> passiveParameters = new ArrayList<>();
		List<String> outputResponses = new ArrayList<>();
		for (String param : headers.get(0)) {
			if (param.toLowerCase().contains("flame") && param.toLowerCase().contains("speed")) {
				outputResponses.add(param);
			} else if (param.toLowerCase().contains("phi")) {
				passiveParameters.add(param);
			}
		}
		
		// create model instance
		ExecutableModel canteraLFS = new ExecutableModel();
		
		// set up model name
		canteraLFS.setModelName(Property.MODEL_CANTERA.getPropertyName());
		
		// set up model active parameters
		canteraLFS.setActiveParameters(activeParameters);
		
		// set up model passive parameters
		canteraLFS.setPassiveParameters(passiveParameters);
		
		// set up model output response
		canteraLFS.setOutputResponses(outputResponses);
		
		// set up model exp files
		List<String> expFiles = new ArrayList<>();
		expFiles.add(expDataCSV.getName());
		expFiles.add(FILE_MECHANISM);
		canteraLFS.setExpFiles(expFiles);
		
		// set up model case names
		canteraLFS.setCaseNames(caseList);
		
		
		logger.info("Executable model canteralLFS is prepared. ");
		return canteraLFS;
	}

	@Override
	public List<String> formFiles(ExecutableModel exeModel, String otherOptions) throws IOException, MoDSMooAgentException {
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
		
		// set up the tranModel
		String tran = JSonRequestParser.getFlameSpdTranModel(otherOptions);
		if (tran != null && !tran.isEmpty()) {
			setTranModel(tran);
		}
		
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
		String outputFile = Property.MODEL_CANTERA_OUTPUT.getPropertyName();
		// append all names to modelFiles
		modelFiles.addAll(folderInitialFiles);
		modelFiles.addAll(folderAllFiles);
		modelFiles.add(outputFile);
		
		logger.info("Files required by "+modelName+" is prepared. ");
		
		return modelFiles;
	}

	@Override
	public List<String> createFolderInitial(List<String> activeParameters)
			throws IOException, MoDSMooAgentException {
		// set the passive parameter csv file path
		File passiveParametersAndOutputsFilePath = new File(folderInitialPath
				.concat(FRONTSLASH+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_PASSIVE_SUFFIX));
		// set the base mechanism file path, as Cantera reads active parameters from mechanism file
		File activeParameterBaseMechanismFilePath = new File(folderInitialPath
				.concat(FRONTSLASH+FILE_MECHANISM_BASE));
		
		// get the filePath of experimental data
		File expData = null;
		for (String expFilePath : expFiles) {
			if (expFilePath.contains(FILE_MODEL_EXPDATA_SUFFIX) && expFilePath.contains(modelName)) {
				expData = new File(folderTemporaryPath.concat(FRONTSLASH+expFilePath));
			}
		}
		
		// get the filePath of mechanism file
		File mech = new File(folderTemporaryPath.concat(FRONTSLASH+FILE_MECHANISM));
		
		// create files in the initial folder
		List<String> initialFiles = new ArrayList<>();
		String initialActiveFile = createActiveParametersFile(activeParameterBaseMechanismFilePath, mech);
		String initialPassiveFile = createPassiveParametersAndOutputsFile(passiveParametersAndOutputsFilePath, expData, caseNames);
		initialFiles.add(initialActiveFile);
		initialFiles.add(initialPassiveFile);
		
		logger.info("Folder /Initial required by "+modelName+" is prepared. ");
		
		return initialFiles;
	}

	@Override
	public List<String> createFolderAll(List<String> processedActiveParam)
			throws IOException, MoDSMooAgentException {
		// set the mechanism file, element file and lfsSimulation file path
		File copyOfMechanismFilePath = new File(folderAllPath.concat(FRONTSLASH+FILE_MECHANISM_CANTERA));
		File elementData = new File(folderAllPath.concat(FRONTSLASH+FILE_MECHANISM_ELEMENT));
		File lfsSimulationFilePath = new File(folderAllPath.concat(FRONTSLASH+FILE_CANTERA_LFSSIMULATION));
		
		// get the filePath of experimental data and mechanism
		File expData = null;
		File mechanism = new File(folderTemporaryPath.concat(FRONTSLASH+FILE_MECHANISM));
		for (String expFilePath : expFiles) {
			if (expFilePath.contains(FILE_MODEL_EXPDATA_SUFFIX) && expFilePath.contains(modelName)) {
				expData = new File(folderTemporaryPath.concat(FRONTSLASH+expFilePath));
			}
		}
		
		// create files in the initial folder
		List<String> allFiles = new ArrayList<>();
		allFiles.addAll(generateCanteraMechanismFile(copyOfMechanismFilePath, elementData, mechanism));
		allFiles.add(createLFSSimulationFile(lfsSimulationFilePath, expData));
		
		logger.info("Folder /All required by "+modelName+" is prepared. ");
		
		return allFiles;
	}

	@Override
	public void setUpMoDS() throws IOException, MoDSMooAgentException {
		// modify algorithms with new output response to update response_param_subtypes
		updateAlgorithms("response_param_subtypes", "subtype_".concat(outputResponses.get(0)));
		
		// set up model
		LinkedHashMap<String, LinkedHashMap<String, String>> models = new LinkedHashMap<String, LinkedHashMap<String, String>>();
		LinkedHashMap<String, String> model = new LinkedHashMap<String, String>();
		model.put("executable_name", Property.MODEL_CANTERA_EXE.getPropertyName());
		model.put("working_directory", "");
		if (getTranModel().toLowerCase().contains("average") || getTranModel().toLowerCase().contains("mix") || getTranModel().toLowerCase().contains("1")) {
			model.put("args", Property.MODEL_CANTERA_MIX_AVERAGE_OPT.getPropertyName()+" "+FILE_CANTERA_LFSSIMULATION); // TODO further parameterise this
		} else if (getTranModel().toLowerCase().contains("multi") || getTranModel().toLowerCase().contains("2")) {
			model.put("args", Property.MODEL_CANTERA_MULTI_OPT.getPropertyName()+" "+FILE_CANTERA_LFSSIMULATION);
		}
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
		
		// set up functions
		// TODO further parameterise
		List<Function> functions = new ArrayList<>();
		for (String i : activeParameters.keySet()) {
			Function function = new Function();
			function.setName("rxn_"+i.concat("_update"));
			function.setUsage("working_write");
			
			LinkedHashMap<String, String> detailList = new LinkedHashMap<String, String>();
			detailList.put("independent_variables", "multi base");
			detailList.put("independent_param_subtypes", "subtype_"+"rxn_"+i+" subtype_"+"rxn_"+i+"_base");
			detailList.put("dependent_variable", "y");
			detailList.put("dependent_param_subtype", "subtype_"+"rxn_"+i+"_lfs");
			detailList.put("expression", "multi*base");
						
			function.setDetailList(detailList);
			functions.add(function);
		}
		collectFunctions(functions);
		
		// set up parameters
		// TODO further parameterise this
		List<Parameter> parameters = new ArrayList<>();
		// constructing row
		String row = new String();
		for (int j = 0; j < caseNames.size(); j++) {
			row = row.concat(";"+j);
		}
		row = row.substring(1);
		// active parameters
		for (String i : activeParameters.keySet()) {
			// base active parameters
			Parameter baseParam = new Parameter();
			baseParam.setType("active_input");
			baseParam.setSubtype("subtype_"+"rxn_"+i+"_base");
			baseParam.setName("rxn_"+i+"_base");
			baseParam.setPreserveWhiteSpace("true");
			baseParam.setCaseNamesList(caseNames);
			baseParam.setModelList(caseModel);
			
			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("path", "//ctml/reactionData[@id='GAS_reaction_data']/reaction[@id='"+i+"']/rateCoeff/Arrhenius/A");
			initialRead.put("read_function", "Get_XML_double");
			
			fileHash.put("initialRead "+FILE_MECHANISM_BASE, initialRead);
			baseParam.setFileHash(fileHash);
			parameters.add(baseParam);
			
			// lfs active parameters
			Parameter lfsParam = new Parameter();
			lfsParam.setType("active_input");
			lfsParam.setSubtype("subtype_"+"rxn_"+i+"_lfs");
			lfsParam.setName("rxn_"+i+"_lfs");
			lfsParam.setPreserveWhiteSpace("true");
			lfsParam.setCaseNamesList(caseNames);
			lfsParam.setModelList(caseModel);
			
			fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			initialRead = new LinkedHashMap<String, String>();
			initialRead.put("path", "//ctml/reactionData[@id='GAS_reaction_data']/reaction[@id='"+i+"']/rateCoeff/Arrhenius/A");
			initialRead.put("read_function", "Get_XML_double");
			
			LinkedHashMap<String, String> workingWrite = new LinkedHashMap<String, String>();
			workingWrite.put("path", "//ctml/reactionData[@id='GAS_reaction_data']/reaction[@id='"+i+"']/rateCoeff/Arrhenius/A");
			workingWrite.put("write_function", "Set_XML_double");
			
			fileHash.put("initialRead "+FILE_MECHANISM_BASE, initialRead);
			fileHash.put("workingWrite "+FILE_MECHANISM_CANTERA, workingWrite);
			
			lfsParam.setFileHash(fileHash);
			parameters.add(lfsParam);
		}
		// passive parameters
		for (String i : passiveParameters) {
			Parameter param = new Parameter();
			param.setType("passive_input");
			param.setName(i);
			param.setSubtype("subtype_"+i);
			param.setCaseDetailSep(";");
			param.setPreserveWhiteSpace("true");
			param.setCaseNamesList(caseNames);
			param.setModelList(caseModel);
			
			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("column", i);
			initialRead.put("row", row); // TODO further parameterise this
			initialRead.put("read_function", "Get_DSV_double");
			
			LinkedHashMap<String, String> workingWrite = new LinkedHashMap<String, String>();
			workingWrite.put("column", i);
			workingWrite.put("row", "0");
			workingWrite.put("write_function", "Set_DSV_double");
			
			fileHash.put("initialRead "+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_PASSIVE_SUFFIX, initialRead);
			fileHash.put("workingWrite "+FILE_CANTERA_LFSSIMULATION, workingWrite);
			
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
			param.setCaseNamesList(caseNames);
			param.setModelList(caseModel);
			
			String column = new String();
			if (i.toLowerCase().contains("igni") && i.toLowerCase().contains("delay")) {
				column = "Ignition time [ms]";
			} else if (i.toLowerCase().contains("flame") && i.toLowerCase().contains("speed")) {
				column = "Laminar flame speed [cm/s]";
			} // TODO further parameterise this
			
			LinkedHashMap<String, LinkedHashMap<String, String>> fileHash = new LinkedHashMap<String, LinkedHashMap<String, String>>();
			LinkedHashMap<String, String> initialRead = new LinkedHashMap<String, String>();
			initialRead.put("column", i);
			initialRead.put("row", row);
			initialRead.put("read_function", "Get_DSV_double");
//			initialRead.put("lb_addend", "-1.39;-1.56;-1.83;-2.00;-2.10;-2.13;-2.08;-1.95;-2.07;-1.81"); // TODO further parameterise this
//			initialRead.put("ub_addend", "1.39;1.56;1.83;2.00;2.10;2.13;2.08;1.95;2.07;1.81"); // TODO further parameterise this
			
			LinkedHashMap<String, String> workingRead = new LinkedHashMap<String, String>();
			workingRead.put("column", column);
			workingRead.put("row", "0");
			workingRead.put("read_function", "Get_DSV_double");
			
			fileHash.put("initialRead "+FILE_MODS_PREFIX+UNDERSCORE+modelName+UNDERSCORE+FILE_MODS_PASSIVE_SUFFIX, initialRead);
			fileHash.put("workingRead "+Property.MODEL_CANTERA_OUTPUT.getPropertyName(), workingRead);
			
			param.setFileHash(fileHash);
			parameters.add(param);
		}
		collectParameters(parameters);
		
		logger.info("Information related to "+modelName+" in MoDS_inputs XML file is collected. ");
	}
	
	/**
	 * Set up the simulation script required for the model to execute. 
	 * 
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	@Override
	public void placeScript() throws IOException, MoDSMooAgentException {
		File srcScript = new File(getClass().getClassLoader().getResource(Property.MODEL_CANTERA_SCRIPT.getPropertyName()).getFile());
		File jobScript = new File(jobFolderPath.concat(FRONTSLASH+FILE_CANTERALFS_SCRIPT));
		
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
	
	private String createActiveParametersFile(File activeParameterBaseMechanismFilePath, File mech) throws IOException, MoDSMooAgentException {
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
	
	private String createPassiveParametersAndOutputsFile(File passiveParametersAndOutputsFilePath, 
			File expData, List<String> caseNames) throws IOException, MoDSMooAgentException {
		BufferedReader br = null;
		BufferedWriter bw = null;
		
		// create the passive parameters and output response csv file
		try {
			br = new BufferedReader(new InputStreamReader(new FileInputStream(expData)));
			bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(passiveParametersAndOutputsFilePath)));
			// add the column title
			String line = br.readLine();
			bw.write("Case name".concat(",").concat("Mechanism").concat(",").concat(line).concat("\n")); // TODO further parameterise this
			// add the data part
			int i = 0;
			while ((line = br.readLine()) != null) {
				bw.write(caseNames.get(i).concat(",").concat(FILE_MECHANISM_CANTERA).concat(",").concat(line).concat("\n"));
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
	
	private List<String> generateCanteraMechanismFile(File copyOfMechanismFilePath, File elementData, File mechanism) throws IOException, MoDSMooAgentException {
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
	        		element = element.concat(line+"\n");
	        	}
	        	bw.write(line.replace("#element_data", "element_data.xml")+"\n");
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
			bw.write("<ctml>"+"\n");
			bw.write(element);
			bw.write("</ctml>"+"\n");
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		mechanismFiles.add(copyOfMechanismFilePath.getName());
		mechanismFiles.add(elementData.getName());
		
		return mechanismFiles;
	}
	
	private String createLFSSimulationFile(File lfsSimulationFilePath, File expData) throws IOException, MoDSMooAgentException {
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
		for (int i = headerLine.length -1 ; i > 0; i--) {
			if (headerLine[i].toLowerCase().contains("case name") 
					|| headerLine[i].toLowerCase().contains("laminar")
					|| headerLine[i].toLowerCase().contains("flame") 
					|| headerLine[i].toLowerCase().contains("speed") 
					|| headerLine[i].toLowerCase().contains("lfs")) {
				headerList.remove(i+1);
				firstDataList.remove(i+1);
			}
		}
		
		// add header and first case of experiment to csv file
		List<String[]> dataLines = new ArrayList<>();
		dataLines.add(headerList.toArray(new String[0]));
		dataLines.add(firstDataList.toArray(new String[0]));
		
		try (PrintWriter pw = new PrintWriter(lfsSimulationFilePath)) {
			dataLines.stream()
			.map(this::convertToCSV)
			.forEach(pw::println);
		}
		
		return lfsSimulationFilePath.getName();
	}
	
	private void convertCoMoCTMLToCanteraCTML(File comoCTML, File canteraCTML) throws IOException, MoDSMooAgentException, TransformerException {
		TransformerFactory factory = TransformerFactory.newInstance();
		Source xslt = new StreamSource(new File(getClass().getClassLoader().getResource(FILE_COMO_CANTERA_CTML).getPath()));
		Transformer transformer = factory.newTransformer(xslt);
		
		Source como = new StreamSource(comoCTML);
		transformer.transform(como, new StreamResult(canteraCTML));
		
		delete(comoCTML.getPath(), canteraCTML.getPath());
	}
	
}
