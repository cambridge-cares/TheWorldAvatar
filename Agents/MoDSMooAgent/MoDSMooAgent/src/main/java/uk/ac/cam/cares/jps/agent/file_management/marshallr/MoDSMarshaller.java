package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.output.FileWriterWithEncoding;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.InitMoDSInputs;
import uk.ac.cam.cares.jps.agent.file_management.MoDSInputsState;
import uk.ac.cam.cares.jps.agent.file_management.mods.MoDS;
import uk.ac.cam.cares.jps.agent.file_management.mods.functions.Function;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.Parameter;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;

public class MoDSMarshaller extends MoDSInputsState implements IMoDSMarshaller {
	private static Logger logger = LoggerFactory.getLogger(MoDSMarshaller.class);
	private MoDSMooAgentProperty MoDSMooAgentProperty;
	
	public MoDSMarshaller(MoDSMooAgentProperty MoDSMooAgentProperty) {
		this.MoDSMooAgentProperty = MoDSMooAgentProperty;
	}
	
	/**
	 * Initialise setup. 
	 * 
	 * @param jobFolderName
	 */
	public void initialise(String jobFolderName) throws IOException, MoDSMooAgentException {
		init();
		
		// create the job folder 
		jobFolderPath = Paths.get(FOLDER_ROOT,"."+jobFolderName).toString();
		//jobFolderPath = Paths.getFOLDER_ROOT.concat(File.pathSeparator).concat("."+jobFolderName);
		File jobFolder = new File(jobFolderPath);
		if (!jobFolder.exists()) {
			jobFolder.mkdirs();
		}
		// create the \Temporary folder for file storage
		//folderTemporaryPath = jobFolderPath.concat(File.pathSeparator).concat(FOLDER_TEMPORARY);
		folderTemporaryPath = Paths.get(jobFolderPath,FOLDER_TEMPORARY).toString();
		File temporary = new File(folderTemporaryPath);
		if (!temporary.exists()) {
			temporary.mkdirs();
		}
		// create the \Initial folder
		//folderInitialPath = jobFolderPath.concat(File.pathSeparator).concat(FOLDER_INITIAL);
		folderInitialPath = Paths.get(jobFolderPath,FOLDER_INITIAL).toString();
		File initial = new File(folderInitialPath);
		if (!initial.exists()) {
			initial.mkdirs();
		}
		// create the \All folder
		//folderAllPath = jobFolderPath.concat(File.pathSeparator).concat(FOLDER_ALL);
		folderAllPath = Paths.get(jobFolderPath,FOLDER_ALL).toString();
		File all = new File(folderAllPath);
		if (!all.exists()) {
			all.mkdirs();
		}
		// create the \Working_dir folder
		//folderWorkingDirPath = jobFolderPath.concat(File.pathSeparator).concat(FOLDER_WORKING_DIR);
		folderWorkingDirPath = Paths.get(jobFolderPath,FOLDER_WORKING_DIR).toString();
		File workingDir = new File(folderWorkingDirPath);
		if (!workingDir.exists()) {
			workingDir.mkdirs();
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
		
		logger.info("MoDSMarshaller was initialised.");
	}

	@Override
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException {
		ModelKineticsSRM kineticsSRM = new ModelKineticsSRM(MoDSMooAgentProperty);
		ExecutableModel exeModel = kineticsSRM.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		kineticsSRM.formFiles(exeModel, otherOptions);
		kineticsSRM.setUpMoDS();
		kineticsSRM.placeScript();
		
		logger.info("Model kineticsSRM was added to the MoDS job.");
	}

	@Override
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException {
		ModelCanteraLFS canteraLFS = new ModelCanteraLFS(MoDSMooAgentProperty);
		ExecutableModel exeModel = canteraLFS.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		canteraLFS.formFiles(exeModel, otherOptions);
		canteraLFS.setUpMoDS();
		canteraLFS.placeScript();
		
		logger.info("Model canteraLFS was added to the MoDS job.");
	}
	
	@Override
	public String marshall() throws IOException, MoDSMooAgentException {
		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true);
		
		
		mods = objectMapper.readValue(objectMapper.writeValueAsString(modsJsonNode), MoDS.class);
		mods.setXmlns("http://como.cheng.cam.ac.uk/MoDS");
		mods.setXmlnsXsi("http://www.w3.org/2001/XMLSchema-instance");
		mods.setXsiSchemaLocation("http://como.cheng.cam.ac.uk/MoDS MoDS_inputs.xsd");
		//saveMoDSInputsContent(folderWorkingDirPath.concat(FRONTSLASH+FILE_MODS_INPUTS));
		saveMoDSInputsContent(Paths.get(folderWorkingDirPath,FILE_MODS_INPUTS).toString());
		//cleanUp(folderWorkingDirPath.concat(FRONTSLASH+FILE_MODS_INPUTS));
		cleanUp(Paths.get(folderWorkingDirPath,FILE_MODS_INPUTS).toString());
		deleteDirectory(new File(folderTemporaryPath));
		
		logger.info("MoDS input files are now in place.");
		
		return jobFolderPath;
	}
	
	/**
	 * Initialisation. 
	 */
	private void init() {
		initMoDSInputs = new InitMoDSInputs(MoDSMooAgentProperty);
		initMoDSInputs.init();
	}
	
	/**
	 * Generate Algorithms node in MoDS input file. 
	 * 
	 * @param algorithms
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void collectAlgorithms(LinkedHashMap<String, LinkedHashMap<String, String>> algorithms) throws IOException, MoDSMooAgentException {
		for (String i : algorithms.keySet()) {
			String algoJson = new JSONObject().put("name", i)
					.put("details", collectDetails(algorithms.get(i))).toString();
			JsonNode locatedNode = modsJsonNode.path("algorithms").path("algorithm");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(algoJson));
		}
	}
	
	/**
	 * Update Algorithms node in MoDS input file. 
	 * 
	 * @param detailLocation
	 * @param newContent
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void updateAlgorithms(String detailLocation, String newContent) throws IOException, MoDSMooAgentException {
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
	
	/**
	 * Generate Models node in MoDS input file. 
	 * 
	 * @param models
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void collectModels(LinkedHashMap<String, LinkedHashMap<String, String>> models) throws IOException, MoDSMooAgentException {
		for (String i : models.keySet()) {
			String modJson = new JSONObject().put("name", i)
					.put("details", collectDetails(models.get(i))).toString();
			JsonNode locatedNode = modsJsonNode.path("models").path("model");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(modJson));	
		}
	}
	
	/**
	 * Generate simplified Models node in MoDS input file. 
	 * 
	 * @param models
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public JSONObject collectSimplifiedModels(List<String> models) throws IOException, MoDSMooAgentException {
		JSONObject modelsInJson = new JSONObject();
		List<JSONObject> modelArray = new ArrayList<>();
		for (String model : models) {
			JSONObject modJson = new JSONObject().put("name", model);
			modelArray.add(modJson);
		}
		modelsInJson.put("model", new JSONArray(modelArray));
		return modelsInJson;
	}
	
	/**
	 * Generate Cases node in MoDS input file. 
	 * 
	 * @param cases
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void collectCases(LinkedHashMap<String, List<String>> cases) throws IOException, MoDSMooAgentException {
		for (String i : cases.keySet()) {
			String caseJson = new JSONObject().put("name", i)
					.put("models", collectSimplifiedModels(cases.get(i))).toString();
			JsonNode locatedNode = modsJsonNode.path("cases").path("case");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(caseJson));
		}
	}
	
	/**
	 * Generate simplified Cases node in MoDS input file. 
	 * 
	 * @param cases
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public JSONObject collectSimplifiedCases(List<String> cases) throws IOException, MoDSMooAgentException {
		JSONObject casesInJson = new JSONObject();
		List<JSONObject> caseArray = new ArrayList<>();
		for (String cas : cases) {
			JSONObject casJson = new JSONObject().put("name", cas);
			caseArray.add(casJson);
		}
		casesInJson.put("case", new JSONArray(caseArray));
		return casesInJson;
	}
	
	/**
	 * Generate Files node in MoDS input file. 
	 * 
	 * @param files
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void collectFiles(LinkedHashMap<String, LinkedHashMap<String, String>> files) throws IOException, MoDSMooAgentException {
		for (String i : files.keySet() ) {
			String fileJson = new JSONObject().put("name", i)
					.put("details", collectDetails(files.get(i))).toString();
			JsonNode locatedNode = modsJsonNode.path("files").path("file");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(fileJson));
		}
	}
	
	/**
	 * Generate Functions node in MoDS input file. 
	 * 
	 * @param functions
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void collectFunctions(List<Function> functions) throws IOException, MoDSMooAgentException {
		for (Function function : functions) {
			String funcJson = new JSONObject()
					.put("name", function.getName())
					.put("usage", function.getUsage())
					.put("details", collectDetails(function.getDetailList())).toString();
			JsonNode locatedNode = modsJsonNode.path("functions").path("function");
			ArrayNode addedNode = ((ArrayNode) locatedNode).add(new ObjectMapper().readTree(funcJson));
		}
	}
	
	/**
	 * Generate Parameters node in MoDS input file. 
	 * 
	 * @param parameters
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public void collectParameters(List<Parameter> parameters) throws IOException, MoDSMooAgentException {
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
	
	/**
	 * Generate Files node if Parameters node in MoDS input file. 
	 * 
	 * @param files
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public JSONObject collectParameterFiles(LinkedHashMap<String, LinkedHashMap<String, String>> files) 
			throws IOException, MoDSMooAgentException {
		JSONObject filesInJson = new JSONObject();
		for (String i : files.keySet()) {
			String tokens[] = i.split(" ");
			filesInJson.put(tokens[0], new JSONObject().put("name", tokens[1])
							.put("details", collectDetails(files.get(i))));
		}
		return filesInJson;
	}
	
	/**
	 * Generate Details node in MoDS input file. 
	 * 
	 * @param details
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	public JSONObject collectDetails(LinkedHashMap<String, String> details) throws IOException, MoDSMooAgentException {
		JSONObject detailsInJson = new JSONObject();
		List<JSONObject> detailArray = new ArrayList<>();
		for (String i : details.keySet()) {
			JSONObject detailJson = new JSONObject().put("name", i).put("content", details.get(i));
			detailArray.add(detailJson);
		}
		detailsInJson.put("detail", new JSONArray(detailArray));
		return detailsInJson;
	}
	
	/**
	 * Write MoDS inputs to file. 
	 * 
	 * @param filePath
	 * @throws MoDSMooAgentException
	 */
	private void saveMoDSInputsContent(String filePath) throws MoDSMooAgentException {
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
			e.printStackTrace();
		}
	}
	
	/**
	 * Clean up the content generated during initialisation in the MoDS input file. 
	 * 
	 * @param filePath
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	private void cleanUp(String filePath) throws IOException, MoDSMooAgentException {
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
			e.printStackTrace();
		}
	}
	
	/**
	 * Delete the temporary MoDS input file. 
	 * 
	 * @param xmlFileOrig
	 * @param xmlFileTemp
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 */
	protected void delete(String xmlFileOrig, String xmlFileTemp) throws IOException, MoDSMooAgentException {
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
	
	/**
	 * Delete the temporary directory that generated during creating MoDS job. 
	 * 
	 * @param directoryToBeDeleted
	 */
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
	 * @throws MoDSMooAgentException
	 */
	public void checkFolderPath(String folderPath) throws IOException, MoDSMooAgentException {
		File folder = new File(folderPath);
		if (!folder.exists()) {
			folder.mkdir();
		}
	}

	@Override
	public void plugInModelMoo(List<String> dataVar, String mechanismIRI, List<String> reactionIRIList,
			String otherOptions) throws IOException, MoDSMooAgentException {
		// TODO Auto-generated method stub
		
	}

	
	@Override
	public void setUpMoDS() throws IOException, MoDSMooAgentException {
		// TODO Auto-generated method stub
		
	}
	
}
