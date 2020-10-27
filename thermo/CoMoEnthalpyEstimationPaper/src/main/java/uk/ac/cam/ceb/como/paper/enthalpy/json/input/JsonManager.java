package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;

import org.json.JSONObject;

import uk.ac.cam.ceb.como.paper.enthalpy.utils.FrequencyUtils;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.PropertiesManager;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk
 * @author msff2 (msff2@cam.ac.uk)
 * 
 * - Generates json input files for each result of junit test cross validation stored in folder "rest_results".
 * - It performs federated query via three repositories: ontocompchem and ontospecies stored on Claudius, and ontospecies repository stored on localhost.
 * - It takes only one pair of ontocompchemIRI and ontospeciesIRI for each 'cas reg id'. 
 * - It creates automatically a folder in user's profile and inside that folder stores all input.json files for each test case.
 * - Some values stored in a json file are taken from .properties file. 
 *
 */
public class JsonManager {

	public static Properties ebrAgentProperties = PropertiesManager.loadProperties(JsonManager.class.getClassLoader().getResourceAsStream("ebr.agent.properties"));

	static String ontospeciesServerUrl = ebrAgentProperties.getProperty("ontospecies.rdf4j.server.url").toString();
	static String ontocompchemServerUrl = ebrAgentProperties.getProperty("ontocompchem.rdf4j.server.url").toString();
	static String ontospeciesServerUrls = ebrAgentProperties.getProperty("ontospecies.rdf4j.server.urls").toString();

	public static void main(String[] args) throws Exception {

	createJsonFile("C:\\Users\\NK\\git\\json-input-ebr-agent\\thermochemistry\\CoMoEnthalpyEstimationPaper\\test_data\\test_results", createEBRAgentFolder());

	}
	/**
	 * 
	 * @return creates EBRAgent folder in user's profile.
	 */
	public static File createEBRAgentFolder() {
		
		/**
		 * Creates folder EBRAgent_ with date stamp id.
		 */
		
		String folderName = "EBRAgent_" + new SimpleDateFormat("yyyyMMddHHmmss").format(new Timestamp(0));
		File ebrFolder = new File(System.getProperty("user.home") + "/" + folderName);
		
		ebrFolder.mkdirs();
		
		return ebrFolder;
	}


	/**
	 * 
	 * @param ebrFolder created folder in user's space.
	 * @param folderName the folder path.
	 */
	public static void createJsonFolderPath(File ebrFolder, String folderName) {
		
		/**
		 * Creates a folder where json file will be stored.
		 */
		
		File jsonFolderPath = new File(ebrFolder.getAbsolutePath() + "/" + folderName);
		
		if(!jsonFolderPath.exists()) {
			
		jsonFolderPath.mkdirs();
		
		}
		
	}
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param jsonBean the Java bean that stores information that are used as input for EBRAgent.
	 * @param file the folder path that contains information about number of runs, number of results (reactions), and number of radicals.
	 */
	public static void generateNumberOfRunsResultsRadicalsJson(JsonBean jsonBean, File file) {
		
		String[] names = file.getName().split("-");
		
		int crtRuns = Integer.parseInt(names[5]);
		int crtRes =  Integer.parseInt(names[6]);
		int crtRadicals =Integer.parseInt(names[7]);
		
		/**
		 * Stores in json the following values based on folder name: number of runs, number of reactions and number of radicals. 
		 */
		
		jsonBean.setCtrRuns(crtRuns);
		jsonBean.setCtrRes(crtRes);
		jsonBean.setCtrRadicals(crtRadicals);
		
	}
	
	public static LinkedHashMap<String, LinkedList<SpeciesBean>> getCasRegIDSpeciesBeanList(File innerFile) throws Exception{
		
		LinkedHashMap<String, LinkedList<SpeciesBean>> casRegIDSpeciesMap = new LinkedHashMap<String, LinkedList<SpeciesBean>>();
		
		for (File f4 : innerFile.listFiles()) {
			
			LinkedList<SpeciesBean> speciesBeanList = new LinkedList<SpeciesBean>();
				
			if (f4.isDirectory()) {
				
				/**
				 * 
				 * Federated query via three repositories: ontocompchem and ontospecies on Claudius, and ontospecies on localhost.
				 * It returns level of theory, ontocompchem IRI and ontospecies IRI.
				 * 
				 */
				
			speciesBeanList.addAll(FederatedQuery.runFederatedSPARQLSpeciesBean(ontocompchemServerUrl, ontospeciesServerUrl, ontospeciesServerUrls,QueryString.getSpeciesIRIWithLevelOfTheory(f4.getName())));
			
				/**
				 * linked hash map that stores cas reg id as a key and list of (level of theory, ontocomcphem IRI, ontospecies IRI).
				 */
			casRegIDSpeciesMap.put(f4.getName(), speciesBeanList);
				
			}
			
			}
		
		return casRegIDSpeciesMap;
		
	}
	
	public static void createJsonFile(String rootPath, File ebrFolder) throws Exception {
		
		/**
		 * Iterates through sub-folders in root folder "test_data" and creates Json file
		 * stored in a folder that is named by using reaction type, species type (HCO)
		 * and parameters such as number of reactions, number of runs, and number of
		 * radicals.
		 */
		File root = new File(rootPath);

		for (File f : root.listFiles()) {

			if (f.isDirectory()) {
				
				JsonBean jsonBean  = new JsonBean();

				String name = f.getName();

				/**
				 * Prints on console reaction type based on folder name.
				 */
				System.out.println("reaction type: " + name.substring(name.lastIndexOf("_") + 1));
				
				if (!f.getName().contains("ti_")) {

					for (File f2 : f.listFiles()) {

						if (f2.isDirectory()) {

							System.out.println("Upper folder name: " + f2.getName());
							
							/**
							 * Stores in json the following values based on folder name: number of runs, number of reactions and number of radicals. 
							 */
							
							generateNumberOfRunsResultsRadicalsJson(jsonBean,f2);
							
							/**
							 * Creates a folder where json file will be stored.
							 */
							createJsonFolderPath(ebrFolder, f2.getName()); 
							
							/**
							 * Creates input json file under  folder name that is equal as a "f2" folder name.
							 */
							FileWriter inputJsonFileWriter = new FileWriter(ebrFolder.getAbsolutePath() + "/" + f2.getName() + "/" + "input.json");

							for (File f3 : f2.listFiles()) {

								if (f3.isDirectory()) {
									
									LinkedHashMap<String, LinkedList<SpeciesBean>> casRegIDSpeciesMap = new LinkedHashMap<String, LinkedList<SpeciesBean>>();
									
									casRegIDSpeciesMap.putAll(getCasRegIDSpeciesBeanList(f3));
									
									/**
									 * 
									 * @author NK510 (caresssd@hermes.cam.ac.uk)
									 * 
									 * Calculates frequency of level of theory quantity for all species under one folder.
									 * 
									 */
									FrequencyUtils frequencyUtils = new FrequencyUtils();
									
									LinkedHashMap<String, Integer> frequencyMap = new LinkedHashMap<String, Integer>();
									
									
									/**
									 * Sorted level of theory by frequency value for all species given in one folder 'data-pre-processing'
									 */
									frequencyMap.putAll(frequencyUtils.getCalculationFrequencyOfLevelOfTheory(casRegIDSpeciesMap));
									
									
									/**
									 * Select one pair of (ontocompchem IRI, ontospecies IRI) for each cas registry id and based on frequency of level of theory for all species.
									 */
									LinkedList<SpeciesBean> iriList = new LinkedList<SpeciesBean>();
									
									iriList.addAll(getIriList(casRegIDSpeciesMap,frequencyMap));
									
									getJsonBean(name,iriList, jsonBean,inputJsonFileWriter);
									
									
								}
							}
						}
					}
				}
		}		
	}
	}
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param name the name of root folder
	 * @param iriList the list of species beans (ontocomcpchem iri, ontospecies iri, cas reg id)
	 * @param jsonBean the json bean that contains information as an input for EBR agent.
	 * @param inputJsonFileWriter the writer of input json file.
	 * @throws IOException the io exception.
	 */
	public static void getJsonBean(String name, LinkedList<SpeciesBean> iriList, JsonBean jsonBean,FileWriter inputJsonFileWriter) throws IOException {
		
		jsonBean.setReferenceSpecies(iriList);
		
		System.out.println();
		
		jsonBean.setSrcCompoundsRef(ebrAgentProperties.getProperty("srcCompoundsRef").toString().replaceAll("\"+",""));
		jsonBean.setSrcRefPool(ebrAgentProperties.getProperty("srcRefPool").toString().replaceAll("\"+",""));
		jsonBean.setSrcTargetPool(ebrAgentProperties.getProperty("srcTargetPool").toString().replaceAll("\"+",""));
		jsonBean.setDestRList(ebrAgentProperties.getProperty("destRList").toString().replaceAll("\"+",""));
		jsonBean.setTempFolder(ebrAgentProperties.getProperty("tempFolder").toString().replaceAll("\"+",""));
		
		/**
		 * Set reaction type that is given as folder name.
		 */
		jsonBean.setReactionType(name.substring(name.lastIndexOf("_") + 1));
		
		jsonBean.setInputZipFile(ebrAgentProperties.getProperty("inputZipFile").toString().replaceAll("\"+",""));
		
		/**
		 * Reads 'process to run' from .properties file and stores it into json.
		 */
		jsonBean.setWhichProcessToRun(ebrAgentProperties.getProperty("whichProcessToRun").toString().replaceAll("\"+",""));
		
		/**
		 * Converts value of 'isRefAndTargetSetSame' as a String is converted into boolean.
		 */
		boolean b  = Boolean.parseBoolean(ebrAgentProperties.getProperty("isRefAndTargetSetSame").toString().replaceAll("\"+",""));
		
		jsonBean.setRefAndTargetSetSame(b);
		
		System.out.println("- - - - - - - - - Json object- - - - - - - - - - - - - - - - - - - -");
		
		writeJsonObject(jsonBean,  inputJsonFileWriter);
		
	}
	
	public static void writeJsonObject(JsonBean jsonBean, FileWriter inputJsonFileWriter) throws IOException {
		
		JSONObject jsonObject = new JSONObject(jsonBean);
		JSONObject jobJsonObject = new JSONObject();
		jobJsonObject.put("job", jsonObject);
		
		String ebrJson = jobJsonObject.toString();
		
		System.out.println(ebrJson);
		
		 writeJsonFile(jobJsonObject, inputJsonFileWriter);
		
	}
	
	public static void writeJsonFile(JSONObject jobJsonObject, FileWriter inputJsonFileWriter) throws IOException {
		
		/**
		 * Store json object into json file.
		 */
		inputJsonFileWriter.write(jobJsonObject.toString());
		inputJsonFileWriter.close();
		
		System.out.println("- - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
	}
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param casRegIDSpeciesMap the hash map that contains information about cas registry id, level of theory, ontocompchem iri, ontospecies iri.
	 * @param frequencyMap the hash map that contains information about level of theory as a key, and frequency for that level of theory for all species.
	 * @return the linked list that contains pairs of ontocompchem iri and ontospecies iri.
	 */
	public static LinkedList<SpeciesBean> getIriList (LinkedHashMap<String, LinkedList<SpeciesBean>> casRegIDSpeciesMap, LinkedHashMap<String, Integer> frequencyMap){
		
		LinkedList<SpeciesBean> iriList = new LinkedList<SpeciesBean>();
		
		for(Map.Entry<String, LinkedList<SpeciesBean>> casrmap :  casRegIDSpeciesMap.entrySet()) {

		for(Map.Entry<String, Integer> fmap: frequencyMap.entrySet()) {
				
		LinkedList<SpeciesBean> iri = new LinkedList<SpeciesBean>();
			
		for(SpeciesBean speciesB : casrmap.getValue()) {
				
		if(fmap.getKey().toString().equalsIgnoreCase(speciesB.getLevelOfTheory())) {
				
		SpeciesBean sb = new SpeciesBean(speciesB.getOntocompchemIRI(),speciesB.getOntospeciesIRI());
				
		iri.add(sb);
				
		break;
				
		}
			
		}
			
		iriList.addAll(iri);
			
		}
		
		}
		
		return iriList;
		
	}
}