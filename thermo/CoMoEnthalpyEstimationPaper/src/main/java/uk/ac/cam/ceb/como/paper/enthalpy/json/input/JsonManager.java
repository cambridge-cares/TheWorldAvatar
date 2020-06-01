package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

import java.io.File;
import java.io.FileWriter;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.LinkedList;
import java.util.Properties;

import org.json.JSONObject;

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

	public static Properties ebrAgentProperties = PropertiesManager
			.loadProperties(JsonManager.class.getClassLoader().getResourceAsStream("ebr.agent.properties"));

	static String ontospeciesServerUrl = ebrAgentProperties.getProperty("ontospecies.rdf4j.server.url").toString();
	static String ontocompchemServerUrl = ebrAgentProperties.getProperty("ontocompchem.rdf4j.server.url").toString();
	static String ontospeciesServerUrls = ebrAgentProperties.getProperty("ontospecies.rdf4j.server.urls").toString();
	
	static String whichProcessToRun = ebrAgentProperties.getProperty("whichProcessToRun").toString();

	public static void main(String[] args) throws Exception {

	createJsonFile("C:\\Users\\NK\\git\\json-input-ebr-agent\\thermochemistry\\CoMoEnthalpyEstimationPaper\\test_data\\test_results");

	}

	
	public static void createJsonFile(String rootPath) throws Exception {

		/**
		 * Creates folder EBRAgent_ with date stamp id.
		 */
		String folderName = "EBRAgent_" + new SimpleDateFormat("yyyyMMddHHmmss").format(new Timestamp(0));

		File ebrFolder = new File(System.getProperty("user.home") + "/" + folderName);

		ebrFolder.mkdirs();

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
							
							
							String[] names = f2.getName().split("-");
							
							int crtRuns = Integer.parseInt(names[5]);
							int crtRes =  Integer.parseInt(names[6]);
							int crtRadicals =Integer.parseInt(names[7]);
							
							/**
							 * Stores in json the following values based on folder name: number of runs, number of reactions and number of radicals. 
							 */
							
							jsonBean.setCtrRuns(crtRuns);
							jsonBean.setCtrRes(crtRes);
							jsonBean.setCtrRadicals(crtRadicals);
							
							/**
							 * Creates a folder where json file will be stored.
							 */
							File jsonFolderPath = new File(ebrFolder.getAbsolutePath() + "/" + f2.getName());
							
							if(!jsonFolderPath.exists()) {
								
							jsonFolderPath.mkdirs();
							
							}
							
							/**
							 * Creates input json file under  folder name that is equal as a "f2" folder name.
							 */
							FileWriter inputJsonFileWriter = new FileWriter(ebrFolder.getAbsolutePath() + "/" + f2.getName() + "/" + "input.json");

							for (File f3 : f2.listFiles()) {

								if (f3.isDirectory()) {

									LinkedList<SpeciesBean> speciesBeanList = new LinkedList<SpeciesBean>();
									
									for (File f4 : f3.listFiles()) {

										if (f4.isDirectory()) {
											
											System.out.println("CAS reg ID: " + f4.getName());
											
											/**
											 * 
											 * Federated query via three repositories: ontocompchem and ontospecies on Claudius, and ontospecies on localhost.
											 * 
											 */
											speciesBeanList.addAll(FederatedQuery.runFederatedSPARQLSpeciesBean(ontocompchemServerUrl, ontospeciesServerUrl, ontospeciesServerUrls,QueryString.getSpecies(f4.getName())));
											
											jsonBean.setReferenceSpecies(speciesBeanList);
										}
									}
									
									for(SpeciesBean s: speciesBeanList) {
									
										
										System.out.println("- s.getOntocompchemIRI(): " + s.getOntocompchemIRI());
										System.out.println("- s.getOntospeciesIRI(): " + s.getOntospeciesIRI());
									}
									
									jsonBean.setSrcCompoundsRef("input/g09");
									jsonBean.setSrcRefPool("input/referenceSpecies.csv");
									jsonBean.setSrcTargetPool("input/targetSpecies.csv");
									jsonBean.setDestRList("output");
									jsonBean.setTempFolder("temp");
									
									/**
									 * Set reaction type that is given as folder name.
									 */
									jsonBean.setReactionType(name.substring(name.lastIndexOf("_") + 1));
									
									jsonBean.setInputZipFile("input.zip");
									/**
									 * reads 'process to run' from .properties file and stores it into json.
									 */
									jsonBean.setWhichProcessToRun(whichProcessToRun.toString().replaceAll("\"+",""));
									jsonBean.setRefAndTargetSetSame(false);
									
									System.out.println("- - - - - - - - - Json object- - - - - - - - - - - - - - - - - - - -");
									
									JSONObject jsonObject = new JSONObject(jsonBean);
									JSONObject jobJsonObject = new JSONObject();
									jobJsonObject.put("job", jsonObject);
									
									String ebrJson = jobJsonObject.toString();
									
									System.out.println(ebrJson);
									
									/**
									 * store json object into json file.
									 */
									inputJsonFileWriter.write(jobJsonObject.toString());
									inputJsonFileWriter.close();
									
									System.out.println("- - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
								}
							}
						}
					}
				}
				
				System.out.println("- - -  - -  - - - - -  - -");
			}
		}
	}
}