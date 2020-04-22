package uk.ac.cam.cares.ebr.upload.species;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.LinkedHashMap;

import java.util.Map.Entry;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HttpRequest {

	private static Logger logger = LoggerFactory.getLogger(HttpRequest.class);	
	/**
	 * @author msff2@cam.ac.uk
	 * 
	 * Enables to perform an HTTP get request.
	 * 
	 * @param query
	 * @return
	 * @throws MalformedURLException
	 * @throws IOException
	 */
	public static String performHTTPRequest(String query) throws MalformedURLException, IOException {
		
		
        URL httpURL = new URL(query);
        
        URLConnection httpURLConnection = httpURL.openConnection();
        
        System.out.println("httpURLConnection.getURL(): " + httpURLConnection.getURL());
        
        BufferedReader in = new BufferedReader(
                                new InputStreamReader(
                                		httpURLConnection.getInputStream()));
        String inputLine;
        
        String fileContent = "";
        
        while ((inputLine = in.readLine()) != null){
        	
            fileContent = fileContent.concat(inputLine);
        }
        
        in.close();
        
        System.out.println("fileContent: "+ fileContent + "  fileContent.isEmpty(): " +fileContent.isEmpty());
        
        return fileContent;
    }
	
	/**
	 * @author msff2@cam.ac.uk
	 * 
	 * The code is modified by @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * Uploads the 923 Gaussian files to the OntoCompChem knowledge graph. If the<br>
	 * upload is successful, it returns true. If the upload fails, it<br>
	 * throws an exception. If the Gaussian file does not exist, for any<br>
	 * reason, it returns false.
	 * 
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 * 
	 */
	private static boolean isGaussianFileUploaded(String referenceSpeciesCASRegID, String uniqueSpeciesIRI, String speciesFolderPath, String fileExtension) throws IOException{
		
//		File logFile = new File(jobFolder.getAbsolutePath().concat(File.separator).concat(jobFolder.getName()).concat(slurmJobProperty.getOutputFileExtension()));
		
		String gaussianFilePath = speciesFolderPath+ "\\" + referenceSpeciesCASRegID + fileExtension;
		
//		System.out.println(" file exists: " + gaussianFile.exists() + "  gaussianFile: " + gaussianFile.getAbsolutePath() + "   speciesIRI: " + uniqueSpeciesIRI);
		
//		if(gaussianFile.exists()){
			
//			String uniqueSpeciesIRI = Utils.getUniqueSpeciesIRI(jobFolder, slurmJobProperty);
			
			String jsonInput = generateJSONInput(gaussianFilePath, uniqueSpeciesIRI).toString();
			
			System.out.println("jsonInput: " + jsonInput);
			
			// Performs a HTTP request to upload the log file (output) to<br>
			// the OntoCompChem knowledge graph. 
			
//			performHTTPRequest(slurmJobProperty.getKgURLToUploadResultViaJsonInput().concat(encodeIntoURLFormat(jsonInput)));
			
			performHTTPRequest("http://theworldavatar.com/ontocompchemupload/convert/single?input=".concat(encodeIntoURLFormat(jsonInput)));
			
			return true;
			
//		}else{
			
//			return false;
//		}
	}
	
	/**
	 * Converts a JSON input into the URL formatted string.
	 * 
	 * @param jsonInput
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	private static String encodeIntoURLFormat(String jsonInput) throws UnsupportedEncodingException{
		return URLEncoder.encode(jsonInput, "UTF-8");
	}
	
	/**
	 * Generates the JSON input for uploading the log file of a DFT job<br>
	 * by calling the ontocompchemupload service. 
	 * 
	 */
	
	private static JSONObject generateJSONInput(String gaussianFilePath, String uniqueSpeciesIRI){
		
		JSONObject input = new JSONObject();
		
		input.put("referenceSpecies", gaussianFilePath);
		input.put("uniqueSpeciesIRI", uniqueSpeciesIRI);
	
//		input.put(Property.JSON_INPUT_REF_SPECIES.getPropertyName(), jobFolder.getAbsolutePath());
//		input.put(Property.JSON_INPUT_UNIQUE_SPECIES_IRI.getPropertyName(), speciesIRIInput);
		
		return input;
	}
	
	/**
	 * Monitors the currently running quantum jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.    
	 * @throws IOException 
	 * 
	 */
	public static void uploadSpecies(LinkedHashMap<String,String> speciesMap, String speciesFolderPath, String fileExtension) throws IOException {

		int count = 1;
		
		for(Entry<String, String> map :speciesMap.entrySet()) {
			
			if(new File (speciesFolderPath + "//" + map.getKey() +fileExtension).exists()){
				
			boolean uploadedGaussianFile = isGaussianFileUploaded(map.getKey().toString(),map.getValue().toString(),speciesFolderPath, fileExtension);
			
			logger.info(count + " . Log file " + speciesFolderPath + map.getKey()+ ".g09 is upladed: " + uploadedGaussianFile);
			
			System.out.println(count + " . Log file " + speciesFolderPath + map.getKey()+ ".g09 is upladed: " + uploadedGaussianFile);
			
			count++;
			
			}else {
				continue;
			}
		}		
	}
}