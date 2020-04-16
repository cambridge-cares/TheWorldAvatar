package uk.ac.cam.cares.mapping.species;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

import com.opencsv.CSVWriter;

import uk.ac.cam.cares.ebr.manager.FolderManager;
import uk.ac.cam.cares.ebr.manager.RepositoryManager;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 *
 *Main method generates csv file that contains cas registy id and species IRIs related to that cas registry id.
 *
 */
public class CSVGenerator {

	static String localHostOntoSpeciesUrl = "http://localhost:8080/rdf4j-server/repositories/ontospecieskb";
	static String csvFilePath = "C:\\Users\\NK\\Documents\\cas_reg_id_species_2.csv";
	static String gaussianFolderPath ="C:\\Users\\NK\\Documents\\philipp\\171-pb556\\esc\\g09";
	
	public static void main(String[] args) {

		LinkedList<String> speciesCasRegId = new LinkedList<String>(FolderManager.getHCOSpeciesCasRegID(gaussianFolderPath));
		
		HashMap<String, LinkedList<String>> speciesCasMap = new HashMap<String, LinkedList<String>>();
		
		int i =1;
		
		for(String cas: speciesCasRegId) {

			speciesCasMap.putAll(RepositoryManager.queryOntoSpeciesRepository(localHostOntoSpeciesUrl, cas));
			
			System.out.println(i + ". " + cas );
			
			i++;
		}
		
		generateCSVFile(speciesCasMap,csvFilePath);
	}
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param speciesCasMap the hash map that contains species cas registry id and correspoding species IRI 
	 * @param csvFilePath the csv file path
	 * 
	 */
public static void generateCSVFile(HashMap<String, LinkedList<String>> speciesCasMap, String csvFilePath){
		
		File file = new File(csvFilePath);
		
		try {
		
		 FileWriter output = new FileWriter(file);
		 
         CSVWriter writer = new CSVWriter(output);
		
		for(Entry<String, LinkedList<String>> map: speciesCasMap.entrySet()) {

			String key = map.getKey();
			
			String speciesIRI = "";
			
			int size = map.getValue().size();
			
			for(String s: map.getValue()) {
				
				if(size>1) {
					
				speciesIRI =  s + "," +speciesIRI;
				
				}else {
					
					speciesIRI=s;
				}
			}

			String[] line = {key, speciesIRI};
			
			writer.writeNext(line);
		}
		
		writer.close();
		
		}catch(IOException e){
		
		e.printStackTrace();
		
		}
	}
}