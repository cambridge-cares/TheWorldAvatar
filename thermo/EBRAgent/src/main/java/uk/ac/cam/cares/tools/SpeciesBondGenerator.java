package uk.ac.cam.cares.tools;

import java.util.LinkedHashMap;
import java.util.Scanner;
import java.util.Map.Entry;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * Generates bond connectivities based on geometry and bonds
 *
 */
public class SpeciesBondGenerator {

	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param species The species bean
	 * @param updatedSpeciesHashMap the hash map that contains species with number of atoms for each species id. It is generated from geometry.
	 * @return the string that is bond connectivity (5. column in csv file). 
	 */
	public String generateBondConnectivity(NISTSpeciesId species, LinkedHashMap<Integer,String> updatedSpeciesHashMap) {
		
		Scanner scanner = new Scanner(species.getBond());
		
		String speciesConnectivity = "";
		
		while (scanner.hasNextLine()) {
		
		String line = scanner.nextLine();
		
		/**
		 * 
		 * Take first element in each line from species geometry.
		 * 
		 */

		String[] index = line.split(" ");	
		
		String firstSpeciesName="";
		
		for(Entry<Integer, String> updatedMap: updatedSpeciesHashMap.entrySet()) {
			
		if(index[0].equals(updatedMap.getKey().toString())) {
				
				firstSpeciesName= firstSpeciesName + updatedMap.getValue();
			}
		}
		
		String secondSpeciesName="";
		
		for(Entry<Integer, String> updatedMap: updatedSpeciesHashMap.entrySet()) {
			
		if(index[1].equals(updatedMap.getKey().toString())) {
				
				secondSpeciesName= secondSpeciesName + updatedMap.getValue();
				
			}
		}
		
		 speciesConnectivity = speciesConnectivity + "[[" + firstSpeciesName + "]"+"{" + index[2]+ "}" + "[" + secondSpeciesName + "]]";
		
		}
				
	scanner.close();
	
	return "{" + speciesConnectivity + "}";
	
	}
}