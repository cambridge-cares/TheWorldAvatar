package com.cmclinnovations.jps.csv.species;

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Scanner;

import com.cmclinnovations.jps.model.species.SpeciesBean;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * Generates species elements (IV columns in csv file) based on given geometry for each species. 
 *
 */
public class SpeciesElementGenerator {
	
/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param nistSpeciesId the species bean that contains information about species cas reg number, species geometry and species bond.
 * @return the hash map that contains species names.
 */
public LinkedHashMap<Integer,String> getSpeciesHashMap(SpeciesBean nistSpeciesId) {
	
	LinkedHashMap<Integer,String> speciesHashMap = new LinkedHashMap<Integer,String>();
	
	Scanner scanner = new Scanner(nistSpeciesId.getGeometry());
	
	int n=1;
	
	while (scanner.hasNextLine()) {
	
	String line = scanner.nextLine();
	
	/**
	 * 
	 * Take first element in each line from geometry.
	 * 
	 */

	String[] atomName = line.split(" ");
	
	speciesHashMap.put(n++, atomName[0]);
	
	}
	
scanner.close();
	
return speciesHashMap;	

}

/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param speciesNameList The list of species beans.
 * @param speciesHashMap the hash map that contains atom names.
 * @return the updated species names with unique id number. For example, {1: Cl} -> {1: Cl3}
 *  
 */

public LinkedHashMap<Integer,String> getUpdatedSpeciesHashMap (LinkedList<String> speciesNameList, LinkedHashMap<Integer,String> speciesHashMap) {
	
	LinkedHashMap<Integer,String> updatedSpeciesHashMap = new LinkedHashMap<Integer,String>();
	
	for(String speciesName : speciesNameList) {
		
		int count =0;
			
		for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
			
			if(map.getValue().equals(speciesName)) {
				
				count++;			

				/**
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * Creates new species name by adding the number of atoms for each species in species list.
				 */
				String newSpeciesName = speciesName+count;
				
				updatedSpeciesHashMap.put(map.getKey(), newSpeciesName);					
			} 
		}
		
		}
	
	
	return updatedSpeciesHashMap;
}

/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param speciesNameList the list of species beans.
 * @param speciesHashMap the species map.
 * @return the hash map that contains species name as a key and number of appearing that species in geometry. For example, {Cl, 1}.
 */
public  LinkedHashMap<String, Integer> gestSpeciesFrequencyHashMap(LinkedList<String> speciesNameList, LinkedHashMap<Integer,String> speciesHashMap){
	
	LinkedHashMap<String, Integer> speciesQrequencyMap = new LinkedHashMap<String, Integer>();
	
	for(String speciesName : speciesNameList) {
		
		int count =0;
			
		for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
			
			if(map.getValue().equals(speciesName)) {
				
				count++;
						
			} 
		}
		
		speciesQrequencyMap.put(speciesName, count);
		
		}

	return speciesQrequencyMap;
}
/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param speciesFrequencyMap the map contains unique species name and frequency of that species in geometry.
 * @return the species elements that includes number of atoms. For example {[O:O1,O2][Cl:Cl1]}
 * 
 */
public String generateSpeciesElements(LinkedHashMap<String, Integer> speciesFrequencyMap) {
	
	String finalSpeciesName = "";
	
	for(Map.Entry<String,Integer> fmap : speciesFrequencyMap.entrySet()) {
		
		String initialSpeciesName = "[" + fmap.getKey() + ":";
		
		String speciesName="";
		
		for(int count=1; count<= fmap.getValue(); count++) {
			
			if(count==fmap.getValue()) {
				
				speciesName = speciesName + fmap.getKey() + count;
				
			}else {
				
			speciesName = speciesName + fmap.getKey() + count +",";
			
			}
		}
		
		 finalSpeciesName =  finalSpeciesName + initialSpeciesName + speciesName + "]";
	}
	
	return "{" +finalSpeciesName + "}";

}

}