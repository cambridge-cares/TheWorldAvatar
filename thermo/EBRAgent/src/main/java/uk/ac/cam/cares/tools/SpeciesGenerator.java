package uk.ac.cam.cares.tools;

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Scanner;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

public class SpeciesGenerator {
	
	public void generateSpeciesNameFromGeometryRepresentation(LinkedList<NISTSpeciesId> nistSpeciesLinkedList) {
		
		LinkedHashMap<Integer,String> speciesHashMap = new LinkedHashMap<Integer,String>();
		
		for(NISTSpeciesId species: nistSpeciesLinkedList) {
			
			speciesHashMap.putAll(getSpeciesHashMap(species));
			
			LinkedList<String> speciesNameList= new LinkedList<String>();
			
			for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
				
			speciesNameList.add(map.getValue());
			
			}
			
			LinkedHashMap<Integer,String> updatedSpeciesHashMap = new LinkedHashMap<Integer,String>();
			
			LinkedHashMap<String, Integer> speciesFrequencyMap = new LinkedHashMap<String, Integer>();
			
			updatedSpeciesHashMap.putAll(getUpdatedSpeciesHashMap(speciesNameList,speciesHashMap));
			
			speciesFrequencyMap.putAll(gestSpeciesFrequencyHashMap(speciesNameList, speciesHashMap));
			
			System.out.println("- - -  - - -  - -Original Map: - -  - - - - - -");

			
			for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
				
			System.out.println(map.getKey() +  "  " + map.getValue());
			
			}
			
			System.out.println("- - -  - - -  - -Updated Map: - -  - - - - - -");
			for(Map.Entry<Integer, String> updatedMap : updatedSpeciesHashMap.entrySet()) {
			
			System.out.println(updatedMap.getKey() + "  " + updatedMap.getValue());
			
			}
			
			System.out.println("- - -  - - -  Frequencies- - - -  - - - - - -");
			
			String finalSpeciesName = "";
			
			for(Map.Entry<String,Integer> fmap : speciesFrequencyMap.entrySet()) {
				
				System.out.println(fmap.getKey() + "  " + fmap.getValue());
				
				String initialSpeciesName = "[" + fmap.getKey() + ": ";
				
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
			
			System.out.println("final species atom list: " + "{" +finalSpeciesName + "}");
			
			
		}
	}

private  LinkedHashMap<Integer,String> getSpeciesHashMap(NISTSpeciesId nistSpeciesId) {
	
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

	String atomName = line.contains(" ") ? line.substring(0, line.indexOf(" ")) : line;
	
	speciesHashMap.put(n++, atomName);
	
	}
	
scanner.close();
	
return speciesHashMap;	

}

private LinkedHashMap<Integer,String> getUpdatedSpeciesHashMap (LinkedList<String> speciesNameList, LinkedHashMap<Integer,String> speciesHashMap) {
	
	LinkedHashMap<Integer,String> updatedSpeciesHashMap = new LinkedHashMap<Integer,String>();
	
	for(String speciesName : speciesNameList) {
		
		int count =0;
			
		for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
			
			if(map.getValue().equals(speciesName)) {
				
				count++;
				
				String newSpeciesName = speciesName+count + "-" + map.getKey();
				
				updatedSpeciesHashMap.put(map.getKey(), newSpeciesName);					
			} 
		}
		
		}
	
	
	return updatedSpeciesHashMap;
}

private LinkedHashMap<String, Integer> gestSpeciesFrequencyHashMap(LinkedList<String> speciesNameList, LinkedHashMap<Integer,String> speciesHashMap){
	
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

}