package uk.ac.cam.cares.tools;

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Scanner;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

public class SpeciesGenerator {
	
//	public static void generateSpeciesNameFromGeometryRepresentation(LinkedList<NISTSpeciesId> nistSpeciesLinkedSet) {
//		
//		LinkedHashMap<Integer,String> speciesHashMap = new LinkedHashMap<Integer,String>();
//		
//		for(NISTSpeciesId species: nistSpeciesLinkedSet) {
//			
//			Scanner scanner = new Scanner(species.getGeometry());
//			
//			int n=1;
//			
//			while (scanner.hasNextLine()) {
//			
//			String line = scanner.nextLine();
//			
//			/**
//			 * 
//			 * Take first element in each line from geometry.
//			 * 
//			 */
//	
//			String atomName = line.contains(" ") ? line.substring(0, line.indexOf(" ")) : line;
//			
//			speciesHashMap.put(n++, atomName);
//			
//			}
//			
//			scanner.close();
//			
//			LinkedList<String> speciesNameList= new LinkedList<String>();
//			
//			for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
//				
//			speciesNameList.add(map.getValue());
//			
//			}
//			
//			LinkedHashMap<Integer,String> updatedSpeciesHashMap = new LinkedHashMap<Integer,String>();
//			
//			LinkedHashMap<String, Integer> speciesQrequencyMap = new LinkedHashMap<String, Integer>();
//			
//			for(String speciesName : speciesNameList) {
//				
//			int count =0;
//				
//			for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
//				
//				if(map.getValue().equals(speciesName)) {
//					
//					count++;
//					
//					String newSpeciesName = speciesName+count + "-" + map.getKey();
//					
//					updatedSpeciesHashMap.put(map.getKey(), newSpeciesName);					
//				} 
//			}
//			
//			speciesQrequencyMap.put(speciesName, count);
//			
//			}
//			
//			System.out.println("- - -  - - -  - -Original Map: - -  - - - - - -");
//
//			
//			for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
//				
//			System.out.println(map.getKey() +  "  " + map.getValue());
//			
//			}
//			
//			System.out.println("- - -  - - -  - -Updated Map: - -  - - - - - -");
//			for(Map.Entry<Integer, String> updatedMap : updatedSpeciesHashMap.entrySet()) {
//			
//			System.out.println(updatedMap.getKey() + "  " + updatedMap.getValue());
//			
//			}
//			
//			System.out.println("- - -  - - -  Frequencies- - - -  - - - - - -");
//			
//			for(Map.Entry<String,Integer> fmap : speciesQrequencyMap.entrySet()) {
//				
//				System.out.println(fmap.getKey() + "  " + fmap.getValue());
//			}
//			
//		}
//	}
	
	/**
	 * 
	 * @param nistSpeciesLinkedSet 
	 * @return
	 */
	public  LinkedHashMap<Integer,String> generateSpeciesNamesFromGeometryRepresentation(LinkedList<NISTSpeciesId> nistSpeciesLinkedSet) {
		
		LinkedHashMap<Integer,String> speciesHashMap = new LinkedHashMap<Integer,String>();
		
		for(NISTSpeciesId species: nistSpeciesLinkedSet) {
			
			Scanner scanner = new Scanner(species.getGeometry());
			
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
			
						
		}
		
		return speciesHashMap;

			
	}
	
	/**
	 * 
	 * @param speciesHashMap
	 * @return
	 */
	public LinkedList<String> getSpeciesNameList(LinkedHashMap<Integer, String> speciesHashMap) {
		
		LinkedList<String> speciesNameList= new LinkedList<String>();
		
		for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
			
		speciesNameList.add(map.getValue());
		
		}
		System.out.println(" Species names: ");
		int count =1;
		for(String specienName : speciesNameList) {
			
			System.out.println(count++ + ". " +specienName);
			
		}
		return speciesNameList;
		
	}
	
	/**
	 * 
	 * @param speciesNameList
	 * @param speciesHashMap
	 * @return
	 */
	public LinkedHashMap<Integer,String> getUpdatedSpeciesNameHashMap(LinkedList<String> speciesNameList, LinkedHashMap<Integer,String> speciesHashMap){
		
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
		System.out.println("Updated species map: ");
		
		for(Map.Entry<Integer, String> speciesMap: updatedSpeciesHashMap.entrySet()) {
			
			System.out.println(speciesMap.getKey() + ".  "  + speciesMap.getValue());
			
		}
		
		return updatedSpeciesHashMap;
	}
	
	/**
	 * 
	 * @param speciesHashMap
	 * @param speciesNameList
	 * @return
	 */
	public LinkedHashMap<String, Integer> getSpeciesFrequencyHashMap(LinkedHashMap<Integer,String> speciesHashMap, LinkedList<String> speciesNameList){
		
		LinkedHashMap<String, Integer> speciesFrequencyHashMap = new LinkedHashMap<String, Integer>();
		
		for(String speciesName : speciesNameList) {
			
		int count =0;
			
		for(Map.Entry<Integer, String> map : speciesHashMap.entrySet()) {
			
			if(map.getValue().equals(speciesName)) {
				
				count++;	
			}
		}
		
		speciesFrequencyHashMap.put(speciesName, count);
		
		}
		
		System.out.println("Species frequency map:");
		
	 for(Map.Entry<String, Integer> fmap : speciesFrequencyHashMap.entrySet()) {
		 
		 System.out.println(fmap.getKey() + " :  " + fmap.getValue());
	 }
		return speciesFrequencyHashMap;
	}
}