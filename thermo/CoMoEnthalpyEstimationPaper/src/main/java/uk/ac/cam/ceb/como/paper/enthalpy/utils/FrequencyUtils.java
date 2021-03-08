package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import uk.ac.cam.ceb.como.paper.enthalpy.json.input.SpeciesBean;

public class FrequencyUtils {

	
	
public LinkedHashMap<String, Integer> getCalculationFrequencyOfLevelOfTheory(LinkedHashMap<String, LinkedList<SpeciesBean>> casRegIDSpeciesMap) throws IOException{
		
		LinkedHashMap<String,Integer> frequencyMap = new LinkedHashMap<String, Integer>();
				
		for(Map.Entry<String, LinkedList<SpeciesBean>> casrmap :  casRegIDSpeciesMap.entrySet()) {
			
			for(SpeciesBean speciesB : casrmap.getValue()) {
				
				Integer frequencyCount = frequencyMap.get(speciesB.getLevelOfTheory());
				
				if(frequencyCount==null) {
					
					frequencyCount = 0;
				}
				
				frequencyMap.put(speciesB.getLevelOfTheory(), frequencyCount +1);		
			}
				
			}
		

		LinkedHashMap<String,Integer> sortedMap = new LinkedHashMap<String,Integer>();
		
		sortedMap.putAll(sortLevelOfTheoryFrequency(frequencyMap));
		
		

		for(Map.Entry<String, Integer> map: sortedMap.entrySet()) {
			
//			outputTxtFileWriter.write("-level of theory: " + map.getKey() + " frequency: " + map.getValue());
//			outputTxtFileWriter.write(System.getProperty("line.separator"));
			
		}

		return sortedMap;
	}

	
	
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param frequencyMap the linked hash map that constains level of theory as value and frequency of of level of theory appearance for all species.
	 * @return sorted frequency linked hash map.
	 */
	public LinkedHashMap<String, Integer> sortLevelOfTheoryFrequency(LinkedHashMap<String, Integer> frequencyMap){
		
		LinkedHashMap<String,Integer> sortedLinkedHashMap = new LinkedHashMap<String, Integer>();
		
		Set<Map.Entry<String, Integer>> frequencySet = frequencyMap.entrySet();
		
		LinkedList<Map.Entry<String, Integer>> linkedList = new LinkedList<Map.Entry<String,Integer>>(frequencySet);
		
		Collections.sort(linkedList, new Comparator<Map.Entry<String, Integer>>(){

			@Override
			public int compare(Entry<String, Integer> arg0, Entry<String, Integer> arg1) {
				// TODO Auto-generated method stub
				return arg1.getValue().compareTo(arg0.getValue());
			}
			
		});
		
		/**
		 * populate map with data stored in linked list.
		 */
		for(Map.Entry<String, Integer> map : linkedList) {
			
			sortedLinkedHashMap.put(map.getKey(), map.getValue());
		}
		
		return sortedLinkedHashMap;
		
	}

	
}
