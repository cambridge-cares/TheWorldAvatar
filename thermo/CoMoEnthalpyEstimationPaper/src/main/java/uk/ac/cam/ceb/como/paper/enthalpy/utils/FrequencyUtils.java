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

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param listOfLevelOfTheory the level of theory.
	 * @return linked hash map that contains frequencies of appearance for all species (cas registry id).
	 */
	public LinkedHashMap<String, Integer> calculateFrequencyOfAllLevelOfTheory(LinkedList<SpeciesBean> listOfSpeciesBean){
		
		LinkedHashMap<String,Integer> frequencyMap = new LinkedHashMap<String, Integer>();
				
		for(SpeciesBean s: listOfSpeciesBean) {
			
			
			Integer frequencyCount = frequencyMap.get(s.getLevelOfTheory());
			
			if(frequencyCount==null) {
				
				frequencyCount = 0;
			}
			
			frequencyMap.put(s.getLevelOfTheory(), frequencyCount +1);
			
		}
				
		return frequencyMap;
	}
	
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param speciesBeanList the list of species beans that contains information about level of theory, ontocomchem IRI, ontospecies IRI.
	 * @param outputTxtFileWriter the file writer that stores results in txt file.
	 * @return the hash map that contains level of theory and its frequency. 
	 * @throws IOException the io exception. 
	 */
	public LinkedHashMap<String, Integer> getFrequencyOfLevelOfTheory(LinkedList<SpeciesBean> speciesBeanList, FileWriter outputTxtFileWriter ) throws IOException{
		
		LinkedHashMap<String, Integer> frequencyMap = new LinkedHashMap<String, Integer>();
		
		frequencyMap.putAll(calculateFrequencyOfAllLevelOfTheory(speciesBeanList));
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * 
		 * Sorting linked hash map that contains level of theory as key and the frequency of level of theory appearance for all species.
		 * 
		 */
		
		
		
		LinkedHashMap<String,Integer> sortedMap = new LinkedHashMap<String,Integer>();
		
		sortedMap.putAll(sortLevelOfTheoryFrequency(frequencyMap));
		
		/**
		 * Lists sorted map.
		 */
		for(Map.Entry<String, Integer> map: sortedMap.entrySet()) {
			
			outputTxtFileWriter.write("-level of theory: " + map.getKey() + " frequency: " + map.getValue());
			outputTxtFileWriter.write(System.getProperty("line.separator"));
			
		}

		outputTxtFileWriter.write(System.getProperty("line.separator"));
		
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

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @return a list of pairs ontocompchemIRI and ontospeciesIRI that has most frequent level of theory
	 * 
	 */
	public LinkedList<SpeciesBean> getIRI(LinkedHashMap<String, Integer> frequencyMap, LinkedHashMap<String, LinkedList<SpeciesBean>> casRegIDSpeciesMap ){
		
		LinkedList<SpeciesBean> iriList = new LinkedList<SpeciesBean>();
		
		for(Map.Entry<String, Integer> map: frequencyMap.entrySet()) {
		
		System.out.println("level of theory: " + map.getKey());
			
		for(Map.Entry<String, LinkedList<SpeciesBean>> casRegMap: casRegIDSpeciesMap.entrySet()) {
		
			System.out.println("car reg id: " + casRegMap.getKey());
			/**
			 * @author NK510 (caresssd@hermes.cam.ac.uk)
			 * 
			 * Iterates through list of species beans that contains level of theory, ontocompchem iri, ontospecies iri.
			 */
			for(SpeciesBean speciesBean : casRegMap.getValue()) {
				
				System.out.println(speciesBean.getLevelOfTheory()+ " = " + map.getKey());
				
				if(speciesBean.getLevelOfTheory().equalsIgnoreCase(map.getKey())) {
					
					SpeciesBean bean = new SpeciesBean(speciesBean.getOntocompchemIRI(),speciesBean.getOntospeciesIRI());
				
					System.out.println("ontocompchem: " + speciesBean.getOntocompchemIRI());
					System.out.println("ontospecies: " + speciesBean.getOntospeciesIRI());
					System.out.println();
					
					iriList.add(bean);
					
			        }
				
				break;
		}
		
		}
		
System.out.println();
		
		}
		
return iriList;
	
	}
}
