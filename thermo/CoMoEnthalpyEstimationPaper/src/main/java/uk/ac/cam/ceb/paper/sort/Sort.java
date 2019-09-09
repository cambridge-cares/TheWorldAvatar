package uk.ac.cam.ceb.paper.sort;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 * 
 * @author nk510
 * Sorts hash maps for species and error bar used in cross validation algorithm. 
 *
 */
public class Sort {

	public static HashMap<Species,Double> sortSpeciesErrorBarMapByValue(Map<Species,Double> speciesMap) {
		
		/**
		 * @author nk510
		 * Convert HashMap for species and error bar into linked list 
		 * 
		 */
		List<Map.Entry<Species, Double>> speciesList = new LinkedList<Map.Entry<Species,Double>>(speciesMap.entrySet());
		
		
		/**
		 * @author nk510
		 * Sorted linked list of species and error bar.
		 */
		Collections.sort(speciesList, new Comparator<Map.Entry<Species,Double>>(){

			@Override
			public int compare(Entry<Species, Double> o1, Entry<Species, Double> o2) {
				// TODO Auto-generated method stub
				
				return (o1.getValue().compareTo(o2.getValue()));
			}
	});
		
		/**
		 * @author nk510
		 * Convert sorted linked list into hash map for species and error bar.
		 */
		HashMap<Species,Double> sortedSpeciesMap = new HashMap<Species,Double>();
		
		for(Map.Entry<Species, Double> species : speciesList) {
			
			sortedSpeciesMap.put(species.getKey(), species.getValue());
		}
	
		return sortedSpeciesMap;
	}

	

	
}
