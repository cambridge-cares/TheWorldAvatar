package uk.ac.cam.ceb.paper.sort;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 * 
 * @author nk510 (caresssd@hermes.cam.ac.uk)
 * @author am2145(am2145@cam.ac.uk)
 * 
 * Sorts hash maps for species and error bar used in cross validation algorithm. 
 *
 */
public class Sort {

	public static Map<Species,Double> sortingSpeciesMapComparingByValue(Map<Species,Double> speciesMap) {
		
		/**
		 * 
		 * Sorted hash map of rejected species in decreasing order comparing by error bar value. Works in Java 1.8 or higher.
		 * 
		 */
		Map<Species, Double> sortedSpeciesMap = speciesMap.entrySet()
				  .stream()
				  .sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
				  .collect(Collectors.toMap(
				    Map.Entry::getKey, 
				    Map.Entry::getValue, 
				    (oldValue, newValue) -> newValue, LinkedHashMap::new));
		
		return sortedSpeciesMap;
	}
	


}