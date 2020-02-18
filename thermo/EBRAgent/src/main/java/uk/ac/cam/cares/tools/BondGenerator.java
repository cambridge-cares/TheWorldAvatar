package uk.ac.cam.cares.tools;

import java.util.LinkedHashMap;

import java.util.Map.Entry;


public class BondGenerator {

	
	/**
	 * 
	 * @param updatedHashMap The hash map that contains index for each species selected from geometry. For example if geometry contains the following species name map (1,O) , (2,Cl) (3,O) then updated map will be (1, O1), (2, Cl1) and (3, O2).  
	 * @param index the index of species name in updated map.
	 * @return The species name that includes number of species (species index). For example Cl2. 
	 */
	public String getSpeciesName(LinkedHashMap<Integer, String> updatedHashMap, int index) {
		
		String speciesName="";
		
		for(Entry<Integer, String> updatedMap: updatedHashMap.entrySet()) {
			
			if(index == updatedMap.getKey()) {
				
				speciesName=updatedMap.getValue();

				return speciesName;
			}
			
		}
		
		return null;
	}
	
	
 
	
}
