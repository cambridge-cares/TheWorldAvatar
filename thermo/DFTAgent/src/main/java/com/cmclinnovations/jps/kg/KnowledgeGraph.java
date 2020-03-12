package com.cmclinnovations.jps.kg;

import java.util.ArrayList;

import com.cmclinnovations.jps.kg.object.model.Species;

/**
 * Defined to include the functionality to query and find those species</br>
 * that are not associated to any quantum chemistry calculations yet.  
 * 
 * @author msff2
 *
 */
public class KnowledgeGraph {
	
	private static String[] speciesSmile = new String[]{
			"C1C2C3C4CC2C1C34", 
			"C1C2C3CC4C1C4C23", 
			"C1C2CC1C1CC2C1", 
			"C1C2CC3CC(C3)C12",
			"C1CC11CC2CC1C2",
			"C1C2CC3CC3CC12",
			"C1C2CC3CC1C2C3",
			"C1C2CC3CC2CC13",
			"C1CC11CC2CC2C1",
			"C1C2CC3CCC3C12"
			};
	
	/**
	 * Forwards the call to the method that queries the OntoSpecies knowledge graph
	 * 
	 * @return
	 */
	public static ArrayList<Species> getSpeciesForCalculation(){
		return querySpeciesForCalculation();
	}
	
	public static ArrayList<Species> querySpeciesForCalculation(){
		ArrayList<Species> species = new ArrayList<Species>();
		for(String singleSpeciesSmile: speciesSmile){
			Species singleSpecies = new Species();
			singleSpecies.setSmile(singleSpeciesSmile);
			species.add(singleSpecies);
		}
		return species;
	}
}
