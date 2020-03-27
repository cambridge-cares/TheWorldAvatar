package com.cmclinnovations.jps.agent.caller;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.jps.agent.caller.configuration.AgentCallerConfiguration;
import com.cmclinnovations.jps.agent.caller.configuration.DFTAgentCallerProperty;

/**
 * This works in combination with a script developed for calling DFT Agent<br>
 * to run calculations of a set of species residing in the OntoSpecies kno-<br>
 * wledge graph stored in multiple knowledge bases. The set contains those<br>
 * species that are not linked to any quantum chemistry calculations stored<br>
 * in the OntoCompChem knowledge graph.
 *  
 * @author msff2
 *
 */
public class AgentCaller {

	public static ApplicationContext applicationContext;
	public static DFTAgentCallerProperty dftAgentCallerProperty;
	
	/**
	 * The default constructor of this class.
	 */
	public AgentCaller(){
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(AgentCallerConfiguration.class);
		}
		if (dftAgentCallerProperty == null) {
			dftAgentCallerProperty = applicationContext.getBean(DFTAgentCallerProperty.class);
		}
	}
	
	public static void main(String[] args) {
		AgentCaller agentCaller = new AgentCaller();
		HashSet<String> speciesToRunDFTCalculation = agentCaller.getSpeciesToRunDFTCalculation();
	} 
	
	public HashSet<String> getSpeciesToRunDFTCalculation(){
		HashSet<String> speciesToRunDFTCalculation = getAllSpecies();
		speciesToRunDFTCalculation.removeAll(getAlreadyCalculatedSpecies());
		return speciesToRunDFTCalculation;
	}
	
	public HashSet<String> getAlreadyCalculatedSpecies(){
		return null;
	}
	
	public HashSet<String> getAllSpecies(){
		return null;
	}

}
