package com.cmclinnovations.jps.agent.caller;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Set;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.jps.agent.caller.configuration.AgentCallerConfiguration;
import com.cmclinnovations.jps.agent.caller.configuration.DFTAgentCallerProperty;
import com.cmclinnovations.jps.kg.query.OntoCompChemQuery;
import com.cmclinnovations.jps.kg.query.OntoSpeciesQuery;

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
	
	public static void main(String[] args) throws Exception{
		AgentCaller agentCaller = new AgentCaller();
		Set<String> speciesToRunDFTCalculations = agentCaller.getSpeciesToRunDFTCalculation();
		for(String speciesToRunDFTCalculation: speciesToRunDFTCalculations){
			System.out.println(speciesToRunDFTCalculation);
		}
	} 
	
	public Set<String> getSpeciesToRunDFTCalculation() throws DFTAgentCallerException, Exception{
		Set<String> speciesToRunDFTCalculation = getAllSpecies();
		speciesToRunDFTCalculation.removeAll(getAlreadyCalculatedSpecies());
		return speciesToRunDFTCalculation;
	}
	
	public Set<String> getAlreadyCalculatedSpecies() throws DFTAgentCallerException, Exception{
		OntoCompChemQuery ontoCompChemQuery = new OntoCompChemQuery();
		return ontoCompChemQuery.queryOntoCompChemKG();
	}
	
	public Set<String> getAllSpecies() throws Exception{
		OntoSpeciesQuery ontoSpeciesQuery = new OntoSpeciesQuery();
		return ontoSpeciesQuery.queryOntoSpciesKG();
	}
	
	/**
	 * Enables to perform an HTTP get request.
	 * 
	 * @param query
	 * @return
	 * @throws MalformedURLException
	 * @throws IOException
	 */
	public static String get(String query) throws MalformedURLException, IOException{
        URL httpURL = new URL(query);
        URLConnection httpURLConnection = httpURL.openConnection();
        BufferedReader in = new BufferedReader(
                                new InputStreamReader(
                                		httpURLConnection.getInputStream()));
        String inputLine;
        String fileContent = "";
        while ((inputLine = in.readLine()) != null){ 
            fileContent = fileContent.concat(inputLine);
        }
        in.close();
        System.out.println("fileContent:\n"+fileContent);
        return fileContent;
    }

}
