package com.cmclinnovations.jps.agent.caller;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.Set;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
	private static Logger logger = LoggerFactory.getLogger(AgentCaller.class);
	/**
	 * An instance of the base class that supports the annotation based<br>
	 * reading of attributes from a property file.  
	 */
	public static ApplicationContext applicationContext;
	/**
	 * An instance of the class that reads attributes from the dft-agent-<br>
	 * caller.properties file using annotations.
	 */
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
	
	/**
	 * The main method of this class created to initiate the call from a<br>
	 * bash script or batch file to run DFT calculations for those species<br>
	 * that are not connected to any calculations. The species belong to<br>
	 * the OntoSpecies knowledge graph and the calculations belong to the<br>
	 * OntoCompChem knowledge graph.
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception{
		AgentCaller agentCaller = new AgentCaller();
		Set<String> speciesToRunDFTCalculations = agentCaller.getSpeciesToRunDFTCalculation();
		for(String speciesToRunDFTCalculation: speciesToRunDFTCalculations){
			System.out.println(speciesToRunDFTCalculation);
			String httpRequest = agentCaller.produceHTTPRequest(speciesToRunDFTCalculation);
			System.out.println(httpRequest);
			agentCaller. performHTTPRequest(httpRequest);
			logger.info("DFTAgentCaller: a job has been submitted.");
			System.out.println("DFTAgentCaller: a job has been submitted.");
		}
	} 
	
	/**
	 * Produces the complete HTTP request that consists of two parts for<br>
	 * submitting a DFT job for the current species.<br>
	 * 
	 * 1. The first part containing the HTTP protocol, server address, service<br>
	 * address and input variable, i.e. i.e. http://localhost:8080/DFTAgent/job/request?input=
	 * 
	 * 2. The last part containing the JSON input, e.g. 
	 * {"job":{"levelOfTheory":"B3LYP/6-31G(d)","keyword":"Opt","algorithmChoice":"Freq"},
	 * "speciesIRI":"http://www.theworldavatar.com/kb/ontospecies/00f46355-
	 * 2ea4-3ef1-b61a-e3d87e91a8db.owl#00f46355-2ea4-3ef1-b61a-e3d87e91a8db"}
	 * 
	 * @param speciesIRI
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	public String produceHTTPRequest(String speciesIRI) throws UnsupportedEncodingException{
		return getHTTPRequestFirstPart().concat(URLEncoder.encode(getJSONInputPart(speciesIRI), "UTF-8"));
	}
	
	/**
	 * The protocol, address, service address and input variable part of<br>
	 * the complete HTTP request.
	 * 
	 * @return
	 */
	private String getHTTPRequestFirstPart(){
		return dftAgentCallerProperty.getHttpRequestFirstPart();
	}
	
	/**
	 * The JSON input part of the complete HTTP request.
	 * 
	 * @param speciesIRI
	 * @return
	 */
	private String getJSONInputPart(String speciesIRI){
		return generateJSONInput(speciesIRI).toString();
	}
	
	/**
	 * Generates the JSON input for creating a DFT job request for the<br>
	 * current species. It reads the properties related to the DFT job<br>
	 * from the dft-agent-caller.properties file. 
	 * 
	 * @param speciesIRIInput
	 * @return
	 */
	private JSONObject generateJSONInput(String speciesIRIInput){
		JSONObject job = new JSONObject();
		job.put(dftAgentCallerProperty.getJobLevelOfTheoryPropertyLabel(), dftAgentCallerProperty.getJobLevelOfTheory());
		job.put(dftAgentCallerProperty.getJobKeywordPropertyLabel(), dftAgentCallerProperty.getJobKeyword());
		job.put(dftAgentCallerProperty.getJobAlgorithmChoicePropertyLabel(), dftAgentCallerProperty.getJobAlgorithmChoice());
		JSONObject json = new JSONObject();
		json.put(dftAgentCallerProperty.getJobPropertyLabel(), job);
		json.put(dftAgentCallerProperty.getJobSpeciesIRIPropertyLabel(), speciesIRIInput);
		return json;
	}
	
	/**
	 * Extracts those species from the OntoSpcies knowledge graph that are<br>
	 * not connected to any calculations in the OntoCompChem knowledge graph.
	 * 
	 * @return
	 * @throws DFTAgentCallerException
	 * @throws Exception
	 */
	public Set<String> getSpeciesToRunDFTCalculation() throws DFTAgentCallerException, Exception{
		Set<String> speciesToRunDFTCalculation = getAllSpecies();
		speciesToRunDFTCalculation.removeAll(getAlreadyCalculatedSpecies());
		return speciesToRunDFTCalculation;
	}
	
	/**
	 * Extracts the species, from the OntoCompChem knowledge graph, for<br> 
	 * which a calculations was performed.
	 * 
	 * @return
	 * @throws DFTAgentCallerException
	 * @throws Exception
	 */
	public Set<String> getAlreadyCalculatedSpecies() throws DFTAgentCallerException, Exception{
		OntoCompChemQuery ontoCompChemQuery = new OntoCompChemQuery();
		return ontoCompChemQuery.queryOntoCompChemKG();
	}
	
	/**
	 * Extracts all species from the OntoSpcies knowledge graph.
	 * 
	 * @return
	 * @throws Exception
	 */
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
	public static String performHTTPRequest(String query) throws MalformedURLException, IOException{
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
