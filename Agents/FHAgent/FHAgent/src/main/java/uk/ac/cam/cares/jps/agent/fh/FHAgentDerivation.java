package uk.ac.cam.cares.jps.agent.fh;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

/*Class for instantiating Derivation instances for the fumehood occupancy case
 * 
 */
public class FHAgentDerivation {
    private static final Logger LOGGER = LogManager.getLogger(FHAgentLauncher.class);
    //Derivation's base URL
    public String derivationInstanceBaseURL;
    //SPARQL enpoints (query, update)
    private String[] endPoint;
    //Init eror messate
    private final String ERROR_INIT = "Error on initialising derivation instantiation. ";

    //Clients for instantating the derivation isntances
    public DerivationClient derivClient;
    public RemoteStoreClient storeClient;

    //IRIs to be instantiated
    public String agentIRI;
    public Map<String, String> iriMap = new HashMap<>();
    public Map<String, String> RawToDerivedMap = new HashMap<>();
    
    private static final String ONTODEV = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final String ONTOAGE = "https://www.theworldavatar.com/kg/ontoagent/";
    private static final String ONTODERIV = "https://www.theworldavatar.com/kg/ontoderivation/";
    private static final String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public String occupiedStateTypeIRI = ONTODEV + "OccupiedState";
    public String lengthTypeIRI = OM + "Length" ;

    /*
     * Standard init for the class
     * @param agentPropFile location of the agent.properties
     * @param clientPropFile location of the client.properties
     * @param iriMappingFile location of the IRIMapper.txt
     * @param outputToIRIMap a map between the variable key and IRI of the occupiedState produced by the agent
     */
    public FHAgentDerivation(String agentPropFile, String clientPropFile, String iriMappingFile, Map<String, String> outputToIRIMap){
        try{
            readBaseIRI(agentPropFile);
            readSparqlEndpoint(clientPropFile);
            retrieveIRIMapping(agentPropFile, iriMappingFile, outputToIRIMap);

            storeClient = new RemoteStoreClient(endPoint[0], endPoint[1]);
            derivClient = new DerivationClient(storeClient, derivationInstanceBaseURL);

        }
        catch(Exception e){
            throw new JPSRuntimeException(ERROR_INIT + e);
        }

    }
    
    /*
     * Reads derivation.baseurl from properties file
     */
    private void readBaseIRI(String propertiesFile) throws IOException{
        try (InputStream input = new FileInputStream(propertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            try {
            // Read the mappings folder from the properties file
            derivationInstanceBaseURL = prop.getProperty("derivation.baseurl");
                if (derivationInstanceBaseURL == null){
                    throw new IOException("derivation.baseurl does not exist in properties file or is empty.");
                }
            }
            catch (Exception e) {
            	throw new IOException ("Error parsing derivation base URL from properties file.", e);
            }
        }
    }

    /*
     * Read SPARQL endpoint from the client.properties file
     */
    private void readSparqlEndpoint(String propertiesFile) throws IOException{
        try (InputStream input = new FileInputStream(propertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            try {
                // Read the mappings folder from the properties file
                endPoint = new String[]{prop.getProperty("sparql.query.endpoint"), prop.getProperty("sparql.update.endpoint")};
            }
            catch (Exception e) {
            	throw new IOException ("Error parsing derivation base URL from properties file.");
            }
        }
    }

    /*
     * Creates the IRI of the variables and agent instances
     * @param agentPropFile The location of the agent.properties file
     * @param outputToIRIMap a map between the variable key and IRI of the occupiedState produced by the agent
     */
    private void createIRI (String agentPropFile, Map<String, String> outputToIRIMap){
        try (InputStream input = new FileInputStream(agentPropFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            String derivationMappingString;
            try {
                // Read the mappings folder from the properties file
                derivationMappingString = prop.getProperty("derivation.mapping");
                }
                catch (NullPointerException e) {
                    throw new IOException ("The key derivation.mapping cannot be found in the properties file.");
                }

                if (derivationMappingString == null) {
                    throw new InvalidPropertiesFormatException("The properties file does not contain the key derivation.mapping " +
                            "with a path to the folder containing the required JSON key to IRI mappings.");
                }
                agentIRI = ONTOAGE + "FHAgent"+ UUID.randomUUID();
                
                for(String mappingString:derivationMappingString.split(",")){
                    String[] mapping = mappingString.split(":");
                    RawToDerivedMap.put(mapping[0], mapping[1]);
                    if(!iriMap.containsKey(mapping[0])){
                        iriMap.put(mapping[0], ONTODERIV+mapping[0]+UUID.randomUUID());
                    }
                    
                    if(!iriMap.containsKey(mapping[1])){
                        iriMap.put(mapping[1], outputToIRIMap.get(mapping[1]));
                    }
                }
            }
            catch(Exception e){
                throw new JPSRuntimeException("Failed to instantiate agent instances: " + e);
            }
    }

    /*
     * Save the IRI mapping to IRIMapper.txt
     * @param mappingFile The location of IRIMapper.txt
     */
    private void saveIRIMapping (String mappingFile) throws IOException {

        FileWriter fileWriter = new FileWriter(mappingFile);
        PrintWriter printWriter = new PrintWriter(fileWriter);
        
        
        for (String varName: iriMap.keySet()) {
            printWriter.print(varName+"="+iriMap.get(varName)+"\r\n");
        }

        printWriter.close();
        
    }

    /*
     * Retrieve IRI map between variable key and IRI form IRIMapper.txt. 
     * If the file does not exist yet, create new IRI and file instead.
     * @param agentPropFile location of the agent.properties
     * @param iriMappingFile location of the IRIMapper.txt
     * @param outputToIRIMap a map between the variable key and IRI of the occupiedState produced by the agent
     */
    private void retrieveIRIMapping (String agentPropFile, String mappingFile, Map<String, String> outputToIRIMap){
        File mapping = new File(mappingFile);
        if(mapping.exists()) { 
            try (FileReader fr = new FileReader(mapping);
                BufferedReader br = new BufferedReader(fr);) {
                String line;
                while ((line = br.readLine()) != null) {
                    String[] iriPair = line.split("=");
                    iriMap.put(iriPair[0], iriPair[1]);
                }
            } catch (IOException e) {
                throw new JPSRuntimeException("Failed to retrieve IRI map from mapping file: " + e);
            }
        }
        else {
            try{
                createIRI(agentPropFile, outputToIRIMap);
                saveIRIMapping(mappingFile);
            }
            catch (Exception e){
                throw new JPSRuntimeException("Failed to create IRI mapping: " + e);
            }
            
        }
    }

    /*
     * Create agent and derivatiopn instances for all derivation pair
     * @param agenURL the URL from which the agent is called
     */
    public void instantiateAgent(String agentURL) {
        if (agentURL.equals(null)){
            agentURL = derivationInstanceBaseURL;
        }
        try{
            for(String rawVar : RawToDerivedMap.keySet()){
                String derivVar = RawToDerivedMap.get(rawVar);
    
                String inputIRI = iriMap.get(rawVar);
                String outputIRI = iriMap.get(derivVar);
                
                try{
                    derivClient.createOntoAgentInstance(agentIRI, agentURL, Arrays.asList(lengthTypeIRI), Arrays.asList(occupiedStateTypeIRI));
                    derivClient.addTimeInstanceCurrentTimestamp(inputIRI);
                    String derivedString = derivClient.createDerivationWithTimeSeries(Arrays.asList(outputIRI), agentIRI, Arrays.asList(inputIRI));

                    LOGGER.info("Validating " + derivedString);
                    try {
                        if (derivClient.validateDerivations()) {
                            LOGGER.info("Validated derived difference successfully");
                        }
                    } catch (Exception e) {
                        LOGGER.error("Validation failure for derived difference" + e.getMessage());
                        throw new JPSRuntimeException(e);
                    }
                }
                catch (Exception e) {
                    if (!e.getMessage().contains("some entities are already part of another derivation")){
                        throw  new JPSRuntimeException("Failed to instantiate derivation instances", e);
                    }
                }
                
            }
        }
        
        catch(Exception e){
            throw new JPSRuntimeException("Failed to instantiate agent instances: ", e);
        }
    }

    //update the derivation latest update timestamp
    public void updateDerivationTimeStamp (String inputIRIString, String outputIRIString) {
        derivClient.updatePureSyncDerivations(Arrays.asList(inputIRIString, outputIRIString));
    }

    
}
