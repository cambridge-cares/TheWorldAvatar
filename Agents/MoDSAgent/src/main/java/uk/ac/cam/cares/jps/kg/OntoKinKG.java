package uk.ac.cam.cares.jps.kg;

import java.io.UnsupportedEncodingException;
import java.util.LinkedHashMap;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.web.util.UriUtils;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.Property;

public class OntoKinKG {
	Logger logger = Logger.getLogger(OntoKinKG.class);
	private MoDSMechCalibAgentProperty modsMechCalibAgentProperty;
	
	public OntoKinKG(MoDSMechCalibAgentProperty modsMechCalibAgentProperty) {
		this.modsMechCalibAgentProperty = modsMechCalibAgentProperty;
	}
	
	/**
	 * Query number of reactions in a given mechanism. 
	 * 
	 * @param mechanismIRI
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	public List<List<String>> queryNumOfReactions(String mechanismIRI) throws MoDSMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		String queryString = formNumOfReactionsQuery(mechanismIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(modsMechCalibAgentProperty.getRdf4jServerURL(), 
				modsMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		return testResults;
	}
	
	/**
	 * Query all reactions in a given mechanism. 
	 * 
	 * @param mechanismIRI
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	public LinkedHashMap<String, String> queryAllReactions(String mechanismIRI) throws MoDSMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		String queryString = formAllReactionsQuery(mechanismIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(modsMechCalibAgentProperty.getRdf4jServerURL(), 
				modsMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		for (List<String> rxn : testResults) {
			if (StringUtils.isNumeric(rxn.get(0))) {
				queriedReactionList.put(rxn.get(0), encodeReactionEquation(rxn.get(1)));
			}
		}
		return queriedReactionList;
	}
	
	/**
	 * Query the reaction equations for optimisation given a mechanism and a list of reactions. 
	 * 
	 * @param mechanismIRI
	 * @param reactionIRIList
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	public LinkedHashMap<String, String> queryReactionsToOptimise(String mechanismIRI, List<String> reactionIRIList) throws MoDSMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		for (String reactionIRI : reactionIRIList) {
			if(!reactionIRI.trim().startsWith("<") && !reactionIRI.trim().endsWith(">")){
				reactionIRI = "<".concat(reactionIRI).concat(">");
			}
			String queryString = formReactionsToOptimiseQuery(reactionIRI);
			List<List<String>> testResults = RepositoryManager.queryRepository(modsMechCalibAgentProperty.getRdf4jServerURL(), 
					modsMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
			queriedReactionList.put(testResults.get(1).get(0), encodeReactionEquation(testResults.get(1).get(1)));
		}
		
		return queriedReactionList;
	}
	
	/**
	 * Query reaction equation given its sequence number. 
	 * 
	 * @param mechanismIRI
	 * @param reactionNo
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	public LinkedHashMap<String, String> queryReactionBasedOnNo(String mechanismIRI, String reactionNo) throws MoDSMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReaction = new LinkedHashMap<String, String>();
		String queryString = formReactionBasedOnNoQuery(mechanismIRI, reactionNo);
		List<List<String>> testResults = RepositoryManager.queryRepository(modsMechCalibAgentProperty.getRdf4jServerURL(), 
				modsMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		queriedReaction.put(testResults.get(1).get(0), encodeReactionEquation(testResults.get(1).get(1)));
		
		return queriedReaction;
	}
	
	/**
	 * Query pre-exponential factor given a reaction. 
	 * 
	 * @param mechanismIRI
	 * @param reactionIRIList
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	public LinkedHashMap<String, String> queryRxnPreExpFactor(String mechanismIRI, List<String> reactionIRIList) throws MoDSMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		for (String reactionIRI : reactionIRIList) {
			if(!reactionIRI.trim().startsWith("<") && !reactionIRI.trim().endsWith(">")){
				reactionIRI = "<".concat(reactionIRI).concat(">");
			}
			String queryString = formRxnPreExpFactorQuery(reactionIRI);
			List<List<String>> testResults = RepositoryManager.queryRepository(modsMechCalibAgentProperty.getRdf4jServerURL(), 
					modsMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
			queriedReactionList.put(testResults.get(1).get(0), testResults.get(1).get(1));
		}
		
		return queriedReactionList;
	}
	
	/**
	 * Download a mechanism given its name. 
	 * 
	 * @param aboxFileName
	 * @param contextURL
	 * @param aboxFilePath
	 * @throws MoDSMechCalibAgentException
	 * @throws OntoException
	 */
	public void downloadMechanism(String aboxFileName, String contextURL, String aboxFilePath) throws MoDSMechCalibAgentException, OntoException {
		RepositoryManager.downloadOntology(modsMechCalibAgentProperty.getRdf4jServerURL(), aboxFileName, contextURL, 
				modsMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), aboxFilePath);
	}
	
	/**
	 * Upload a mechanism to a given server. 
	 * 
	 * @param aboxFileName
	 * @param aboxFilePath
	 * @return
	 * @throws MoDSMechCalibAgentException
	 * @throws OntoException
	 */
	public String uploadMechanism(String aboxFileName, String aboxFilePath) throws MoDSMechCalibAgentException, OntoException {
		RepositoryManager.loadOntology(modsMechCalibAgentProperty.getKgUploadRdf4jServerUrl(), aboxFileName, aboxFilePath, 
				Property.RDF4J_ONTOKIN_KB_URL.getPropertyName(), modsMechCalibAgentProperty.getKgUploadRdf4jRepositoryOntoKin());
		return Property.RDF4J_ONTOKIN_KB_URL.getPropertyName().concat(aboxFileName);
	}
	
	/**
	 * Delete a mechanism from kg. 
	 * @param mechanismName
	 * @param contextURL
	 * @return
	 */
	public boolean deleteMechanism(String mechanismName, String contextURL) {
		try {
			RepositoryManager.deleteOntology(modsMechCalibAgentProperty.getKgUploadRdf4jServerUrl(), 
					mechanismName, contextURL, modsMechCalibAgentProperty.getKgUploadRdf4jRepositoryOntoKin());
			return true;
		} catch (OntoException e) {
			e.printStackTrace();
		}
		return false;
	}
	
	/**
	 * Form the query string for number of reactions. 
	 * 
	 * @param mechanismIRI
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	private String formNumOfReactionsQuery(String mechanismIRI) throws MoDSMechCalibAgentException {
		String queryString = Property.PREFIX_BINDING_ONTOKIN.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_REACTION_MECHANISM.getPropertyName());
		queryString = queryString.concat(Property.PREFIX_BINDING_RDF.getPropertyName());
		queryString = queryString.concat("SELECT (COUNT(?reaction) AS ?numOfReactions) \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ?reaction rdf:type reaction_mechanism:ChemicalReaction . \n");
		queryString = queryString.concat("    ?reaction ontokin:belongsToPhase ?phase . \n");
		queryString = queryString.concat("    ?phase rdf:type ontokin:GasPhase . \n");
		queryString = queryString.concat("    ?phase ontokin:containedIn ").concat(mechanismIRI).concat(" \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Form the query string for list of all reactions. 
	 * 
	 * @param mechanismIRI
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	private String formAllReactionsQuery(String mechanismIRI) throws MoDSMechCalibAgentException {
		String queryString = Property.PREFIX_BINDING_ONTOKIN.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_REACTION_MECHANISM.getPropertyName());
		queryString = queryString.concat(Property.PREFIX_BINDING_RDF.getPropertyName());
		queryString = queryString.concat(Property.PREFIX_BINDING_DC.getPropertyName());
		queryString = queryString.concat("SELECT ?No ?Equation \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ?reaction rdf:type reaction_mechanism:ChemicalReaction . \n");
		queryString = queryString.concat("    ?reaction ontokin:belongsToPhase ?phase . \n");
		queryString = queryString.concat("    ?phase rdf:type ontokin:GasPhase . \n");
		queryString = queryString.concat("    ?phase ontokin:containedIn ").concat(mechanismIRI).concat(" . \n");
		queryString = queryString.concat("    ?reaction dc:identifier ?No . \n");
		queryString = queryString.concat("    ?reaction ontokin:hasEquation ?Equation \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Form the query string for list of reactions to be optimised. 
	 * 
	 * @param reactionIRI
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	private String formReactionsToOptimiseQuery(String reactionIRI) throws MoDSMechCalibAgentException {
		String queryString = Property.PREFIX_BINDING_ONTOKIN.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_DC.getPropertyName());
		queryString = queryString.concat("SELECT ?reactionNo ?reactionEquation \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" dc:identifier ?reactionNo . \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" ontokin:hasEquation ?reactionEquation \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Form the query string for reaction equation given its sequence number. 
	 * 
	 * @param mechanismIRI
	 * @param reactionNo
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	private String formReactionBasedOnNoQuery(String mechanismIRI, String reactionNo) throws MoDSMechCalibAgentException {
		String queryString = Property.PREFIX_BINDING_ONTOKIN.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_REACTION_MECHANISM.getPropertyName());
		queryString = queryString.concat(Property.PREFIX_BINDING_RDF.getPropertyName());
		queryString = queryString.concat(Property.PREFIX_BINDING_DC.getPropertyName());
		queryString = queryString.concat("SELECT ?Reaction ?Equation \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ?Reaction rdf:type reaction_mechanism:ChemicalReaction . \n");
		queryString = queryString.concat("    ?Reaction ontokin:belongsToPhase ?phase . \n");
		queryString = queryString.concat("    ?phase rdf:type ontokin:GasPhase . \n");
		queryString = queryString.concat("    ?phase ontokin:containedIn ").concat(mechanismIRI).concat(" . \n");
		queryString = queryString.concat("    ?Reaction dc:identifier \"").concat(reactionNo).concat("\" . \n");
		queryString = queryString.concat("    ?Reaction ontokin:hasEquation ?Equation \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Form the query string for pre-exponential factor given a reaction. 
	 * @param reactionIRI
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	private String formRxnPreExpFactorQuery(String reactionIRI) throws MoDSMechCalibAgentException {
		String queryString = Property.PREFIX_BINDING_ONTOKIN.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_DC.getPropertyName());
		queryString = queryString.concat("SELECT ?ReactionNo ?PreExpFactor \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" dc:identifier ?ReactionNo . \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" ontokin:hasArrheniusCoefficient ?arrhCoef . \n");
		queryString = queryString.concat("    ").concat("?arrhCoef ontokin:hasPreExponentialFactor ?preE . \n");
		queryString = queryString.concat("    ").concat("?arrhCoef ontokin:hasPreExponentialFactorUnits ?preEU \n");
		queryString = queryString.concat("    ").concat("    BIND(CONCAT(?preE, \"_\", ?preEU) AS ?PreExpFactor) \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Encode a reaction equation into URL. 
	 * 
	 * @param equation
	 * @return
	 */
	private String encodeReactionEquation(String equation) {
	    try {
	    	equation = UriUtils.encodePath(equation, "UTF-8")
	    			.replace("=", "%3D")
	    			.replace("+", "%2B")
	    			.replace("*", "%2A")
	    			.replace("(", "%28")
	    			.replace(")", "%29");
	    } catch (UnsupportedEncodingException e) {
	        logger.error("Error encoding parameter {}"+e.getMessage());
	    }
	    return equation;
	}
}
