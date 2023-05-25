package uk.ac.cam.cares.jps.kg;

import java.io.UnsupportedEncodingException;
import java.util.LinkedHashMap;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.web.util.UriUtils;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.moo.Property;

public class OntoKinKG {
	Logger logger = Logger.getLogger(OntoKinKG.class);
	private MoDSMooAgentProperty MoDSMooAgentProperty;
	
	public OntoKinKG(MoDSMooAgentProperty MoDSMooAgentProperty) {
		this.MoDSMooAgentProperty = MoDSMooAgentProperty;
	}
	
	/**
	 * Query number of reactions in a given mechanism. 
	 * 
	 * @param mechanismIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public List<List<String>> queryNumOfReactions(String mechanismIRI) throws MoDSMooAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		String queryString = formNumOfReactionsQuery(mechanismIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		return testResults;
	}
	
	/**
	 * Query all reactions in a given mechanism. 
	 * 
	 * @param mechanismIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public LinkedHashMap<String, String> queryAllReactions(String mechanismIRI) throws MoDSMooAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		String queryString = formAllReactionsQuery(mechanismIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
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
	 * @throws MoDSMooAgentException
	 */
	public LinkedHashMap<String, String> queryReactionsToOptimise(String mechanismIRI, List<String> reactionIRIList) throws MoDSMooAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		for (String reactionIRI : reactionIRIList) {
			if(!reactionIRI.trim().startsWith("<") && !reactionIRI.trim().endsWith(">")){
				reactionIRI = "<".concat(reactionIRI).concat(">");
			}
			String queryString = formReactionsToOptimiseQuery(reactionIRI);
			List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
					MoDSMooAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
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
	 * @throws MoDSMooAgentException
	 */
	public LinkedHashMap<String, String> queryReactionBasedOnNo(String mechanismIRI, String reactionNo) throws MoDSMooAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReaction = new LinkedHashMap<String, String>();
		String queryString = formReactionBasedOnNoQuery(mechanismIRI, reactionNo);
		List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		queriedReaction.put(testResults.get(1).get(0), encodeReactionEquation(testResults.get(1).get(1)));
		
		return queriedReaction;
	}
	
	/**
	 * Download a mechanism given its name. 
	 * 
	 * @param aboxFileName
	 * @param contextURL
	 * @param aboxFilePath
	 * @throws MoDSMooAgentException
	 * @throws OntoException
	 */
	public void downloadMechanism(String aboxFileName, String contextURL, String aboxFilePath) throws MoDSMooAgentException, OntoException {
		RepositoryManager.downloadOntology(MoDSMooAgentProperty.getRdf4jServerURL(), aboxFileName, contextURL, 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoKin(), aboxFilePath);
	}
	
	/**
	 * Form the query string for number of reactions. 
	 * 
	 * @param mechanismIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	private String formNumOfReactionsQuery(String mechanismIRI) throws MoDSMooAgentException {
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
	 * @throws MoDSMooAgentException
	 */
	private String formAllReactionsQuery(String mechanismIRI) throws MoDSMooAgentException {
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
	 * @throws MoDSMooAgentException
	 */
	private String formReactionsToOptimiseQuery(String reactionIRI) throws MoDSMooAgentException {
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
	 * @param mechanismIRI
	 * @param reactionNo
	 * @return
	 * @throws MoDSMooAgentException
	 */
	private String formReactionBasedOnNoQuery(String mechanismIRI, String reactionNo) throws MoDSMooAgentException {
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
