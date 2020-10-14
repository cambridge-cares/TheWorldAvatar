package uk.ac.cam.cares.jps.kg;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import javax.servlet.ServletException;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.web.util.UriUtils;

import uk.ac.cam.cares.jps.agent.configuration.AutoMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.Property;

public class OntoKinKG extends RepositoryManager {
	Logger logger = Logger.getLogger(OntoKinKG.class);
	private AutoMechCalibAgentProperty autoMechCalibAgentProperty;
	
//	public static void main(String[] args) throws ServletException, AutoMechCalibAgentException {
//		OntoKinKG ontoKinKG = new OntoKinKG();
//		String mechanismIRI = "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237";
//		ontoKinKG.queryNumOfReactions(mechanismIRI);
//	}
	
	public OntoKinKG(AutoMechCalibAgentProperty autoMechCalibAgentProperty) {
		this.autoMechCalibAgentProperty = autoMechCalibAgentProperty;
	}
	
	/**
	 * Reads the 
	 */
	public List<List<String>> queryNumOfReactions(String mechanismIRI) throws AutoMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		String queryString = formNumOfReactionsQuery(mechanismIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(autoMechCalibAgentProperty.getRdf4jServerURL(), 
				autoMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		return testResults;
	}
	
	public LinkedHashMap<String, String> queryAllReactions(String mechanismIRI) throws AutoMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		String queryString = formAllReactionsQuery(mechanismIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(autoMechCalibAgentProperty.getRdf4jServerURL(), 
				autoMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		for (List<String> rxn : testResults) {
			if (StringUtils.isNumeric(rxn.get(0))) {
				queriedReactionList.put(rxn.get(0), encodeReactionEquation(rxn.get(1)));
			}
		}
		return queriedReactionList;
	}
	
	public LinkedHashMap<String, String> queryReactionsToOptimise(String mechanismIRI, List<String> reactionIRIList) throws AutoMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		for (String reactionIRI : reactionIRIList) {
			if(!reactionIRI.trim().startsWith("<") && !reactionIRI.trim().endsWith(">")){
				reactionIRI = "<".concat(reactionIRI).concat(">");
			}
			String queryString = formReactionsToOptimiseQuery(reactionIRI);
			List<List<String>> testResults = RepositoryManager.queryRepository(autoMechCalibAgentProperty.getRdf4jServerURL(), 
					autoMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
			queriedReactionList.put(testResults.get(1).get(0), encodeReactionEquation(testResults.get(1).get(1)));
		}
		
		return queriedReactionList;
	}
	
	public LinkedHashMap<String, String> queryReactionBasedOnNo(String mechanismIRI, String reactionNo) throws AutoMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReaction = new LinkedHashMap<String, String>();
		String queryString = formReactionBasedOnNoQuery(mechanismIRI, reactionNo);
		List<List<String>> testResults = RepositoryManager.queryRepository(autoMechCalibAgentProperty.getRdf4jServerURL(), 
				autoMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		queriedReaction.put(testResults.get(1).get(0), encodeReactionEquation(testResults.get(1).get(1)));
		
		return queriedReaction;
	}
	
	public LinkedHashMap<String, String> queryRxnPreExpFactor(String mechanismIRI, List<String> reactionIRIList) throws AutoMechCalibAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		for (String reactionIRI : reactionIRIList) {
			if(!reactionIRI.trim().startsWith("<") && !reactionIRI.trim().endsWith(">")){
				reactionIRI = "<".concat(reactionIRI).concat(">");
			}
			String queryString = formRxnPreExpFactorQuery(reactionIRI);
			List<List<String>> testResults = RepositoryManager.queryRepository(autoMechCalibAgentProperty.getRdf4jServerURL(), 
					autoMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
			queriedReactionList.put(testResults.get(1).get(0), testResults.get(1).get(1));
		}
		
		return queriedReactionList;
	}
	
	public String queryMechanismIRI(String mechanismOwl) throws AutoMechCalibAgentException {
		if(mechanismOwl.trim().startsWith("<")){
			mechanismOwl = mechanismOwl.substring(1);
		}
		if(mechanismOwl.trim().endsWith(">")){
			mechanismOwl = mechanismOwl.substring(0,mechanismOwl.length()-1);
		}
		String queriedMechanismIRI = "";
		String queryString = formMechanismIRIQuery(mechanismOwl);
		List<List<String>> testResults = RepositoryManager.queryRepository(autoMechCalibAgentProperty.getRdf4jServerURL(), 
				autoMechCalibAgentProperty.getRdf4jRepositoryOntoKin(), queryString);
		queriedMechanismIRI = testResults.get(1).get(0);
		return queriedMechanismIRI;
	}
	
	private String formNumOfReactionsQuery(String mechanismIRI) throws AutoMechCalibAgentException {
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
	
	private String formAllReactionsQuery(String mechanismIRI) throws AutoMechCalibAgentException {
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
	
	private String formReactionsToOptimiseQuery(String reactionIRI) throws AutoMechCalibAgentException {
		String queryString = Property.PREFIX_BINDING_ONTOKIN.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_DC.getPropertyName());
		queryString = queryString.concat("SELECT ?reactionNo ?reactionEquation \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" dc:identifier ?reactionNo . \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" ontokin:hasEquation ?reactionEquation \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	private String formReactionBasedOnNoQuery(String mechanismIRI, String reactionNo) throws AutoMechCalibAgentException {
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
	
	private String formRxnPreExpFactorQuery(String reactionIRI) throws AutoMechCalibAgentException {
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
	
	private String formMechanismIRIQuery(String mechanismOwl) throws AutoMechCalibAgentException {
		String queryString = Property.PREFIX_BINDING_ONTOKIN.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_RDF.getPropertyName());
		queryString = queryString.concat(Property.PREFIX_BINDING_DC.getPropertyName());
		queryString = queryString.concat("SELECT ?MechanismIRI \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat("?MechanismIRI rdf:type ontokin:ReactionMechanism . \n");
		queryString = queryString.concat("    ").concat("FILTER regex(str(?MechanismIRI), \"").concat(mechanismOwl).concat("\", \"i\") \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
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
