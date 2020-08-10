package uk.ac.cam.cares.jps.kg;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import javax.servlet.ServletException;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.web.util.UriUtils;

import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.Property;

public class OntoKinKG extends RepositoryManager {
	Logger logger = Logger.getLogger(OntoKinKG.class);
	public static final String RDF = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n";
	public static final String RDFS = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n";
	public static final String DC = "PREFIX dc: <http://purl.org/dc/elements/1.1/> \n";
	public static final String REACTION_MECHANISM = "PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#> \n";
	
	public static void main(String[] args) throws ServletException, MoDSAgentException {
		OntoKinKG ontoKinKG = new OntoKinKG();
		String mechanismIRI = "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237";
		ontoKinKG.queryNumOfReactions(mechanismIRI);
	}
	
//	
	/**
	 * Reads the 
	 */
	public List<List<String>> queryNumOfReactions(String mechanismIRI) throws MoDSAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		String queryString = formNumOfReactionsQuery(Property.PREFIX_BINDING_ONTOKIN.getPropertyName(), mechanismIRI);
		List<List<String>> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
				Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName(), queryString);
		return testResults;
	}
	
	public LinkedHashMap<String, String> queryAllReactions(String mechanismIRI) throws MoDSAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		String queryString = formAllReactionsQuery(Property.PREFIX_BINDING_ONTOKIN.getPropertyName(), mechanismIRI);
		List<List<String>> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
				Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName(), queryString);
		for (List<String> rxn : testResults) {
			if (StringUtils.isNumeric(rxn.get(0))) {
				queriedReactionList.put(rxn.get(0), encodeReactionEquation(rxn.get(1)));
			}
		}
		return queriedReactionList;
	}
	
	public LinkedHashMap<String, String> queryReactionsToOptimise(String mechanismIRI, List<String> reactionIRIList) throws MoDSAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReactionList = new LinkedHashMap<String, String>();
		for (String reactionIRI : reactionIRIList) {
			if(!reactionIRI.trim().startsWith("<") && !reactionIRI.trim().endsWith(">")){
				reactionIRI = "<".concat(reactionIRI).concat(">");
			}
			String queryString = formReactionsToOptimiseQuery(Property.PREFIX_BINDING_ONTOKIN.getPropertyName(), reactionIRI);
			List<List<String>> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
					Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName(), queryString);
			queriedReactionList.put(testResults.get(1).get(0), encodeReactionEquation(testResults.get(1).get(1)));
		}
		
		return queriedReactionList;
	}
	
	public LinkedHashMap<String, String> queryReactionBasedOnNo(String mechanismIRI, String reactionNo) throws MoDSAgentException {
		if(!mechanismIRI.trim().startsWith("<") && !mechanismIRI.trim().endsWith(">")){
			mechanismIRI = "<".concat(mechanismIRI).concat(">");
		}
		LinkedHashMap<String, String> queriedReaction = new LinkedHashMap<String, String>();
		String queryString = formReactionBasedOnNoQuery(Property.PREFIX_BINDING_ONTOKIN.getPropertyName(), mechanismIRI, reactionNo);
		List<List<String>> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
				Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName(), queryString);
		queriedReaction.put(testResults.get(1).get(0), encodeReactionEquation(testResults.get(1).get(1)));
		
		return queriedReaction;
	}
	
	private String formNumOfReactionsQuery(String prefixBindingOntoKin, String mechanismIRI) throws MoDSAgentException {
		String queryString = prefixBindingOntoKin;
		queryString = queryString.concat(REACTION_MECHANISM);
		queryString = queryString.concat(RDF);
		queryString = queryString.concat("SELECT (COUNT(?reaction) AS ?numOfReactions) \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ?reaction rdf:type reaction_mechanism:ChemicalReaction . \n");
		queryString = queryString.concat("    ?reaction ontokin:belongsToPhase ?phase . \n");
		queryString = queryString.concat("    ?phase rdf:type ontokin:GasPhase . \n");
		queryString = queryString.concat("    ?phase ontokin:containedIn ").concat(mechanismIRI).concat(" \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	private String formAllReactionsQuery(String prefixBindingOntoKin, String mechanismIRI) throws MoDSAgentException {
		String queryString = prefixBindingOntoKin;
		queryString = queryString.concat(REACTION_MECHANISM);
		queryString = queryString.concat(RDF);
		queryString = queryString.concat(DC);
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
	
	private String formReactionsToOptimiseQuery(String prefixBindingOntoKin, String reactionIRI) throws MoDSAgentException {
		String queryString = prefixBindingOntoKin;
		queryString = queryString.concat(DC);
		queryString = queryString.concat("SELECT ?reactionNo ?reactionEquation \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" dc:identifier ?reactionNo . \n");
		queryString = queryString.concat("    ").concat(reactionIRI).concat(" ontokin:hasEquation ?reactionEquation \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	private String formReactionBasedOnNoQuery(String prefixBindingOntoKin, String mechanismIRI, String reactionNo) throws MoDSAgentException {
		String queryString = prefixBindingOntoKin;
		queryString = queryString.concat(REACTION_MECHANISM);
		queryString = queryString.concat(RDF);
		queryString = queryString.concat(DC);
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
