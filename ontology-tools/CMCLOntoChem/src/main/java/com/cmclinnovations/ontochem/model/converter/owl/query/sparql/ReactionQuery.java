package com.cmclinnovations.ontochem.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Arrhenius;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientE;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientP;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientb;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Coverage;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterE;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterM;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Efficiencies;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.FallOff;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTeller;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTellerCoefficientB;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTellerCoefficientC;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.RateCoefficient;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Reaction;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ReactionData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ReactionOrder;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMin;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.RateCoeffFloatArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMin;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.utils.OwlConverterUtils;

/**
 * Implements the methods that are used to perform SPARQL queries on top of
 * the OWL representation of a chemical mechanism ontology to extract
 * data and metadata about reactions to codify them in CTML.  
 * 
 * @author msff2
 *
 */
public class ReactionQuery extends OwlConverter implements IReactionQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ReactionQuery.class);
	/**
	 * Queries the instances of the classes ReactionMetadata and 
	 * ChemicalReaction. The way it does it is to query the instances of
	 * reaction metadata which is followed by the query of reactions attached
	 * to each metadata instance.
	 */
	public void query(){
		queriedReactionDataList = new ArrayList<ReactionData>();
		ctmlMD.setReactionData(queriedReactionDataList);
		queryReactionsMetadataAndReactions();
	}
	
	/**
	 * Queries the instances of the classes ReactionMetadata and 
	 * ChemicalReaction. The way it does it is to query the instances of
	 * reaction metadata which is followed by the query of reactions attached
	 * to each metadata instance.
	 */
	private void queryReactionsMetadataAndReactions(){
		queryInstance(appConfigOntokin.getReactionMetadata());
		if (queryResult != null && !queryResult.isEmpty()) {
			// To maintain the order of reaction metadata codification in CTML
			// it sorts the list of reaction metadata instance ids
			Collections.sort(queryResult);
			ArrayList<String> reactionMetadataIds = queryResult;
			queryResult = new ArrayList<String>();
			for(String reactionMetadataId:reactionMetadataIds){
				reactionDataInOwl = new ReactionData();
				queriedReactionDataList.add(reactionDataInOwl);
				// Setting reaction metadata in the following method allows to
				// reduce the amount of code from this method
				setReactionMetadata(reactionDataInOwl, reactionMetadataId);
				reactionInOwlList = new ArrayList<Reaction>();
				reactionDataInOwl.setReaction(reactionInOwlList);
				queryReactions(reactionMetadataId);
			}
		}
	}
	
	/**
	 * Queries reactions attached to an instance of metadata
	 *  
	 * @param reactionMetadataId
	 */
	private void queryReactions(String reactionMetadataId) {
		// Queries and retrieves IRIs of reactions attached to
		// the instance of the current set of metadata
		queryOnlyReactions(reactionMetadataId);
		if (queryResult != null && !queryResult.isEmpty()) {
			// To maintain the order of reaction codification in CTML it
			// sorts the list of reaction ids
			Collections.sort(queryResult);
			ArrayList<String> reactionIds = queryResult;
			// Frees up the queryResult list for next use
			queryResult = new ArrayList<String>();
			// Queries species metadata and all the properties of a group of
			// species
			queryAllProperties(reactionIds);
		}
	}	
	
	/**
	 * Queries a reaction using its phase instance id.
	 * 
	 * @param phaseOwlId the instance id of a phase in the OWL ontology 
	 * being processed
	 */
	private void queryOnlyReactions(String reactionMetadataId){
		String q = formSubjectRetrievalQuery(reactionMetadataId, appConfigOntokin.getReactionMetadataProperty());
		// This method call will store result in a global variable called
		// queryResult, therefore, the line following the call will extract
		// results from it. 
		performMultilineAnswerQuery(q, 2);
	}
	
	/**
	 * Queries all properties of reaction data and metadata.
	 * 
	 * @param reactionIds a list of OWL instance ids of all the reactions. 
	 */
	private void queryAllProperties(ArrayList<String> reactionIds){
		for(String reactionInstanceId: reactionIds){
			queryAllProperties(reactionInstanceId);
			String sourceComment = performQuery(formQueryWithBaseURL(reactionInstanceId, appConfigOntokin.getSourceComment()), 1);
			if (sourceComment != null) {
				objectVsSourceComment.put("reaction".concat(Integer.toString(++reactionSequence)), sourceComment);
			}
		}
	}
	
	/**
	 * Queries reaction metadata.
	 * 
	 * @param reactionData
	 * @param metadataInstanceId
	 */
	private void setReactionMetadata(ReactionData reactionData, String metadataInstanceId){
		String id = readReactionMetaDataId(metadataInstanceId);
		if(id!=null){
			reactionData.setId(id);
		}
		String caseSensitivity = readReactionDataCaseSensitivity(metadataInstanceId);
		if(caseSensitivity!=null){
			reactionData.setCaseSensitive(caseSensitivity);			
		}
		String sourceComment = performQuery(formQueryWithBaseURL(metadataInstanceId, appConfigOntokin.getSourceComment()), 1);
		if (sourceComment != null) {
			objectVsSourceComment.put("reactionData".concat(Integer.toString(++reactionDataSequence)), sourceComment);
		}
	}
	
	/**
	 * Reads  metadata id (e.g. GAS_reaction_data and MATERIAL1_reaction_data)
	 * 
	 * @param metaDataInstance
	 * @return
	 */
	private String readReactionMetaDataId(String metaDataInstance){
		String q = formQueryWithAStandardVocabulary(DUBLIN_CORE, DUBLIN_CORE_URL, metaDataInstance, DUBLIN_CORE_ID);
		return performQuery(q, 1);
	}
	
	/**
	 * Reads information about if reaction data is case sensitive or not
	 * 
	 * @param metaDataInstance
	 * @return
	 */
	private String readReactionDataCaseSensitivity(String metaDataInstance){
		String q = formQueryWithBaseURL(metaDataInstance, appConfigOntokin.getReactionDataCaseSensitivity());
		return performQuery(q, 1);
	}
	
	/**
	 * Queries all the properties of a reaction.
	 * 
	 * @param instance
	 */
	private void queryAllProperties(String instance){
		// Creates a reaction instance with all the properties found in OWL.
		Reaction reaction = new Reaction();
		reactionInOwlList.add(reaction);
		// Calls the method that queries data belonging to the attributes of 
		// the reaction element
		queryAttributes(instance, reaction);		
		// Calls the method that queries data belonging to the subelements of 
		// the reaction element
		querySimpleElements(instance, reaction);
		// Calls the method that queries data belonging to the subelements of 
		// the reaction element
		queryComplexElements(instance, reaction);
	}

	/**
	 * Calls the methods that deals with the queries related to the extraction
	 * of data belonging to the attributes which are inside the reaction element.
	 * 
	 * @param instance the OWL instance id of a reaction
	 * @param reaction the instance of a reaction
	 */
	private void queryAttributes(String instance, Reaction reaction) {
		queryIsDuplicate(instance, reaction);
		queryIsReversible(instance, reaction);
		queryType(instance, reaction);
		queryId(instance, reaction);
		queryNonCon(instance, reaction);
		queryPartialPressure(instance, reaction);
		querySiteFrac(instance, reaction);
		queryLandauTeller(instance, reaction);
	}
	
	/**
	 * Calls the methods that deals with the queries related to the extraction
	 * of data belonging to the elements which are under the reaction element.
	 * 
	 * @param instance the OWL instance id of a reaction
	 * @param reaction the instance of a reaction
	 */
	private void querySimpleElements(String instance, Reaction reaction) {
		// Passes the already created reaction instance to all the methods
		// in order to assign the values of the corresponding properties. 

		// The following method queries the comment about a reaction.
		queryComment(instance, reaction);
		// The following method queries the equation of a reaction.  
		queryEquation(instance, reaction);
		// The following method queries the reactants of a reaction.  
		queryReactants(instance, reaction);
		// The following method queries the products of a reaction.  
		queryProducts(instance, reaction);	
	}
	
	/**
	 * Calls the methods that deals with the queries related to the extraction
	 * of data and subelements belonging to the elements which are under the 
	 * reaction element.
	 * 
	 * @param instance the OWL instance id of a reaction
	 * @param reaction the instance of a reaction
	 */
	private void queryComplexElements(String instance, Reaction reaction) {
		// Passes the already created reaction instance to all the methods
		// in order to assign the values of the corresponding properties. 
		
		// Queries all possible rate coefficients.
		queryRateCoefficients(instance, reaction);
		// Quries the order of reaction for a species.
		queryReactionOrder(instance, reaction);
	}
	
	/**
	 * Queries following types of rate coefficients:</br>
	 * 1. Arrhenius Coefficients;</br>
	 * 2. Sticking Coefficients;</br>
	 *
	 * 
	 * @param instance the OWL instance id of a reaction
	 * @param reaction the instance of a reaction
	 */
	private void queryRateCoefficients(String instance, Reaction reaction){
		rateCoefficient = new RateCoefficient();
		reaction.setRateCoeff(rateCoefficient);
		// The following method queries the Arrhenius coefficients
		readArrhenius(instance, reaction);
		// The following method queries the Sticking coefficient parameters
		readSticking(instance, reaction);
		// The following method queries the Coverage parameters
		readCoverage(instance, reaction);
		// The following method queries the Landau-Teller coefficients
		readLandauTeller(instance, reaction);
		// The following method queries different types of fall-off models
		readFallOff(instance, reaction);
		// Queries the third body efficiencies
		queryEfficiencies(instance, reaction);
		// Queries Chebyshev parameters
		readChebyshev(instance, reaction);
	}
	
	/**
	 * Queries the efficiency of the third body species in the current reaction.
	 * 
	 * @param instance the OWL instance id of the current reaction
	 * @param reaction the instance created to codify the reaction in CTML
	 */
	private void queryEfficiencies(String instance, Reaction reaction){
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getObjectPropertyThirdBodyEfficiency());
		performMultilineAnswerQuery(q, 1);
		if(queryResult!=null && !queryResult.isEmpty()){
			processEfficiencies(instance, reaction);
		}
	}
	
	/**
	 * Queries the efficiency of the third body species in the current reaction.
	 * 
	 * @param reactionInstance the OWL instance id of the current reaction
	 * @param reaction the instance created to codify the reaction in CTML
	 */
	private void processEfficiencies(String reactionInstance, Reaction reaction){
		Collections.sort(queryResult);
		List<String> efficiencyInstances = queryResult;
		queryResult = new ArrayList<String>();
		Efficiencies efficiency = new Efficiencies();
		rateCoefficient.setEfficiencies(efficiency);
		readDefaultThirdBodyEfficiency(reactionInstance, efficiency);
		queryNamedThirdBodies(efficiencyInstances, efficiency);
	}
	
	/**
	 * Queries the efficiency of a third body for the current reaction.
	 * 
	 * @param efficiencyInstance the OWL instance id of the current reaction's 
	 * third body efficiency
	 * @param efficiency the instance of the 
	 */
	private void queryNamedThirdBodies(List<String> efficiencyInstances, Efficiencies efficiency){
		String thirdBodyVsEfficiencyPairs = "";
		for(String efficiencyInstance:efficiencyInstances){
			String thirdBodyVsEfficiencyPair = queryNamedThirdBodies(efficiencyInstance);
			if(thirdBodyVsEfficiencyPair!=null){
				thirdBodyVsEfficiencyPairs = thirdBodyVsEfficiencyPairs.concat(thirdBodyVsEfficiencyPair); 
			}
		}
		efficiency.setValue(thirdBodyVsEfficiencyPairs.trim());
	}
	
	/**
	 * Queries the order of reaction for a set of species.
	 * 
	 * @param reactionInstance the OWL instance id of the current reaction
	 * @param efficiency the instance created to codify the current 
	 * efficiencies in CTML
	 */
	private void readDefaultThirdBodyEfficiency(String reactionInstance, Efficiencies efficiency){
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getHasDefaultThirdBodyEfficiency());
		String defaultEfficiency = performQuery(q, 1);
		if(defaultEfficiency!=null){
			efficiency.setDefault(defaultEfficiency);
		}
	}
	
	/**
	 * Queries the efficiency of a third body for the current reaction.
	 * 
	 * @param efficiencyInstance the OWL instance id of the current reaction's 
	 * third body efficiency
	 */
	private String queryNamedThirdBodies(String efficiencyInstance){
		String thirdBody = readSpecies(efficiencyInstance);
		if(thirdBody!=null){
			thirdBody = thirdBody.concat(COLON);
		}
		String efficiencyValue = readEfficiencyValue(efficiencyInstance);
		if(efficiencyValue!=null){
			thirdBody = thirdBody.concat(efficiencyValue).concat(SPACE);
		}
		return thirdBody;
	}
	
	/**
	 * Queries the efficiency of a third body of the current reaction.
	 * 
	 * @param instance the OWL instance id of a reaction
	 * @return String the efficiency value
	 */
	private String readEfficiencyValue(String efficiencyInstance){
		String q = formQueryWithBaseURL(efficiencyInstance, appConfigOntokin.getDataPropertyHasEfficiencyValue());
		return performQuery(q, 1);
	}
	
	/**
	 * Queries the order of reaction for a set of species.
	 * 
	 * @param instance the OWL instance id of a reaction
	 * @param reaction the instance of a reaction
	 */
	private void queryReactionOrder(String instance, Reaction reaction){
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getObjectPropertyReactionOrder());
		performMultilineAnswerQuery(q, 1);
		if(queryResult!=null && !queryResult.isEmpty()){
			queryReactionOrder(reaction);
		}
	}
	
	/**
	 * Queries the order of reaction for a set of species.
	 * 
	 * @param reaction the instance of a reaction
	 */
	private void queryReactionOrder(Reaction reaction){
		Collections.sort(queryResult);
		List<String> reactionOrderInstances = queryResult;
		queryResult = new ArrayList<String>();
		ArrayList<ReactionOrder> reactionOrderList = new ArrayList<ReactionOrder>();
		reaction.setOrder(reactionOrderList);
		for(String reactionOrderInstance:reactionOrderInstances){
			readReactionOrder(reactionOrderInstance, reactionOrderList);
		}
	}
	
	/**
	 * Reads the order of reaction for a species.
	 * 
	 * @param reactionOrderInstance the OWL instance id of a reaction order
	 * @param reactionOrderlist the list of reaction orders attached to 
	 * a reaction   
	 */
	private void readReactionOrder(String reactionOrderInstance, List<ReactionOrder> reactionOrderlist){
		reactionOrder = new ReactionOrder();
		reactionOrderlist.add(reactionOrder);
		readReactionDirection(reactionOrderInstance);
		String orderSpecies = readOrderSpecies(reactionOrderInstance);
		if(orderSpecies!=null){
			reactionOrder.setSpecies(orderSpecies);
		}
		readReactionOrderValue(reactionOrderInstance);
	}
	
	/**
	 * Reads the value of the current reaction order.
	 * 
	 * @param reactionOrderInstance the OWL instance id of the current 
	 * reaction order 
	 */
	private void readReactionOrderValue(String reactionOrderInstance){
		String value = readName(reactionOrderInstance);
		if(value!=null){
			reactionOrder.setValue(value);
		}
	}
	
	/**
	 * Reads the reaction order species.
	 * 
	 * @param @param reactionOrderInstance the OWL instance id of a reaction order
	 */
	private String readOrderSpecies(String reactionOrderInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionOrderInstance, appConfigOntokin.getObjectPropertyOrderSpecies());
		// Performs the query q and returns the result
		String speciesInstance = performQuery(q, 1);
		if(speciesInstance!=null){
			return readName(speciesInstance);
		}
		return null;
	}

	
	/**
	 * Reads if the order applies to a species contributes to the forward 
	 * direction or the reverse direction.
	 * 
	 * @param reactionOrderInstance the OWL instance id of the current 
	 * reaction order 
	 */
	private void readReactionDirection(String reactionOrderInstance){
		String direction = readDirection(reactionOrderInstance);
		if(direction!=null){
			reactionOrder.setDirection(direction);
		}
	}
	
	/**
	 * Reads if the order applies to a species contributes to the forward 
	 * direction or the reverse direction.
	 * 
	 * @param reactionOrderInstance the OWL instance id of the current 
	 * reaction order 
	 */
	private String readDirection(String reactionOrderInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionOrderInstance, appConfigOntokin.getDataPropertyOrderDirection());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Queries if the reaction is duplicate or not in the mechanism OWL 
	 * file being processed.
	 */
	private void queryIsDuplicate(String instance, Reaction reaction){
		String isDuplicate = readIsDuplicate(instance);
		if(isDuplicate==null){
		} else{
			reaction.setDuplicate(isDuplicate);
		}
	}

	/**
	 * Queries if the reaction is reversible or not in the mechanism OWL 
	 * file being processed.
	 */
	private void queryIsReversible(String instance, Reaction reaction){
		String isReversible = readIsReversible(instance);
		if(isReversible==null){
		} else{
			reaction.setReversible(isReversible);
		}
	}
	
	/**
	 * Queries the type of a reaction in the mechanism OWL file 
	 * being processed.
	 */
	private void queryType(String instance, Reaction reaction){
		// Calling the following method will fill the queryResult global 
		// variable with classes which are the types of the instance provided
		// as the input parameter. 
		readType(instance);
		if(queryResult!=null && !queryResult.isEmpty()){
			ArrayList<String> types = queryResult;
			queryResult = new ArrayList<String>();
			queryType(types, reaction);
		}
	}
	
	private void queryType(ArrayList<String> types, Reaction reaction) {
		for (String type : types) {
			if (type != null) {
				if (appConfigOntokin.getClassReaction().equalsIgnoreCase(type)) {
					continue;
				}
				String claz=retrieveType(type);
				if (claz != null) {
					reaction.setType(claz);
				}

			}
		}
	}
	
	private String retrieveType(String type){
		String retreivedType = null;
		try {
			retreivedType = OwlConverterUtils.getReactionClass(type);
		} catch (OntoException e) {
			logger.error("The type attribute of a reaction does "
					+ "not match with any of the reaction class name.");
		}
		return retreivedType;
	}
	
	/**
	 * Queries the noncon (may not conserve site) info attached to the instance
	 * of a reaction in the mechanism OWL file being processed.
	 */
	private void queryNonCon(String instance, Reaction reaction){
		String noncon = readNoncon(instance);
		if(noncon==null){
		} else{
			reaction.setNoncon(noncon);
		}
	}
	
	/**
	 * Queries the partialPressure info attached to the instance 
	 * of a reaction in the mechanism OWL file being processed.
	 */
	private void queryPartialPressure(String instance, Reaction reaction){
		String partialpressure = readPartialpressure(instance);
		if(partialpressure==null){
		} else{
			reaction.setPartialpressure(partialpressure);
		}
	}
	
	/**
	 * Queries the sitefrac info attached to the instance 
	 * of a reaction in the mechanism OWL file being processed.
	 */
	private void querySiteFrac(String instance, Reaction reaction){
		String sitefrac = readSiteFrac(instance);
		if(sitefrac==null){
		} else{
			reaction.setSitefrac(sitefrac);
		}
	}
	
	/**
	 * Queries if the reaction is of type LandauTeller.
	 */
	private void queryLandauTeller(String instance, Reaction reaction){
		String landauTeller = readLandauTeller(instance);
		if(landauTeller==null){
		} else{
			reaction.setLandauTeller(LANDAUTELLER_YES);
		}
	}
	
	/**
	 * Queries the id attached to the instance of a reaction in the mechanism 
	 * OWL file being processed.
	 */
	private void queryId(String instance, Reaction reaction){
		String id = readId(instance);
		if(id==null){
		} else{
			reaction.setId(id);
		}
	}
	
	/**
	 * Queries the comment attached to the instance of a reaction in the
	 * mechanism OWL file being processed.
	 */
	private void queryComment(String instance, Reaction reaction){
		String comment = readComment(instance);
		if(comment==null){
		} else{
			if(comment.contains("\\\\")){
				reaction.setComment(comment.replace("\\\\", "\\"));
			} else if (comment.contains("\\\"")){
				reaction.setComment(comment.replace("\\\"", "\""));
			} else{
				reaction.setComment(comment);
			}
		}
	}
	
	/**
	 * Queries the comment attached to the instance of a reaction in the
	 * mechanism OWL file being processed.
	 */
	private void queryEquation(String instance, Reaction reaction){
		String equation = readEquation(instance);
		if(equation==null){
		} else{
			reaction.setEquation(equation);
		}
	}
	
	/**
	 * Queries the reactants attached to the instance of a reaction in the
	 * mechanism OWL file being processed.
	 */
	private void queryReactants(String instance, Reaction reaction){
		String reactantVsStoichioCoeffsPairs = readReactants(instance);
		if(reactantVsStoichioCoeffsPairs==null){
		} else{
			reaction.setReactants(reactantVsStoichioCoeffsPairs);
		}
	}
	
	/**
	 * Queries the products attached to the instance of a reaction in the
	 * mechanism OWL file being processed.
	 */
	private void queryProducts(String instance, Reaction reaction){
		String productVsStoichioCoeffsPairs = readProducts(instance);
		if(productVsStoichioCoeffsPairs==null){
		} else{
			reaction.setProducts(productVsStoichioCoeffsPairs);
		}
	}
	
	/**
	 * Queries the objects which are connected to the current reaction using
	 * the hasArrheniusRateCoefficient object property.
	 * 
	 * @param instance the OWL instance id of the current reaction being 
	 * retrieved from the mechanism ontology 
	 * @param reaction an instance of Reaction is being filled to codify 
	 * in CTML.  
	 */
//	private void queryArrhenius(String instance, Reaction reaction){
//		readArrhenius(instance, reaction);
//	}
	
	/**
	 * Forms and then performs a SPARQL query to read the information if a 
	 * reaction in the current mechanism is duplicate or not.
	 * 
	 * @return whether or not a reaction is reversible
	 */
	private String readIsDuplicate(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getReactionDuplicate());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the information if a 
	 * reaction is reversible or not.
	 * 
	 * @return whether or not a reaction is reversible
	 */
	private String readIsReversible(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getReactionReverisble());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the information if a 
	 * reaction is reversible or not.
	 *
	 * @param reactionInstance
	 */
	private void readType(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formTypeQueryWithAnInstance(reactionInstance);
		// Performs the query q and returns the result
		performMultilineAnswerQuery(q, 2);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the noncon (may not 
	 * conserve site) info that expresses whether or not a surface reaction 
	 * is required to conserve the number of surface sites.
	 * 
	 * @param reactionInstance the reaction instance
	 * @return the noncon info
	 */
	private String readNoncon(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getReactionMayNotConserveSite());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the partial pressure 
	 * info that expresses whether or not a surface reaction 
	 * is required to conserve the number of surface sites.
	 * 
	 * @param reactionInstance the reaction instance
	 * @return the necessity to convert into partial pressure
	 */
	private String readPartialpressure(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getConvertToPartialPressure());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the sitefrac 
	 * info that expresses whether the surface species concentrations should 
	 * be converted to site fractions when calculating the reaction rate.
	 * 
	 * @param reactionInstance the reaction instance
	 * @return the necessity to convert into site fraction
	 */
	private String readSiteFrac(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getConvertToSiteFraction());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	/**
	 * Forms and then performs a SPARQL query to read if the reaction
	 * represented in OWL is a LandauTeller reaction.
	 * 
	 * @param reactionInstance the reaction instance
	 * @return either LandauTeller or null
	 */
	private String readLandauTeller(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getObjectPropertyLandauTellerRateCoeff());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the id of a reaction.
	 * 
	 * @param instance the reaction instance
	 * @return the id of the reaction
	 */
	private String readId(String instance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(DUBLIN_CORE, DUBLIN_CORE_URL, instance, DUBLIN_CORE_ID);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the equation of a 
	 * reaction.
	 * 
	 * @return the equation attached to a reaction
	 */
	private String readEquation(String reactionInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getOntoKinEquation());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the reactants of a 
	 * reaction.
	 * 
	 * @return the reactants attached to a reaction
	 */
	private String readReactants(String reactionInstance) {
		String reactantVsStoichiometricCoeff = "";
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getObjectPropertyReactantSpecification());
		performMultilineAnswerQuery(q, 1);
		if(queryResult!=null && !queryResult.isEmpty()){
			Collections.sort(queryResult);
			List<String> reactantSpecInstances = queryResult;
			queryResult = new ArrayList<String>();
			for(String reactantSpecInstance:reactantSpecInstances){
				String result = readReactantSpecification(reactantSpecInstance);
				if (result!=null && !result.isEmpty()){
					reactantVsStoichiometricCoeff = reactantVsStoichiometricCoeff.concat(result).concat(SPACE);
				}
			}
		}
		return reactantVsStoichiometricCoeff.trim();
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the products of a 
	 * reaction.
	 * 
	 * @return the products attached to a reaction
	 */
	private String readProducts(String reactionInstance) {
		String productVsStoichiometricCoeff = "";
		String q = formQueryWithBaseURL(reactionInstance, appConfigOntokin.getObjectPropertyProductSpecification());
		performMultilineAnswerQuery(q, 1);
		if(queryResult!=null && !queryResult.isEmpty()){
			Collections.sort(queryResult);
			List<String> productSpecInstances = queryResult;
			queryResult = new ArrayList<String>();
			for(String productSpecInstance:productSpecInstances){
				String result = readProductSpecification(productSpecInstance);
				if (result!=null && !result.isEmpty()){
					productVsStoichiometricCoeff = productVsStoichiometricCoeff.concat(result).concat(SPACE);
				}
			}
		}
		return productVsStoichiometricCoeff.trim();
	}
	
	/**
	 * Uses the reactant specification instance of a reactant to retrieve the
	 * </br>following two values:</br>
	 * 1. Species</br>
	 * 2. Stoichiometric Coefficient of the species 
	 * 
	 * @param reactantSpecInstance
	 * @return
	 */
	private String readReactantSpecification(String reactantSpecInstance){
		String speciesVsStCoeff = "";
		String speciesInstance = readReactantSpecificationSpecies(reactantSpecInstance);
		if(speciesInstance!=null && !speciesInstance.isEmpty()){
			speciesVsStCoeff = readLabel(speciesInstance);
		}
		String stoichioMetricCoeff = readStoichiometricCoeff(reactantSpecInstance);
		if(speciesVsStCoeff!=null && stoichioMetricCoeff!=null && !speciesVsStCoeff.isEmpty() && stoichioMetricCoeff!=null){
			speciesVsStCoeff = speciesVsStCoeff.concat(COLON).concat(stoichioMetricCoeff);
		}
		return speciesVsStCoeff;
	}
	
	/**
	 * Uses the product specification instance of a product to retrieve the
	 * </br>following two values:</br>
	 * 1. Species</br>
	 * 2. Stoichiometric Coefficient of the species 
	 * 
	 * @param prodcutSpecInstance
	 * @return
	 */
	private String readProductSpecification(String prodcutSpecInstance){
		String speciesVsStCoeff = "";
		String speciesInstance = readProductSpecificationSpecies(prodcutSpecInstance);
		if(speciesInstance!=null && !speciesInstance.isEmpty()){
			speciesVsStCoeff = readLabel(speciesInstance);
		}
		String stoichioMetricCoeff = readStoichiometricCoeff(prodcutSpecInstance);
		if(speciesVsStCoeff!=null && stoichioMetricCoeff!=null && !speciesVsStCoeff.isEmpty() && stoichioMetricCoeff!=null){
			speciesVsStCoeff = speciesVsStCoeff.concat(COLON).concat(stoichioMetricCoeff);
		}
		return speciesVsStCoeff;
	}
	
	/**
	 * Reads the species of a reactant specification 
	 * 
	 * @param reactantSpecInstance
	 * @return
	 */
	private String readReactantSpecificationSpecies(String reactantSpecInstance){
		String q = formQueryWithBaseURL(reactantSpecInstance, appConfigOntokin.getObjectPropertyHasReactant());
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the species of a product specification 
	 * 
	 * @param prodcutSpecInstance
	 * @return
	 */
	private String readProductSpecificationSpecies(String prodcutSpecInstance){
		String q = formQueryWithBaseURL(prodcutSpecInstance, appConfigOntokin.getObjectPropertyHasProduct());
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the product Stoichiometric coefficient of a reaction. 
	 * 
	 * @param productSpecInstance
	 * @return
	 */
	private String readStoichiometricCoeff(String productSpecInstance){
		String q = formQueryWithBaseURL(productSpecInstance, appConfigOntokin.getDataPropertyStoichiometricCoefficient());
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the OWL instance id  
	 * of the Chebyshev model applicable to the current reaction.
	 * 
	 * @param reactionInstance the OWL instance id of the current reaction 
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readChebyshev(String reactionInstance, Reaction reaction) {
		String q = formQueryWithBaseURL(reactionInstance, 
				appConfigOntokin.getObjectPropertyCHEBRateCoeff());
		String chebyshevInstance = performQuery(q, 1);
		if(chebyshevInstance!=null && !chebyshevInstance.isEmpty()){
			readAllCHEBParameters(chebyshevInstance);
		}
	}
	
	/**
	 * Reads all the parameters of the CHEB model from the current 
	 * reaction's Chebyshev parameters. 
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readAllCHEBParameters(String chebyshevInstance){
		readTemperatures(chebyshevInstance);
		readPressures(chebyshevInstance);
		readCHEBMetadata(chebyshevInstance);
	}
	
	/**
	 * Reads the CHEB model metadata from the current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readCHEBMetadata(String chebyshevInstance){
		String chebCoeffName = readName(chebyshevInstance);
		String chebCoeffUnits = readCoeffUnits(chebyshevInstance);
		String degreeT = readDegreeT(chebyshevInstance);
		String degreeP = readDegreeP(chebyshevInstance);
		String coeffValues = readCHEBCoeffValues(chebyshevInstance);
		if(chebCoeffName!=null || chebCoeffUnits!=null || degreeT!=null || degreeP!=null || coeffValues!=null){
			RateCoeffFloatArray floatArray = new RateCoeffFloatArray();
			rateCoefficient.setFloatArray(floatArray);
			readCHEBMetadata(floatArray, chebCoeffName, chebCoeffUnits, degreeT, degreeP, coeffValues);
		}
	}

	/**
	 * Reads the CHEB model metadata and coefficient values from the current 
	 * Chebyshev reaction.
	 * 
	 * @param floatArray an instance of the RateCoeffFloatArray created to 
	 * fill in CHEB coefficient values and related metadata  
	 * @param strings an array of strings containing name, CHEB coeff units,
	 * temperature points, pressure points and values.
	 */
	private void readCHEBMetadata(RateCoeffFloatArray floatArray, String ...strings){
		int counter = 0;
		for(String string:strings){
			counter++;
			switch(counter){
			case 1: if(string!=null){floatArray.setName(string); break;}
			case 2: if(string!=null){floatArray.setUnits(string); break;}
			case 3: if(string!=null){floatArray.setDegreeT(string); break;}
			case 4: if(string!=null){floatArray.setDegreeP(string); break;}
			case 5: if(string!=null){floatArray.setValue(string); break;}
			}
		}
	}
	
	/**
	 * Reads the minimum and maximum temperatures and their units from the
	 * current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readTemperatures(String chebyshevInstance){
		readMinTemperature(chebyshevInstance);
		readMaxTemperature(chebyshevInstance);
	}

	/**
	 * Reads the maximum temperature and its units from the
	 * current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readMaxTemperature(String chebyshevInstance){
		String tMax = readTmax(chebyshevInstance);
		String tMaxUnits = readTmaxUnits(chebyshevInstance);
		if(tMax!=null || tMaxUnits!=null){
			TMax tmax = new TMax();
			rateCoefficient.setTmax(tmax);
			if(tMax!=null){
				tmax.setValue(tMax);
			}
			if(tMaxUnits!=null){
				tmax.setUnits(tMaxUnits);
			}
		}
	}

	/**
	 * Reads the minimum temperature and its units from the
	 * current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readMinTemperature(String chebyshevInstance){
		String tMin = readTmin(chebyshevInstance);
		String tMinUnits = readTminUnits(chebyshevInstance);
		if(tMin!=null || tMinUnits!=null){
			TMin tmin = new TMin();
			rateCoefficient.setTmin(tmin);
			if(tMin!=null){
				tmin.setValue(tMin);
			}
			if(tMinUnits!=null){
				tmin.setUnits(tMinUnits);
			}
		}
	}

	
	/**
	 * Reads the minimum and maximum pressures and their units from the
	 * current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readPressures(String chebyshevInstance){
		readMinPressure(chebyshevInstance);
		readMaxPressure(chebyshevInstance);
	}

	/**
	 * Reads the maximum pressure and its units from the
	 * current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readMaxPressure(String chebyshevInstance){
		String pMax = readPmax(chebyshevInstance);
		String pMaxUnits = readPmaxUnits(chebyshevInstance);
		if(pMax!=null || pMaxUnits!=null){
			PMax pmax = new PMax();
			rateCoefficient.setPmax(pmax);
			if(pMax!=null){
				pmax.setValue(pMax);
			}
			if(pMaxUnits!=null){
				pmax.setUnits(pMaxUnits);
			}
		}
	}

	/**
	 * Reads the minimum pressure and its units from the
	 * current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private void readMinPressure(String chebyshevInstance){
		String pMin = readPmin(chebyshevInstance);
		String pMinUnits = readPminUnits(chebyshevInstance);
		if(pMin!=null || pMinUnits!=null){
			PMin pmin = new PMin();
			rateCoefficient.setPmin(pmin);
			if(pMin!=null){
				pmin.setValue(pMin);
			}
			if(pMinUnits!=null){
				pmin.setUnits(pMinUnits);
			}
		}
	}
	
	/**
	 * Forms and then performs a query to read the CHEB coefficient values 
	 * from the the current Chebyshev reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readCHEBCoeffValues(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyChebyshebRateCoeffsValue());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the pressure points used to 
	 * parameterise the pressure dependency in the current Chebyshev 
	 * reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readDegreeP(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyChebyshebRateCoeffsPressurePoints());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	
	/**
	 * Forms and then performs a query to read the temperature points used to 
	 * parameterise the temperature dependency in the current Chebyshev 
	 * reaction.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readDegreeT(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyChebyshebRateCoeffsTempPoints());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the units of the CHEB  
	 * coefficients from the current reaction's Chebyshev parameters.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readCoeffUnits(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyChebyshebRateCoeffsUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the units of the maximum  
	 * pressure from the current reaction's Chebyshev parameters.
	 * 
	 * @param chebInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readPmaxUnits(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyPressureMaxUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the maxmum pressure  
	 * above which the Chebyshev parameterisation is invalid from the
	 * current reaction's Chebyshev parameters.
	 * 
	 * @param reactionInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readPmax(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyPressureMax());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	
	/**
	 * Forms and then performs a query to read the units of the minimum  
	 * pressure from the current reaction's Chebyshev parameters.
	 * 
	 * @param reactionInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readPminUnits(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyPressureMinUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the minimum pressure  
	 * above which the Chebyshev parameterisation is invalid from the
	 * current reaction's Chebyshev parameters.
	 * 
	 * @param reactionInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readPmin(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyPressureMin());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the units of the minimum  
	 * temperature from the current reaction's Chebyshev parameters.
	 * 
	 * @param reactionInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readTminUnits(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyTempMinUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the minimum temperature  
	 * above which the Chebyshev parameterisation is invalid from the
	 * current reaction's Chebyshev parameters.
	 * 
	 * @param reactionInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readTmin(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyTempMin());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the units of the maximum  
	 * temperature from the current reaction's Chebyshev parameters.
	 * 
	 * @param reactionInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readTmaxUnits(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyTempMaxUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the maximum temperature  
	 * above which the Chebyshev parameterisation is invalid from the
	 * current reaction's Chebyshev parameters.
	 * 
	 * @param reactionInstance the OWL instance id of the CHEB model 
	 * of the current reaction
	 */
	private String readTmax(String chebInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(chebInstance, appConfigOntokin.getDataPropertyTempMax());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Arrhenius Rate 
	 * Coefficient instances of a reaction.
	 * 
	 * @param reactionInstance the OWL instance id of the current reaction 
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readArrhenius(String reactionInstance, Reaction reaction) {
		String q = formQueryWithBaseURL(reactionInstance, 
				appConfigOntokin.getObjectPropertyArrheniusRateCoeff());
		performMultilineAnswerQuery(q, 1);
		readArrhenius(reaction);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the LandauTeller Rate 
	 * Coefficient instances of a reaction.
	 * 
	 * @param reactionInstance the OWL instance id of the current reaction 
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readLandauTeller(String reactionInstance, Reaction reaction) {
		String q = formQueryWithBaseURL(reactionInstance, 
				appConfigOntokin.getObjectPropertyLandauTellerRateCoeff());
		performMultilineAnswerQuery(q, 1);
		readLandauTeller(reaction);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Fall-off model 
	 * parameters of a reaction.
	 * 
	 * @param reactionInstance the OWL instance id of the current reaction 
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readFallOff(String reactionInstance, Reaction reaction) {
		String q = formQueryWithBaseURL(reactionInstance, 
				appConfigOntokin.getObjectPropertyFallOffModelCoeff());
		String fallOffInstance = performQuery(q, 1);
		if(fallOffInstance!=null && !fallOffInstance.isEmpty()){
			fallOff = new FallOff();
			rateCoefficient.setFalloff(fallOff);
			readFallOffParameters(fallOffInstance);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Arrhenius Rate 
	 * Coefficient instances of a reaction.
	 * 
	 * @param the OWL instance id of the Modified Arrhenius Coefficient of 
	 * the current reaction 
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readSticking(String reactionInstance, Reaction reaction) {
		String q = formQueryWithBaseURL(reactionInstance, 
				appConfigOntokin.getObjectPropertyStickingCoeff());
		performMultilineAnswerQuery(q, 1);
		readSticking(reaction);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Coverage Dependency 
	 * instance of a reaction.
	 * 
	 * @param the OWL instance id of the Modified Arrhenius Coefficient of 
	 * the current reaction 
	 * @param arrhenius the instance of the Arrhenius Coefficient which is 
	 * currently being filled in to produce CTML.
	 */
	private void readCoverage(String reactionInstance, Reaction reaction) {
		String q = formQueryWithBaseURL(reactionInstance, 
				appConfigOntokin.getObjectPropertyCoverageCoefficient());
		performMultilineAnswerQuery(q, 1);
		readCoverage(reaction);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Arrhenius Rate 
	 * Coefficient instances of a reaction.
	 *  
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readArrhenius(Reaction reaction){
		if(queryResult!=null && !queryResult.isEmpty()){
			Collections.sort(queryResult);
			List<String> arrheniusInstances = queryResult;
			queryResult = new ArrayList<String>();
			List<Arrhenius> arrheniusList = new ArrayList<Arrhenius>();
			rateCoefficient.setArrhenius(arrheniusList);
			// 1 represents that it is an Arrhenius reaction
			readArrhenius(arrheniusInstances, arrheniusList, 1);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the LandauTeller Rate 
	 * Coefficient instances of a reaction.
	 *  
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readLandauTeller(Reaction reaction){
		if(queryResult!=null && !queryResult.isEmpty()){
			Collections.sort(queryResult);
			List<String> landauTellerInstances = queryResult;
			queryResult = new ArrayList<String>();
			LandauTeller landauTeller = new LandauTeller();
			rateCoefficient.setLandauTeller(landauTeller);
			readLandauTeller(landauTellerInstances, landauTeller);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Fall-off model 
	 * parameters.
	 *  
	 * @param fallOffInstance the OWL instance id of the Fall-off model of 
	 * the current reaction.
	 */
	private void readFallOffParameters(String fallOffInstance){
		processFallOffType(fallOffInstance);
		processnamedThirdBody(fallOffInstance);
		processFallOffParameterValues(fallOffInstance);
	}

	/**
	 * Following the forming and then performing a query to read the named 
	 * third body of the current Fall-off reaction, it assigns the value
	 * to the current instance of the Fall-off reaction to generate CTML.  
	 * 
	 * @param fallOffInstance the OWL instance id of the Fall-off model 
	 * of the current reaction
	 */
	private void processnamedThirdBody(String fallOffInstance){
		String namedThirdBody = readNamedThirdBody(fallOffInstance);
		if(namedThirdBody!=null){
			fallOff.setNamedThirdBody(namedThirdBody);
		}
	}
	
	/**
	 * Reads the OWL instance id of the named third body of the current 
	 * Fall-off reaction.
	 * 
	 * @param instance the OWL instance id of the current Fall-off reaction
	 */
	private String readNamedThirdBody(String fallOffInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(fallOffInstance, appConfigOntokin.getHasNamedThirdBody());
		// Performs the query q and returns the result
		String speciesInstance = performQuery(q, 1);
		if(speciesInstance!=null){
			return readName(speciesInstance);
		}
		return null;
	}
	
	/**
	 * Following the forming and then performing a query to read the parameter 
	 * values of the current Fall-off model, it assigns them to the current
	 * instance of the Fall-off model to generate CTML.  
	 * 
	 * @param fallOffInstance the OWL instance id of the Fall-off model 
	 * of the current reaction
	 */
	private void processFallOffParameterValues(String fallOffInstance){
		String value = readFallOffCoeffValues(fallOffInstance);
		if(value!=null){
			fallOff.setValue(value);
		}
	}
	
	/**
	 * Forms and then performs a query to read the type of the current 
	 * Fall-off model.
	 * 
	 * @param fallOffInstance the OWL instance id of the Fall-off model 
	 * of the current reaction
	 */
	private String readFallOffCoeffValues(String fallOffInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(fallOffInstance, appConfigOntokin.getDataPropertyHasCoeffValues());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a query to read the type of the current 
	 * Fall-off model.
	 * 
	 * @param fallOffInstance the OWL instance id of the Fall-off model 
	 * of the current reaction
	 */
	private void processFallOffType(String fallOffInstance){
		String type = readFallOffType(fallOffInstance);
		queryResult = new ArrayList<String>();
		if(type!=null){
			fallOff.setType(type);
		}
	}
	
	
	/**
	 * Forms and then performs a query to read the type of the current 
	 * Fall-off model.
	 * 
	 * @param fallOffInstance the OWL instance id of the Fall-off model 
	 * of the current reaction
	 */
	private String readFallOffType(String fallOffInstance){
		// Calls the method that forms a SPARQL query
		String q = formTypeQueryWithAnInstance(fallOffInstance);
		// Performs the query q and returns the result
		performMultilineAnswerQuery(q, 2);
		return processFallOffTypeResult();
	}
	
	/**
	 * Retrieves the type of the current Fall-off model.
	 * 
	 * @return the type of the current Fall-off model
	 */
	private String processFallOffTypeResult() {
		if (queryResult != null && !queryResult.isEmpty()) {
			for (String type: queryResult) {
				if (!type.equalsIgnoreCase(appConfigOntokin.getClassFallOffModelCoefficient())) {
					try {
						if (OwlConverterUtils.getReactionClass(type) != null) {
							return OwlConverterUtils.getReactionClass(type);
						}
					} catch (OntoException e) {
						logger.error("More specific Fall-off model was not found.");
						e.printStackTrace();
					}
				}
			}
		}
		return null;
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Arrhenius Rate 
	 * Coefficient instances of a reaction.
	 *  
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readSticking(Reaction reaction){
		if(queryResult!=null && !queryResult.isEmpty()){
			Collections.sort(queryResult);
			// Initialises the sitcking coefficient instance
			arrhenius = new Arrhenius();
			// The following method determines if the coefficients are of type 
			// sticking and then assigns type="stick".
			determineType();
			List<String> arrheniusInstances = queryResult;
			queryResult = new ArrayList<String>();
			List<Arrhenius> arrheniusList = new ArrayList<Arrhenius>();
			rateCoefficient.setArrhenius(arrheniusList);
			// Setting any number except 1 in the last parameter of the
			// readArrhenius method expresses the fact that the method is
			// not dealing with Arrhenius coefficients. 
			readArrhenius(arrheniusInstances, arrheniusList, 2);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Arrhenius Rate 
	 * Coefficient instances of a reaction.
	 *  
	 * @param reaction the instance of the reaction which is currently being 
	 * filled in to produce CTML.
	 */
	private void readCoverage(Reaction reaction){
		if(queryResult!=null && !queryResult.isEmpty()){
			Collections.sort(queryResult);
			// The following method determines if the coefficients are of type 
			// sticking and then assigns type="stick".
			List<String> coverageInstances = queryResult;
			queryResult = new ArrayList<String>();
			List<Coverage> coverageList = new ArrayList<Coverage>();
			arrhenius.setCoverage(coverageList);
			readCoverage(coverageInstances, coverageList);
		}
	}
	
	/**
	 * Determines if the type of rate coefficients is sticking.
	 */
	private void determineType(){
		// If it passes the following check, a sticking coefficent is found.   
		if(queryResult!=null && !queryResult.isEmpty()){
			arrhenius.setType(appConfigCtml.getReactionRateCoeffArrheniusStick());
			// The following method determines if the Motz-Wise correction
			// factor needs to be applied.
			isMotzWiseRequired();
			// The following method extracts the name of the single gas phase
			// species that reacts with a surface.
			readSingleGasPhaseSpecies();
		}
	}
	
	/**
	 * Determines if the Motz-Wise correction factor needs to be applied.
	 */
	private void isMotzWiseRequired(){
		for(String stickingInstance:queryResult){
			String motzWise = readIsRequiredMotzWise(stickingInstance);
			if(motzWise!=null){
				arrhenius.setMotzWise(motzWise);
			}
		}
	}
	
	/**
	 * Reads the single gas phase species of a sticking coefficient reaction.
	 */
	private void readSingleGasPhaseSpecies(){
		for(String stickingInstance:queryResult){
			String species = readSpecies(stickingInstance);
			if(species!=null){
				arrhenius.setSpecies(species);
			}
		}
	}
	
	/**
	 * Reads the single gas phase species of a sticking coefficient reaction.
	 */
	private void readCoverageDependencySpecies(String coverageInstance, Coverage coverage){
		String species = readSpecies(coverageInstance);
		if (species != null) {
			coverage.setSpecies(species);
		}
	}
	
	/**
	 * Reads if the Motz-Wise correction factor is needed for the current 
	 * sticking coefficient model.  
	 * 
	 * @param stickingInstance the OWL instance id of the Sticking coefficient
	 * of the current reaction
	 */
	private String readIsRequiredMotzWise(String stickingInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(stickingInstance, appConfigOntokin.getDataPropertyHasMotzWiseCorrection());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the OWL instance id of one of the following instances:</br>
	 * 1. Sticking instance;</br>
	 * 2. Coverage-dependency instance.
	 * 
	 * @param instance the OWL instance id of the Sticking coefficient or 
	 * coverage-dependency of the current reaction
	 */
	private String readSpecies(String instance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getObjectPropertyHasSpecies());
		// Performs the query q and returns the result
		String speciesInstance = performQuery(q, 1);
		if(speciesInstance!=null){
			return readName(speciesInstance);
		}
		return null;
	}
	
	/**
	 * Reads the name (i.e. rdfs:label) of any instance.
	 * 
	 * @param instance the OWL instance id of an instance.
	 * @return String the name of the species
	 */
	private String readName(String instance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, instance, RDFS_LABEL);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	/**
	 * Forms and then performs a SPARQL query to read the Arrhenius rate 
	 * coefficient instances of a reaction.
	 * 
	 * Reads the following Arrhenius Coefficients:</br>
	 * 1. the pre-exponential factor;</br>
	 * 2. the temperature exponent;</br> 
	 * 3. the activation energy; and </br>
	 * 4. the reference pressure.
	 *
	 * @param arrheniusInstances the instances of Arrhenius coefficient found 
	 * in the current OWL ontology
	 * @param arrheniusList 
	 */
	private void readCoverage(List<String> coverageInstances, List<Coverage> coverageList){
		for(String coverageInstance:coverageInstances){
			coverage = new Coverage();
			coverageList.add(coverage);
			// The following method extracts the name of the species that 
			// causes a coverage-dependency in the observed rate constant.
			readCoverageDependencySpecies(coverageInstance, coverage);
			readPreExponentialModifier(coverageInstance, coverage);
			readReactionOrderModifier(coverageInstance, coverage);
			readExponentialModifier(coverageInstance, coverage);
		}
		if(coverageList!=null && coverageList.size()>=1){
			arrhenius.setCoverage(coverageList);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Arrhenius Rate 
	 * Coefficient instances of a reaction.
	 * 
	 * Reads the following Arrhenius Coefficients:</br>
	 * 1. the pre-exponential factor;</br>
	 * 2. the temperature exponent;</br> 
	 * 3. the activation energy; and </br>
	 * 4. the reference pressure.
	 *   
	 * @param arrheniusInstances the instances of Arrhenius coefficient found 
	 * in the current OWL ontology
	 * @param arrheniusList
	 * @param coeffType the type of a coefficient. 1 means Arrhenius and to 
	 * express any coefficient except Arrhenius we use coeffType>1 or 
	 * coeffType<1.
	 */
	private void readArrhenius(List<String> arrheniusInstances, List<Arrhenius> arrheniusList, int coeffType){
		for(String arrheniusInstance:arrheniusInstances){
			if(coeffType==1){
				arrhenius = new Arrhenius();
			}
			arrheniusList.add(arrhenius);
			readArrheniusName(arrheniusInstance, arrhenius);
			readPreExponentialFactor(arrheniusInstance, arrhenius);
			readTemperatureExponent(arrheniusInstance, arrhenius);
			readActivationEnergy(arrheniusInstance, arrhenius);
			readReferencePressure(arrheniusInstance, arrhenius);
		}
	}
	
	/**
	 * Reads the name of the Arrhenius coefficient being processed.
	 * 
	 * @param arrheniusInstance the instance of the Arrhenius coefficient 
	 * currently being processed 
	 * @param arrhenius an instance of the Arrhenius class
	 */
	private void readArrheniusName(String arrheniusInstance, Arrhenius arrhenius){
		String name = readName(arrheniusInstance);
		if(name!=null){
			arrhenius.setName(name);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Landau-Teller Rate 
	 * Coefficient instances of a reaction.
	 * 
	 * Reads the following Arrhenius Coefficients:</br>
	 * 1. the Landau-Teller Coefficient B;</br>
	 * 2. the Landau-Teller Coefficient C. 
	 *   
	 * @param landauTellerInstances the instances of Landau-Teller coefficient found 
	 * in the current OWL ontology
	 * @param landauTeller an instance of the Landau-Teller class. 
	 */
	private void readLandauTeller(List<String> landauTellerInstances, LandauTeller landauTeller){
		for(String landauTellerInstance:landauTellerInstances){
			readLandauTellerCoefficientB(landauTellerInstance, landauTeller);
			readLandauTellerCoefficientC(landauTellerInstance, landauTeller);
		}
	}
	
	/**
	 * Reads the following Landau-Teller Coefficient:</br>
	 * 1. the rate coefficient C; and</br>
	 * 2. the units of the rate coefficient C. 
	 * 
	 * @param the OWL instance id of the Landau-Teller Coefficient of 
	 * the current reaction 
	 * @param landauTeller the instance of the Landau-Teller coefficient which is 
	 * currently being filled in to produce CTML.
	 */
	private void readLandauTellerCoefficientC(String landauTellerInstance, LandauTeller landauTeller){
		String value = readLandauTellerCoefficientC(landauTellerInstance);
		if(value!=null){
			LandauTellerCoefficientC C = new LandauTellerCoefficientC();
			landauTeller.setC(C);
			C.setValue(value);
			String units = readLandauTellerCoefficientCUnits(landauTellerInstance);
			if(units!=null){
				C.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the Landau-Teller Coefficient C.
	 * 
	 * @param landauTellerInstance the OWL instance id of the Landau-Teller 
	 * Coefficient of the current reaction
	 */
	private String readLandauTellerCoefficientC(String landauTellerInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(landauTellerInstance, appConfigOntokin.getDataPropertyHasLanTellerCoeffC());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the units of the Landau-Teller Coefficient C.
	 * 
	 * @param landauTellerInstance the OWL instance id of the Landau-Teller 
	 * Coefficient of the current reaction
	 */
	private String readLandauTellerCoefficientCUnits(String landauTellerInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(landauTellerInstance, appConfigOntokin.getDataPropertyHasLanTellerCoeffCUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the following Landau-Teller Coefficient:</br>
	 * 1. the rate coefficient B; and</br>
	 * 2. the units of the rate coefficient B. 
	 * 
	 * @param the OWL instance id of the Landau-Teller Coefficient of 
	 * the current reaction 
	 * @param landauTeller the instance of the Landau-Teller coefficient which is 
	 * currently being filled in to produce CTML.
	 */
	private void readLandauTellerCoefficientB(String landauTellerInstance, LandauTeller landauTeller){
		String value = readLandauTellerCoefficientB(landauTellerInstance);
		if(value!=null){
			LandauTellerCoefficientB B = new LandauTellerCoefficientB();
			landauTeller.setB(B);
			B.setValue(value);
			String units = readLandauTellerCoefficientBUnits(landauTellerInstance);
			if(units!=null){
				B.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the Landau-Teller Coefficient B.
	 * 
	 * @param landauTellerInstance the OWL instance id of the Landau-Teller 
	 * Coefficient of the current reaction
	 */
	private String readLandauTellerCoefficientB(String landauTellerInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(landauTellerInstance, appConfigOntokin.getDataPropertyHasLanTellerCoeffB());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the units of the Landau-Teller Coefficient B.
	 * 
	 * @param landauTellerInstance the OWL instance id of the Landau-Teller 
	 * Coefficient of the current reaction
	 */
	private String readLandauTellerCoefficientBUnits(String landauTellerInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(landauTellerInstance, appConfigOntokin.getDataPropertyHasLanTellerCoeffBUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	
	/**
	 * Reads the following Arrhenius Coefficients:</br>
	 * 1. the pre-exponential factor; and</br>
	 * 2. the units of the pre-exponential factor. 
	 * 
	 * @param the OWL instance id of the Modified Arrhenius Coefficient of 
	 * the current reaction 
	 * @param arrhenius the instance of the Arrhenius coefficient which is 
	 * currently being filled in to produce CTML.
	 */
	private void readPreExponentialFactor(String arrheniusInstance, Arrhenius arrhenius){
		String value = readPreExponentialFactor(arrheniusInstance);
		if(value!=null){
			ArrheniusCoefficientA A = new ArrheniusCoefficientA();
			arrhenius.setA(A);
			A.setValue(value);
			String units = readPreExponentialFactorUnits(arrheniusInstance);
			if(units!=null){
				A.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the pre-exponential factor. 
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readPreExponentialFactor(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrCoeffA());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the pre-exponential factor units.  
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readPreExponentialFactorUnits(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrCoeffAUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the following Arrhenius Coefficients:</br>
	 * 1. the temperature exponent; and</br>
	 * 2. the units of the temperature exponent. 
	 * 
	 * @param the OWL instance id of the Modified Arrhenius Coefficient of 
	 * the current reaction 
	 * @param arrhenius the instance of the Arrhenius coefficient which is 
	 * currently being filled in to produce CTML.
	 */
	private void readTemperatureExponent(String arrheniusInstance, Arrhenius arrhenius){
		String value = readTemperatureExponent(arrheniusInstance);
		if(value!=null){
			ArrheniusCoefficientb b = new ArrheniusCoefficientb();
			arrhenius.setB(b);
			b.setValue(value);
			String units = readTemperatureExponentUnits(arrheniusInstance);
			if(units!=null){
				b.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the temperature exponent. 
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readTemperatureExponent(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrCoeffb());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the temperature exponent units.  
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readTemperatureExponentUnits(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrCoeffbUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the following Arrhenius Coefficient:</br>
	 * 1. the activation energy; and</br>
	 * 2. the units of the activation energy. 
	 * 
	 * @param the OWL instance id of the Modified Arrhenius Coefficient of 
	 * the current reaction 
	 * @param arrhenius the instance of the Arrhenius coefficient which is 
	 * currently being filled in to produce CTML.
	 */
	private void readActivationEnergy(String arrheniusInstance, Arrhenius arrhenius){
		String value = readActivationEnergy(arrheniusInstance);
		if(value!=null){
			ArrheniusCoefficientE E = new ArrheniusCoefficientE();
			arrhenius.setE(E);
			E.setValue(value);
			String units = readActivationEnergyUnits(arrheniusInstance);
			if(units!=null){
				E.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the activation energy. 
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readActivationEnergy(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrCoeffE());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the activation energy units.  
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readActivationEnergyUnits(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrCoeffEUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the following Arrhenius Coefficient:</br>
	 * 1. the reference pressure; and</br>
	 * 2. the units of the reference pressure. 
	 * 
	 * @param the OWL instance id of the Modified Arrhenius Coefficient of 
	 * the current reaction 
	 * @param arrhenius the instance of the Arrhenius coefficient which is 
	 * currently being filled in to produce CTML.
	 */
	private void readReferencePressure(String arrheniusInstance, Arrhenius arrhenius){
		String value = readReferencePressure(arrheniusInstance);
		if(value!=null){
			ArrheniusCoefficientP P = new ArrheniusCoefficientP();
			arrhenius.setP(P);
			P.setValue(value);
			String units = readReferencePressureUnits(arrheniusInstance);
			if(units!=null){
				P.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the reference pressure. 
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readReferencePressure(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrRefPressure());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the reference pressure units.  
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readReferencePressureUnits(String arrheniusInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(arrheniusInstance, appConfigOntokin.getDataPropertyHasArrRefPressureUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the following Coverage Dependency parameters:</br>
	 * 1. the pre-exponential modifier; and</br>
	 * 2. the units of the pre-exponential modifier. 
	 * 
	 * @param the OWL instance id of the surface-coverage coefficient of 
	 * the current reaction
	 * @param coverage the instance of the surface-coverage coefficient which 
	 * is currently being filled in to produce CTML.
	 */
	private void readPreExponentialModifier(String coverageInstance, Coverage coverage){
		String value = readPreExponentialModifier(coverageInstance);
		if(value!=null){
			CoverageParameterA A = new CoverageParameterA();
			coverage.setA(A);
			A.setValue(value);
			String units = readPreExponentialModifierUnits(coverageInstance);
			if(units!=null){
				A.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the pre-exponential modifier. 
	 * 
	 * @param arrheniusInstance the OWL instance id of the Modified Arrhenius 
	 * Coefficient of the current reaction
	 */
	private String readPreExponentialModifier(String coverageInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(coverageInstance, appConfigOntokin.getDataPropertyHasCovDepA());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the pre-exponential modifier units.  
	 * 
	 * @param coverageInstance the OWL instance id of the surface-coverage 
	 * coefficient of the current reaction
	 */
	private String readPreExponentialModifierUnits(String coverageInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(coverageInstance, appConfigOntokin.getDataPropertyHasCovDepAUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	/**
	 * Reads the following Coverage Dependency parameters:</br>
	 * 1. the reaction order modifier; and</br>
	 * 2. the reaction order modifier units. 
	 * 
	 * @param coverageInstance the OWL instance id of the surface-coverage 
	 * coefficient of the current reaction
	 * @param coverage the instance of the surface-coverage coefficient which 
	 * is currently being filled in to produce CTML.
	 */
	private void readReactionOrderModifier(String coverageInstance, Coverage coverage){
		String value = readReactionOrderModifier(coverageInstance);
		if(value!=null){
			CoverageParameterM M = new CoverageParameterM();
			coverage.setM(M);
			M.setValue(value);
			String units = readReactionOrderModifierUnits(coverageInstance);
			if(units!=null){
				M.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the reaction order modifier.
	 * 
	 * @param coverageInstance the OWL instance id of the surface-coverage 
	 * coefficient of the current reaction
	 */
	private String readReactionOrderModifier(String coverageInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(coverageInstance, appConfigOntokin.getDataPropertyHasCovDepM());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the reaction order modifier units.  
	 * 
	 * @param coverageInstance the OWL instance id of the surface-coverage 
	 * coefficient of the current reaction
	 */
	private String readReactionOrderModifierUnits(String coverageInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(coverageInstance, appConfigOntokin.getDataPropertyHasCovDepMUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the following Coverage Dependency parameters:</br>
	 * 1. the exponential modifier; and</br>
	 * 2. the exponential modifier units. 
	 * 
	 * @param coverageInstance the OWL instance id of the surface-coverage 
	 * coefficient of the current reaction
	 * @param coverage the instance of the surface-coverage coefficient which 
	 * is currently being filled in to produce CTML.
	 */
	private void readExponentialModifier(String coverageInstance, Coverage coverage){
		String value = readExponentialModifier(coverageInstance);
		if(value!=null){
			CoverageParameterE E = new CoverageParameterE();
			coverage.setE(E);
			E.setValue(value);
			String units = readExponentialModifierUnits(coverageInstance);
			if(units!=null){
				E.setUnits(units);
			}
		}
	}
	
	/**
	 * Reads the exponential modifier.
	 * 
	 * @param coverageInstance the OWL instance id of the surface-coverage 
	 * coefficient of the current reaction
	 */
	private String readExponentialModifier(String coverageInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(coverageInstance, appConfigOntokin.getDataPropertyHasCovDepE());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Reads the exponential modifier units.  
	 * 
	 * @param coverageInstance the OWL instance id of the surface-coverage 
	 * coefficient of the current reaction
	 */
	private String readExponentialModifierUnits(String coverageInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(coverageInstance, appConfigOntokin.getDataPropertyHasCovDepEUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

}
