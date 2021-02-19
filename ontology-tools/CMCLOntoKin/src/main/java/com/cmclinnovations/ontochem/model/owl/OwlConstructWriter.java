package com.cmclinnovations.ontochem.model.owl;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.CtmlConverterState;
import com.cmclinnovations.ontochem.model.configuration.OntoChemKB;
import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.reference.data.structure.Creator;
import com.cmclinnovations.ontochem.model.reference.data.structure.Journal;
import com.cmclinnovations.ontochem.model.reference.data.structure.JournalSpecification;
import com.cmclinnovations.ontochem.model.reference.data.structure.PublicationSpecification;
import com.cmclinnovations.ontochem.model.reference.data.structure.Reference;
import com.cmclinnovations.ontochem.model.utils.CtmlConverterUtils;

/**
 * This class holds all the methods to create OWL
 * classes, properties, instances and property values.
 * 
 * @author msff2
 *
 */
public class OwlConstructWriter extends CtmlConverter implements IOwlConstructWriter{
	
	private Logger logger = org.slf4j.LoggerFactory.getLogger(OwlConstructWriter.class);

	/**
	 * Add a data property to an instance of a class.</p>
	 * If the class is not available in the data factory, 
	 * it creates the class. </br>
	 * If the instance is not available, it will create the 
	 * instance to add the property.</br>
	 *  
	 * @param basePath
	 * @param clasName
	 * @param instance
	 * @param dataPropertyIRI
	 * @param dataPropertyValue
	 * @throws OntoException
	 */
	public void addProperty(String basePath, String clasName, String instance, IRI dataPropertyIRI, String dataPropertyValue) throws OntoException {
		// Creates a class.
		OWLClass clas = createOWLClass(dataFactory, basePathTBox, clasName);
		// Creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Creates the value of the data property being created
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyIRI.getIRIString(), dataPropertyValue);
		// Reads the data property
		OWLDataProperty dataProperty = dataFactory
				.getOWLDataProperty(dataPropertyIRI);
		// Adds to the ontology the instance of the class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clas, individual)));
		// Adds to the ontology the comment about a mechanism in
		// CTML
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal)));
	}
	
	/**
	 * Add a data property to an instance of a class.</p>
	 * If the class is not available in the data factory, 
	 * it creates the class. </br>
	 * If the instance is not available, it will create the 
	 * instance to add the property.</br> 
	 * 
	 * @param basePath
	 * @param className
	 * @param instance
	 * @param dataProperty
	 * @param dataPropertyValue
	 * @throws OntoException
	 */
	public void addProperty(String basePath, String className, String instance, String dataPropertyName, String dataPropertyValue) throws OntoException {
		// Creates a class.
		OWLClass clas = createOWLClass(dataFactory, basePathTBox, className);
		// Creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Creates the value of the data property being created
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyName, dataPropertyValue);
		// Creates the data property
		OWLDataProperty dataPropertyCreated = createOWLDataProperty(dataFactory, basePathTBox,
				dataPropertyName, HASH);
		// Adds to the ontology the instance of the class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clas, individual)));
		// Adds to the ontology the comment about a mechanism in
		// CTML
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataPropertyCreated, individual, literal)));
	}
	
	/**
	 * Adds type to an instance.
	 * 
	 * @param basePath
	 * @param clasName
	 * @param instance
	 * @throws OntoException
	 */
	public void addInstanceType(String basePath, String clasName, String instance) throws OntoException {
		// Creates a class.
		OWLClass clas = createOWLClass(dataFactory, basePathTBox, clasName);
		// Readas, if not available creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Adds to the ontology the instance of the class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clas, individual)));
	}
	
	public void addElementId(String basePath, String id) throws OntoException {
		elementMetaDataInstanceId++;
		addProperty(basePath, appConfigOntokin.getElementMetadata(),
				appConfigOntokin.getElementMetadata().concat(UNDERSCORE).concat(Long.toString(elementMetaDataInstanceId)),
				DublinCoreVocabulary.IDENTIFIER.getIRI(), id);
		if(elementData.getSourceComment()!=null){
			addDataProperty(basePath, HASH, appConfigOntokin.getSourceComment(), elementData.getSourceComment(),
					appConfigOntokin.getElementMetadata().concat(UNDERSCORE)
							.concat(Long.toString(elementMetaDataInstanceId)));
		}
	}

	public void addElementArray(String basePath, String elementArray) throws OntoException{
		for(String element:CtmlConverterUtils.getElements(elementArray)){
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyHasElement(), phaseType,
					appConfigOntokin.getOntokinElement(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)),
					appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(element));
		}
	}
	
	public void addAtomArray(String basePath, String atomArray) throws OntoException{
		for(String atomVsQuantityPair:CtmlConverterUtils.getAtomSpecifications(atomArray)){
			elementSpecificationInstanceId++;
			String speciesId = speciesUniqueIDMap.get(species.getName().concat(UNDERSCORE).concat(speciesData.getId()));
			// Links a species to its element specification that consists
			// of an element and the quantity of the element
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyElementSpecification(),
					appConfigOntokin.getClassSpecies(), appConfigOntokin.getClassElementSpecification(), speciesId,
					appConfigOntokin.getClassElementSpecification().concat(UNDERSCORE)
							.concat(Long.toString(elementSpecificationInstanceId)));
			// Links an element specification to its element
			addObjectProperty(basePath, appConfigOntokin.getIndicatesNumberOf(),
					appConfigOntokin.getClassElementSpecification(), appConfigOntokin.getOntokinElement(),
					appConfigOntokin.getClassElementSpecification().concat(UNDERSCORE)
							.concat(Long.toString(elementSpecificationInstanceId)), 
							appConfigOntokin.getOntokinElement().concat(UNDERSCORE).
							concat(CtmlConverterUtils.getElement(atomVsQuantityPair)));
			// Links an element specification to its element
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyHasAtom(),
					appConfigOntokin.getClassSpecies(), appConfigOntokin.getOntokinElement(),
					speciesId, appConfigOntokin.getOntokinElement().concat(UNDERSCORE).
							concat(CtmlConverterUtils.getElement(atomVsQuantityPair)));
			// Adds the quantity of an element to it
			addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyNumberOfElement(), 
					CtmlConverterUtils.getElementQuantity(atomVsQuantityPair), 
					appConfigOntokin.getClassElementSpecification().concat(UNDERSCORE)
					.concat(Long.toString(elementSpecificationInstanceId)));
		}
	}

	/**
	 * Splits a list of product:quanity pairs using space.</br>
	 * Creates the object property hasProductSpecification (if it 
	 * is unavailable).</br>  
	 * Creates an instance of the class Product Specification and connects</br>
	 * the instance to its reaction.</br>
	 * Adds the product species to the instance.</br>
	 * Adds the Stoichiometric Coefficient of the product to the instance.</br>
	 * 
	 *  @param basePath the base URL used in the OWL ontology being created
	 *  @param products a list of product:quantity pairs
	 */
	public void addProducts(String basePath, String products) throws OntoException{
		for(String productVsStoichioCoeff:CtmlConverterUtils.getProductSpecifications(products)){
			stoichiometricCoeffInstanceId++;
			// Links a reaction to its product specification that consists
			// of a species and its Stoichiometric coefficient
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyProductSpecification(),
					appConfigOntokin.getClassReaction(), appConfigOntokin.getClassStoichiometricCoefficient(),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
									.concat(Long.toString(reactionSerialNo)),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassStoichiometricCoefficient()).concat(UNDERSCORE)
							.concat(Long.toString(stoichiometricCoeffInstanceId)));
			// Links a reaction to its product that is a species
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyHasProduct(),
					appConfigOntokin.getClassReaction(), appConfigOntokin.getClassProduct(),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
									.concat(Long.toString(reactionSerialNo)),
					appConfigOntokin.getClassProduct().concat(UNDERSCORE)
							.concat(Long.toString(stoichiometricCoeffInstanceId)));
			// Calls the method that links the product specification to 
			// the product 
			addProductSpecificationToSpecies(productVsStoichioCoeff);
			// Adds the Stoichiometric Coefficient to the product specification
			addProductStoichiometricCoefficeint(productVsStoichioCoeff);
		}
	}
	
	/**
	 * Adds the Stoichiometric Coefficient to the product specification
	 * 
	 * @param productVsStoichioCoeff a pair of product and quantity
	 * @throws OntoException
	 */
	private void addProductStoichiometricCoefficeint(String productVsStoichioCoeff) throws OntoException{
	addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyStoichiometricCoefficient(), 
			CtmlConverterUtils.getStoichioMetricCoefficient(productVsStoichioCoeff), 
			CtmlConverterUtils.getClassName(appConfigOntokin.getClassStoichiometricCoefficient()).concat(UNDERSCORE)
			.concat(Long.toString(stoichiometricCoeffInstanceId)));
	}
	
	/**
	 * Retrieves the OWL instance id of a product and establishes</br> 
	 * a link between the product and its specification, which</br> 
	 * includes not only the product but also its Stoichiometric</br>
	 * coefficient.
	 * 
	 * @param productVsStoichioCoeff a pair of product and its Stoichiometric</br>
	 * Coefficient
	 * @throws OntoException
	 */
	private void addProductSpecificationToSpecies(String productVsStoichioCoeff) throws OntoException{
		// Extracts the product species id
		String prodcutSpeciesId = extractSpeciesUniqueIdFromMaterial(
				CtmlConverterUtils.getSpecies(productVsStoichioCoeff));
		if (prodcutSpeciesId == null) {
			prodcutSpeciesId = extractSpeciesUniqueIdFromGasPhase(
					CtmlConverterUtils.getSpecies(productVsStoichioCoeff));
		}
		if(prodcutSpeciesId == null){
			logger.error("Product species id is not found in the phases.");
			throw new OntoException("Product species id is not found in the phases.");
		}
		addProperty(basePath, appConfigOntokin.getClassProduct(),
				appConfigOntokin.getClassProduct().concat(UNDERSCORE).concat(Long.toString(stoichiometricCoeffInstanceId)),
				OWLRDFVocabulary.RDFS_LABEL.getIRI(), CtmlConverterUtils.getSpecies(productVsStoichioCoeff));
		//Saves the product name vs product id to reuse the product id 
		// while codifying the corresponding reaction order.
		productVsProductIdMap.put(CtmlConverterUtils.getSpecies(productVsStoichioCoeff),
				stoichiometricCoeffInstanceId);
		// Links a product specification to its product species
		linkProductSpecificationToProduct(prodcutSpeciesId);
	}

	/**
	 * Links a product specification to its product.
	 * 
	 * @param productSpeciesId
	 * @throws OntoException
	 */
	private void linkProductSpecificationToProduct(String productSpeciesId) throws OntoException{
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyIndicatesMultiplicityOf(),
				appConfigOntokin.getClassStoichiometricCoefficient(), appConfigOntokin.getClassProduct(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassStoichiometricCoefficient()).concat(UNDERSCORE)
						.concat(Long.toString(stoichiometricCoeffInstanceId)),
				appConfigOntokin.getClassProduct().concat(UNDERSCORE)
						.concat(Long.toString(stoichiometricCoeffInstanceId)));
		// Creates the same as relation between the product and the species 
		// that comes out in the gas phase or other phases.
		addObjectProperty(basePath, OWLRDFVocabulary.OWL_SAME_AS.toString(),
				appConfigOntokin.getClassProduct(), appConfigOntokin.getClassSpecies(),
				appConfigOntokin.getClassProduct().concat(UNDERSCORE)
						.concat(Long.toString(stoichiometricCoeffInstanceId)),
				productSpeciesId);
	}
	
	/**
	 * Splits a list of reactant:quanity pairs using space.</br>
	 * Creates the object property hasReactantSpecification (if it 
	 * is unavailable).</br>  
	 * Creates an instance of the class Reactant Specification and connects</br>
	 * the instance to the reaction it belongs to.</br>
	 * Adds the reactant to the instance.</br>
	 * Adds the Stoichiometric Coefficient of the reactant to the instance.</br>
	 * 
	 *  @param basePath the base URL used in the OWL ontology being created
	 *  @param reactants a list of reactant:quantity pairs
	 */
	public void addReactants(String basePath, String reactants) throws OntoException{
		for(String reactantVsStoichioCoeff:CtmlConverterUtils.getReactantSpecifications(reactants)){
			stoichiometricCoeffInstanceId++;
			// Links a reaction to the Stoichiometric coefficient of a reactant
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyReactantSpecification(),
					appConfigOntokin.getClassReaction(), appConfigOntokin.getClassStoichiometricCoefficient(),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
									.concat(Long.toString(reactionSerialNo)),
									CtmlConverterUtils.getClassName(appConfigOntokin.getClassStoichiometricCoefficient()).concat(UNDERSCORE)
							.concat(Long.toString(stoichiometricCoeffInstanceId)));
			// Links a reaction to its reactant that is a species
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyHasReactant(),
					appConfigOntokin.getClassReaction(), appConfigOntokin.getClassReactant(),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
									.concat(Long.toString(reactionSerialNo)),
					appConfigOntokin.getClassReactant().concat(UNDERSCORE)
							.concat(Long.toString(stoichiometricCoeffInstanceId)));
			// Calls the method that links the reactant specification to 
			// the reactant species
			addReactantSpecificationToSpecies(reactantVsStoichioCoeff);
			// Adds the Stoichiometric Coefficient to the reactant specification
			addReactantStoichiometricCoefficeint(reactantVsStoichioCoeff);
		}
	}
	
	/**
	 * Adds the Stoichiometric Coefficient to the reactant specification.
	 * 
	 * @param reactantVsStoichioCoeff a pair of reactant and quantity
	 * @throws OntoException
	 */
	private void addReactantStoichiometricCoefficeint(String reactantVsStoichioCoeff) throws OntoException{
		addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyStoichiometricCoefficient(), 
				CtmlConverterUtils.getStoichioMetricCoefficient(reactantVsStoichioCoeff), 
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassStoichiometricCoefficient()).concat(UNDERSCORE)
				.concat(Long.toString(stoichiometricCoeffInstanceId)));
	}
	
	/**
	 * Retrieves the OWL instance id of a reactant and establishes</br> 
	 * a link between the reactant and its specification, which</br> 
	 * includes not only the reactant but also the its Stoichiometric</br>
	 * Coefficient.
	 * 
	 * @param reactantVsStoichioCoeff a pair of reactant and Stoichiometric</br>
	 * Coefficient
	 * @throws OntoException
	 */
	private void addReactantSpecificationToSpecies(String reactantVsStoichioCoeff) throws OntoException{
		// Extracts the reactant species id
		String reactantSpeciesId = extractSpeciesUniqueIdFromMaterial(
				CtmlConverterUtils.getSpecies(reactantVsStoichioCoeff));
		if (reactantSpeciesId == null) {
			reactantSpeciesId = extractSpeciesUniqueIdFromGasPhase(
					CtmlConverterUtils.getSpecies(reactantVsStoichioCoeff));
		}
		if(reactantSpeciesId == null){
			logger.error("Reactant species id is not found in the phases.");
			throw new OntoException("Reactant species id is not found in the phases.");
		}
		addProperty(basePath, appConfigOntokin.getClassReactant(),
				appConfigOntokin.getClassReactant().concat(UNDERSCORE).concat(Long.toString(stoichiometricCoeffInstanceId)),
				OWLRDFVocabulary.RDFS_LABEL.getIRI(), CtmlConverterUtils.getSpecies(reactantVsStoichioCoeff));
		//Saves the reactant name vs reactant id to reuse the reactant id 
		// while codifying the corresponding reaction order.
		reactantVsReactantIdMap.put(CtmlConverterUtils.getSpecies(reactantVsStoichioCoeff),
				stoichiometricCoeffInstanceId);
		// Links a reactant specification to its reactant
		linkReactantSpecificationToReactant(reactantSpeciesId);
	}
	
	/**
	 * Links a reactant specification to its reactant.
	 * 
	 * @param reactantSpeciesId
	 * @throws OntoException
	 */
	private void linkReactantSpecificationToReactant(String reactantSpeciesId) throws OntoException{
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyIndicatesMultiplicityOf(),
				appConfigOntokin.getClassStoichiometricCoefficient(), appConfigOntokin.getClassReactant(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassStoichiometricCoefficient()).concat(UNDERSCORE)
						.concat(Long.toString(stoichiometricCoeffInstanceId)), 
				appConfigOntokin.getClassReactant().concat(UNDERSCORE)
						.concat(Long.toString(stoichiometricCoeffInstanceId)));
		// Creates the same as relation between the reactant and the species 
		// that comes out in the gas phase or other phases.
		addObjectProperty(basePath, OWLRDFVocabulary.OWL_SAME_AS.toString(),
				appConfigOntokin.getClassReactant(), appConfigOntokin.getClassSpecies(),
				appConfigOntokin.getClassReactant().concat(UNDERSCORE)
						.concat(Long.toString(stoichiometricCoeffInstanceId)),
						reactantSpeciesId);
	}
	
	public void addSpeciesId(String basePath, String id) throws OntoException {
		speciesMetaDataInstanceId++;
		addProperty(basePath, appConfigOntokin.getSpeciesMetadata(),
				appConfigOntokin.getSpeciesMetadata().concat(UNDERSCORE).concat(Long.toString(speciesMetaDataInstanceId)),
				DublinCoreVocabulary.IDENTIFIER.getIRI(), id);
		// Adds the source comment to the current species data.
		if (speciesData.getSourceComment() != null) {
			addDataProperty(basePath, HASH, appConfigOntokin.getSourceComment(), speciesData.getSourceComment(),
					appConfigOntokin.getSpeciesMetadata().concat(UNDERSCORE)
							.concat(Long.toString(speciesMetaDataInstanceId)));
			speciesData.setSourceComment(null);
		}
	}
	
	public void addReactionDataId(String basePath, String id) throws OntoException {
		reactionMetaDataInstanceId++;
		addProperty(basePath, appConfigOntokin.getReactionMetadata(),
				appConfigOntokin.getReactionMetadata().concat(UNDERSCORE).concat(Long.toString(reactionMetaDataInstanceId)),
				DublinCoreVocabulary.IDENTIFIER.getIRI(), id);
		// Adds the source comment to the current reaction data.
		if (reactionData.getSourceComment() != null) {
			addDataProperty(basePath, HASH, appConfigOntokin.getSourceComment(), reactionData.getSourceComment(),
					appConfigOntokin.getReactionMetadata().concat(UNDERSCORE)
							.concat(Long.toString(reactionMetaDataInstanceId)));
			reactionData.setSourceComment(null);
		}
	}

	public void addReactionId(String basePath, String id) throws OntoException {
		addProperty(basePath, reactionType,
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				DublinCoreVocabulary.IDENTIFIER.getIRI(), id);
	}	
	
	public void addReaction(String basePath) throws OntoException {
		reactionInstanceId++;
		// Connects the reaction object being created to its metadata object,
		//  which has already been created with the following types of reaction
		//  data: <reactionData id="GAS_reaction_data" caseSensitive="yes">
		addObjectProperty(basePath, appConfigOntokin.getReactionMetadataProperty(), reactionType,
				appConfigOntokin.getReactionMetadata(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(++reactionSerialNo)),
				appConfigOntokin.getReactionMetadata().concat(UNDERSCORE)
						.concat(Long.toString(reactionMetaDataInstanceId)));
		// Connects the reaction object being created with a Gas Phase
		// or a Material.
		connectReactionToGasPhaseOrMaterial(basePath);
		// Adds the source comment to the current reaction.
		if (reaction.getSourceComment() != null) {
			addDataProperty(basePath, HASH, appConfigOntokin.getSourceComment(), reaction.getSourceComment(),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
							Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)));
			reaction.setSourceComment(null);
		}
	}

	public void connectReactionToGasPhaseOrMaterial(String basePath) throws OntoException {
		if (reactionData.getId() == null) {
			logger.error("The id of reaction data is null.");
			throw new OntoException("The id of reaction data is null.");
		}
		if (reactionDataIdVsPhaseMap.containsKey(reactionData.getId())) {
			if (reactionDataIdVsPhaseMap.get(reactionData.getId())
					.equalsIgnoreCase(appConfigOntokin.getClassGasPhase())) {
				connectReactionToGasPhase(basePath, reactionData.getId());
			} else if (reactionDataIdVsPhaseMap.get(reactionData.getId())
					.equalsIgnoreCase(appConfigOntokin.getClassBulkPhase())
					|| reactionDataIdVsPhaseMap.get(reactionData.getId())
							.equalsIgnoreCase(appConfigOntokin.getClassSitePhase())) {
				connectReactionToMaterial(basePath, reactionData.getId());
			}
		}
	}
	
	private void connectReactionToGasPhase(String basePath, String dataSrc) throws OntoException {
		if(gasPhaseDataSrcVsInstanceMap.containsKey(dataSrc)){
			addObjectProperty(basePath, appConfigOntokin.getOntokinBelongsToPhase(),
					reactionType, appConfigOntokin.getClassGasPhase(),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
									.concat(Long.toString(reactionSerialNo)),
					gasPhaseDataSrcVsInstanceMap.get(dataSrc));
		}
	}

	private void connectReactionToMaterial(String basePath, String dataSrc) throws OntoException {
		if(!reactionArrayVsMaterialMap.containsKey(dataSrc)){
			logger.error("A reaction data source is not found in reactionArrayVsMaterialMap.");
			throw new OntoException("A reaction data source is not found in reactionArrayVsMaterialMap.");
		}
		if(!materialVsInstanceMap.containsKey(reactionArrayVsMaterialMap.get(dataSrc))){
			logger.error("The following material is not found in materialVsInstanceMap.");
			throw new OntoException("A reaction data source is not found in materialVsInstanceMap.");			
		}
		linkReactionToMaterial(basePath, dataSrc);
	}
	
	private void linkReactionToMaterial(String basePath, String dataSrc) throws OntoException {
		addObjectProperty(basePath, appConfigOntokin.getOntokinBelongsToMaterial(), reactionType,
				appConfigOntokin.getClassMaterial(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassMaterial().concat(UNDERSCORE)
						.concat(materialVsInstanceMap.get(reactionArrayVsMaterialMap.get(dataSrc))));
	}
	
	public void addArrheniusCoefficient(String basePath) throws OntoException {
		rateCoeffArrheniusInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getClassArrheniusCoefficient(), appConfigOntokin
				.getClassArrheniusCoefficient().concat(UNDERSCORE).concat(Long.toString(rateCoeffArrheniusInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyArrheniusRateCoeff(),
				reactionType, appConfigOntokin.getClassArrheniusCoefficient(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassArrheniusCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffArrheniusInstanceId)));
	}

	public void addStickingCoefficient(String basePath) throws OntoException {
		rateCoeffStickingInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getClassStickingCoefficient(), appConfigOntokin
				.getClassStickingCoefficient().concat(UNDERSCORE).concat(Long.toString(rateCoeffStickingInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyStickingCoeff(),
				reactionType, appConfigOntokin.getClassStickingCoefficient(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassStickingCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffStickingInstanceId)));
	}
	
	
	public void addLandauTellerCoefficient(String basePath) throws OntoException {
		rateCoeffLanTellerInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getClassLandauTellerCoefficient(),
				appConfigOntokin.getClassLandauTellerCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffLanTellerInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyLandauTellerRateCoeff(),
				reactionType, appConfigOntokin.getClassLandauTellerCoefficient(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassLandauTellerCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffLanTellerInstanceId)));
	}
	
	public void addEfficiencyInstance(String basePath) throws OntoException {
		reactionEfficiencyInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getClassThirdBodyEfficiency(), appConfigOntokin
				.getClassThirdBodyEfficiency().concat(UNDERSCORE).concat(Long.toString(reactionEfficiencyInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyThirdBodyEfficiency(),
				reactionType, appConfigOntokin.getClassThirdBodyEfficiency(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassThirdBodyEfficiency().concat(UNDERSCORE)
						.concat(Long.toString(reactionEfficiencyInstanceId)));
	}
	
	public void addFallOffInstance(String basePath) throws OntoException {
		rateCoeffFallOffInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getClassFallOffModelCoefficient(),
				appConfigOntokin.getClassFallOffModelCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffFallOffInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyFallOffModelCoeff(),
				reactionType, appConfigOntokin.getClassFallOffModelCoefficient(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(
						Long.toString(reactionInstanceId)).concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassFallOffModelCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffFallOffInstanceId)));
	}
	
	public void addFallOffType(String basePath) throws OntoException {
		addInstanceType(basePath, CtmlConverterUtils.getReactionClass(fallOff.getType()),
				appConfigOntokin.getClassFallOffModelCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffFallOffInstanceId)));
	}
	
	/**
	 * Codifies in OWL the species which has the thirdbody efficiency in </br> 
	 * a pressure dependent reaction. Also codifies the value of efficiency.
	 */
	public void addSpeciesEfficiency(String species, String efficiency)
			throws OntoException {
		// Codifies the thirdbody efficiency species 
		addEfficiencyOfSpecies(species);
		// Codifies the value of the thirdbody efficiency
		addEfficiencyValue(efficiency);
	}

	/**
	 * Codifies in OWL the species which has the thirdbody efficiency.
	 * 
	 * @param species species for which the thirdbody efficiency is created
	 * @throws OntoException
	 */
	private void addEfficiencyOfSpecies(String species)
			throws OntoException {
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyHasSpecies(),
				appConfigOntokin.getClassThirdBodyEfficiency(), appConfigOntokin.getClassSpecies(),
				appConfigOntokin.getClassThirdBodyEfficiency().concat(UNDERSCORE) + reactionEfficiencyInstanceId,
				speciesUniqueIDMap.get(species));
	}
	
	/**
	 * Codifies in OWL the thirdbody which is named.
	 * 
	 * @param species the species which is named as the thirdbody in 
	 * the context of a reaction. 
	 */
	public void addNamedThirdBodySpecies(String species) throws OntoException {
		addObjectProperty(basePath, appConfigOntokin.getHasNamedThirdBody(),
				appConfigOntokin.getClassFallOffModelCoefficient(), appConfigOntokin.getClassSpecies(),
				appConfigOntokin.getClassFallOffModelCoefficient().concat(UNDERSCORE) + rateCoeffFallOffInstanceId,
				speciesUniqueIDMap.get(species.concat(UNDERSCORE).concat(appConfigCtml.getGasSpeciesDataId())));
	}
	
	public void addCoverageSpecies(String basePath) throws OntoException {
		if (coverage.getSpecies() != null) {
			if(extractSpeciesUniqueId(coverage.getSpecies(), extractCoverageSpeciesDataSrc())==null){
				throw new OntoException("The unique id of a coverage species was not "
						+ "found in the speciesUniqueIDMap.");		
			}
			addCoverageSpecies(basePath, extractSpeciesUniqueId(coverage.getSpecies(), extractCoverageSpeciesDataSrc()));
		}
	}

	private void addCoverageSpecies(String basePath, String speciesUniqueId) throws OntoException {
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyHasSpecies(),
				appConfigOntokin.getClassCoverageCoefficient(), appConfigOntokin.getClassSpecies(),
				appConfigOntokin.getClassCoverageCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffCovDepInstanceId)),
				speciesUniqueId);
	}
	
	private String extractCoverageSpeciesDataSrc() throws OntoException {
		if(extractSpeciesDataSrc()==null){
			throw new OntoException("A coverage species of a reaction was "
					+ "not found in its corresponding phase.");			
		}
		return reactionArrayVsSpeciesArrayMap.get(reactionData.getId());
	}
	
	private String extractSpeciesDataSrc() throws OntoException {
		if (reactionArrayVsSpeciesArrayMap.containsKey(reactionData.getId())) {
			return reactionArrayVsSpeciesArrayMap.get(reactionData.getId());
		}
		return null;
	}
	
	private String extractSpeciesUniqueId(String species, String speciesDataSrc) throws OntoException {
		if(speciesUniqueIDMap.containsKey(species.concat(UNDERSCORE).concat(speciesDataSrc))){
			return speciesUniqueIDMap.get(species.concat(UNDERSCORE).concat(speciesDataSrc));
		}
		return null;
	}
		
	private void addEfficiencyValue(String efficiency)
			throws OntoException {
		addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyHasEfficiencyValue(), efficiency,
				appConfigOntokin.getClassThirdBodyEfficiency(), reactionEfficiencyInstanceId);		
	}
	
	public void addDefaultEfficiency(String defaultEfficiency) throws OntoException {
		addDataProperty(basePath, HASH, appConfigOntokin.getHasDefaultThirdBodyEfficiency(), defaultEfficiency,
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
						.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)));
	}
	
	public void addCovDependencyCoefficient(String basePath) throws OntoException {
		rateCoeffCovDepInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getClassCoverageCoefficient(), appConfigOntokin
				.getClassCoverageCoefficient().concat(UNDERSCORE).concat(Long.toString(rateCoeffCovDepInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyCoverageCoefficient(),
				reactionType, appConfigOntokin.getClassCoverageCoefficient(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
						.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassCoverageCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffCovDepInstanceId)));		
	}
	
	public void addChebCoefficient(String basePath) throws OntoException {
		rateCoeffChebInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getClassCHEBCoefficient(), appConfigOntokin.getClassCHEBCoefficient()
				.concat(UNDERSCORE).concat(Long.toString(rateCoeffChebInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyCHEBRateCoeff(),
				reactionType, appConfigOntokin.getClassCHEBCoefficient(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
						.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getClassCHEBCoefficient().concat(UNDERSCORE)
						.concat(Long.toString(rateCoeffChebInstanceId)));
	}
	
	/**
	 * Adds the name of Arrhenius rate coefficients. 
	 * 
	 * @param name the name of an Arrhenius Coefficients.
	 */
	public void addArrheniusCoeffName(String name, String coefficientType, Long instanceId) throws OntoException{
		if (name != null) {
				addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), name,
						coefficientType.concat(UNDERSCORE)
								.concat(Long.toString(instanceId)));
		}
	}
	
	/**
	 * Adds the name of a material to its instance.
	 * 
	 * @param name the name of a material being processed.
	 * @throws OntoException
	 */
	private void addMaterialName(String name) throws OntoException{
		if (name != null) {
			try {
				addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), name,
						appConfigOntokin.getClassMaterial().concat(UNDERSCORE)
								.concat(Long.toString(materialInstanceId)));
			} catch (OntoException e) {
				logger.error("The name of material could not be created.");
				throw new OntoException("The name of material could not be created.");
			}
		}
	}
	
	
	public void addReactionOrder(String basePath) throws OntoException {
		reactionOrderInstanceId++;
		addInstanceType(basePath, appConfigOntokin.getReactionOrder(),
				appConfigOntokin.getReactionOrder().concat(UNDERSCORE).concat(Long.toString(reactionOrderInstanceId)));
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyReactionOrder(),
				reactionType, appConfigOntokin.getReactionOrder(),
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
						.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getReactionOrder().concat(UNDERSCORE)
						.concat(Long.toString(reactionOrderInstanceId)));
	}
	
	public void addReactionOrderSpecies(String basePath, String direction, String species) throws OntoException {
		if (extractSpeciesDataSrc() != null) {
			if(extractSpeciesUniqueIdFromMaterial(species)!=null){
				addReactionOrderSpecies(direction, species);
			}else{
				addReactionOrderGasPhaseSpecies(direction, species);
			}
		}else{
			reportDataSourceUnavailability();
		}
	}

	private String extractSpeciesUniqueIdFromGasPhase(String species) throws OntoException{
		return extractSpeciesUniqueId(species, appConfigCtml.getGasSpeciesDataId());
	}
	
	private String extractSpeciesUniqueIdFromMaterial(String species) throws OntoException{
		return extractSpeciesUniqueId(species, reactionArrayVsSpeciesArrayMap.get(reactionData.getId()));
	}
	
	private void reportSpeciesUnavailability() throws OntoException{
		throw new OntoException(
				"The unique id of a reaction order species was not found in the speciesUniqueIDMap.");
	}
	
	private void reportDataSourceUnavailability() throws OntoException{
		throw new OntoException(
				"A data source is missing.");
	}
	
	private void addReactionOrderSpecies(String direction, String species) throws OntoException{
		addSpeciesToReactionOrder(basePath, extractSpeciesUniqueId(species,
				reactionArrayVsSpeciesArrayMap.get(reactionData.getId())), direction, species);

	}
	
	private void addReactionOrderGasPhaseSpecies(String direction, String species) throws OntoException{
		if(extractSpeciesUniqueIdFromGasPhase(species)==null){
			reportSpeciesUnavailability();
		}
		addSpeciesToReactionOrder(basePath,
				extractSpeciesUniqueId(species, appConfigCtml.getGasSpeciesDataId()), direction, species);
	}

	private void addSpeciesToReactionOrder(String basePath, String speciesUniqueId, String direction, String species)
			throws OntoException {
		if (direction != null && !direction.isEmpty()
				&& appConfigCtml.getForwardReactionIndicator().equalsIgnoreCase(direction)) {
			if(reactantVsReactantIdMap.containsKey(species)){
				addForwardReactionOrderToReactant(species);
			}else if(productVsProductIdMap.containsKey(species)){
				addForwardReactionOrderToProduct(species);
			}
		} else if (direction != null && !direction.isEmpty()
				&& appConfigCtml.getReverseReactionIndicator().equalsIgnoreCase(direction)) {
			if(reactantVsReactantIdMap.containsKey(species)){
				addReverseReactionOrderToReactant(species);
			}else if(productVsProductIdMap.containsKey(species)){
				addReverseReactionOrderToProduct(species);
			}
		}
	}

	private void addForwardReactionOrderToReactant(String species) throws OntoException {
		if (speciesLabelVsReactionOrderIdMap.containsKey(species)) {
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyAppliesTo(),
					appConfigOntokin.getReactionOrder(), appConfigOntokin.getClassReactant(),
					appConfigOntokin.getReactionOrder().concat(UNDERSCORE)
							.concat(Long.toString(speciesLabelVsReactionOrderIdMap.get(species))),
					appConfigOntokin.getClassReactant().concat(UNDERSCORE)
							.concat(Long.toString(reactantVsReactantIdMap.get(species))));
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyParticipatesIn(),
					appConfigOntokin.getClassReactant(), appConfigOntokin.getClassForwardReaction(),
					appConfigOntokin.getClassReactant().concat(UNDERSCORE)
							.concat(Long.toString(reactantVsReactantIdMap.get(species))),
					appConfigOntokin.getClassForwardReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
			addObjectProperty(basePath, OWLRDFVocabulary.OWL_SAME_AS.toString(),
					appConfigOntokin.getClassForwardReaction(), appConfigOntokin.getClassReaction(),
					appConfigOntokin.getClassForwardReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
		}
	}
	
	private void addForwardReactionOrderToProduct(String species) throws OntoException {
		if (speciesLabelVsReactionOrderIdMap.containsKey(species)) {
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyAppliesTo(),
					appConfigOntokin.getReactionOrder(), appConfigOntokin.getClassProduct(),
					appConfigOntokin.getReactionOrder().concat(UNDERSCORE)
							.concat(Long.toString(speciesLabelVsReactionOrderIdMap.get(species))),
					appConfigOntokin.getClassProduct().concat(UNDERSCORE)
							.concat(Long.toString(productVsProductIdMap.get(species))));
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyParticipatesIn(),
					appConfigOntokin.getClassProduct(), appConfigOntokin.getClassForwardReaction(),
					appConfigOntokin.getClassProduct().concat(UNDERSCORE)
							.concat(Long.toString(productVsProductIdMap.get(species))),
					appConfigOntokin.getClassForwardReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
			addObjectProperty(basePath, OWLRDFVocabulary.OWL_SAME_AS.toString(),
					appConfigOntokin.getClassForwardReaction(), appConfigOntokin.getClassReaction(),
					appConfigOntokin.getClassForwardReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
		}
	}

	private void addReverseReactionOrderToProduct(String species) throws OntoException {
		if (speciesLabelVsReactionOrderIdMap.containsKey(species)) {
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyAppliesTo(),
					appConfigOntokin.getReactionOrder(), appConfigOntokin.getClassProduct(),
					appConfigOntokin.getReactionOrder().concat(UNDERSCORE)
							.concat(Long.toString(speciesLabelVsReactionOrderIdMap.get(species))),
					appConfigOntokin.getClassProduct().concat(UNDERSCORE)
							.concat(Long.toString(productVsProductIdMap.get(species))));
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyParticipatesIn(),
					appConfigOntokin.getClassProduct(), appConfigOntokin.getClassReverseReaction(),
					appConfigOntokin.getClassProduct().concat(UNDERSCORE)
							.concat(Long.toString(productVsProductIdMap.get(species))),
					appConfigOntokin.getClassForwardReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
			addObjectProperty(basePath, OWLRDFVocabulary.OWL_SAME_AS.toString(),
					appConfigOntokin.getClassReverseReaction(), appConfigOntokin.getClassReaction(),
					appConfigOntokin.getClassReverseReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
		}
	}
	
	private void addReverseReactionOrderToReactant(String species) throws OntoException {
		if (speciesLabelVsReactionOrderIdMap.containsKey(species)) {
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyAppliesTo(),
					appConfigOntokin.getReactionOrder(), appConfigOntokin.getClassReactant(),
					appConfigOntokin.getReactionOrder().concat(UNDERSCORE)
							.concat(Long.toString(speciesLabelVsReactionOrderIdMap.get(species))),
					appConfigOntokin.getClassReactant().concat(UNDERSCORE)
							.concat(Long.toString(reactantVsReactantIdMap.get(species))));
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyParticipatesIn(),
					appConfigOntokin.getClassReactant(), appConfigOntokin.getClassReverseReaction(),
					appConfigOntokin.getClassReactant().concat(UNDERSCORE)
							.concat(Long.toString(reactantVsReactantIdMap.get(species))),
					appConfigOntokin.getClassForwardReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
			addObjectProperty(basePath, OWLRDFVocabulary.OWL_SAME_AS.toString(),
					appConfigOntokin.getClassReverseReaction(), appConfigOntokin.getClassReaction(),
					appConfigOntokin.getClassReverseReaction().concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)),
					CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
							.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
							.concat(Long.toString(reactionSerialNo)));
		}
	}

	public void addDuplicateInfo(String basePath, String duplicate) throws OntoException {
		addProperty(basePath, reactionType,
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
						.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getReactionDuplicate(), duplicate);
	}

	public void addReversibleInfo(String basePath, String reversible) throws OntoException {
		addProperty(basePath, reactionType,
				CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
						.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)),
				appConfigOntokin.getReactionReverisble(), reversible);
	}
	
	/**
	 * Adds a data property to the OWL representation of the mechanism.
	 *  
	 * @param speciesValidation
	 * @throws OntoException
	 */
	public void addDataProperty(String iri, String pathSeparator, String property, String propertyValue, String individialName) throws OntoException {
		OWLDataProperty identifierProperty = createOWLDataProperty(dataFactory, basePathTBox,
				property, pathSeparator);
		addDataProperty(identifierProperty, propertyValue, individialName);
	}
	
	/**
	 * Attaches a data property value to an instance.
	 * The IRI contains both the name space and the
	 * name of the property. 
	 * 
	 * @param iri
	 * @param propertyValue
	 * @param individialName
	 * @throws OntoException
	 */
	public void addDataProperty(IRI iri, String propertyValue, String individialName) throws OntoException {
		// Reads the data property from the OWL API data factory
		OWLDataProperty identifierProperty = dataFactory.getOWLDataProperty(iri);
		addDataProperty(identifierProperty, propertyValue, individialName);
	}
	
	/**
	 * Attaches a data property value to an instance.
	 * The IRI contains both the name space and the
	 * name of the property. 
	 * 
	 * @param iri
	 * @param propertyValue
	 * @param propertyName
	 * @param individialName
	 * @throws OntoException
	 */
	public void addDataProperty(IRI iri, String propertyValue, String propertyName, String individialName) throws OntoException {
		// Reads the data property from the OWL API data factory
		OWLDataProperty identifierProperty = dataFactory.getOWLDataProperty(iri);
		OWLLiteral literal = createOWLLiteral(dataFactory, propertyName, propertyValue);
		OWLIndividual individual = dataFactory
				.getOWLNamedIndividual(basePathABox.concat(HASH).concat(individialName));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(identifierProperty, individual, literal)));
	}
	
	/**
	 * Attaches a data property value to an instance.
	 * The IRI contains both the name space and the
	 * name of the property.
	 * 
	 * @param iri
	 * @param propertyValue
	 * @param concept
	 * @param instanceId
	 * @throws OntoException
	 */
	public void addDataProperty(IRI iri, String propertyValue, String concept, long instanceId) throws OntoException {
		// Reads the data property from the OWL API data factory
		OWLDataProperty identifierProperty = dataFactory.getOWLDataProperty(iri);
		addDataProperty(identifierProperty, propertyValue, concept.concat(UNDERSCORE).concat(Long.toString(instanceId)));
	}

	/**
	 * Forwards the call to attach a data property value to an instance. 
	 * 
	 * @param iri
	 * @param pathSeparator
	 * @param property
	 * @param propertyValue
	 * @param concept
	 * @param instanceId
	 * @throws OntoException
	 */
	public void addDataProperty(String iri, String pathSeparator, String property, String propertyValue, String concept, long instanceId) throws OntoException {
		OWLDataProperty identifierProperty = createOWLDataProperty(dataFactory, basePathTBox, property, pathSeparator);
		addDataProperty(identifierProperty, propertyValue, concept.concat(UNDERSCORE).concat(Long.toString(instanceId)));
	}

	/**
	 * Attaches a data property value to an instance.
	 * 
	 * @param identifierProperty
	 * @param propertyValue
	 * @param individialName
	 * @throws OntoException
	 */
	private void addDataProperty(OWLDataProperty identifierProperty, String propertyValue, String individialName) throws OntoException {
		OWLLiteral literal = createOWLLiteral(dataFactory, identifierProperty.toStringID(), propertyValue);
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, individialName);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(identifierProperty, individual, literal)));
	}
	
	/**
	 * Creates an OWL class using OWLDataFactory.
	 * </br>
	 * To enable the creation of the class, its name and URL forming
	 * path should be provided.
	 * 
	 * @param ontoFactory an instance of OWLDataFactory.
	 * @param owlFilePath the path for forming the URL. 
	 * @param className the name of the class.
	 * @return an OWL class.
	 * @see OWLDataFactory
	 */
	private OWLClass createOWLClass(OWLDataFactory ontoFactory, String owlFilePath, String className){
		if(className!=null && (className.trim().startsWith("http://") || className.trim().startsWith("https://"))){
			return ontoFactory.getOWLClass(className.trim());
		}
		return ontoFactory.getOWLClass(owlFilePath.concat("#").concat(className));
	}
	
	private OWLIndividual createOWLIndividual(OWLDataFactory ontoFactory, String owlFilePath, String individualName){
		if(individualName!=null && (individualName.trim().startsWith("http://") || individualName.trim().startsWith("https://"))){
			return ontoFactory.getOWLNamedIndividual(individualName.trim());
		}
		return ontoFactory.getOWLNamedIndividual(owlFilePath.concat("#").concat(individualName));
	}
	
	private OWLDataProperty createOWLDataProperty(OWLDataFactory dataFactory, String iri, String propertyName, String separator){
		if(propertyName!=null && (propertyName.trim().startsWith("http://") || propertyName.trim().startsWith("https://"))){
			return dataFactory.getOWLDataProperty(propertyName);
		}
		return dataFactory.getOWLDataProperty(iri.concat(separator).concat(propertyName));
	}
	
	/**
	 * Creates an OWL literal with one of the following data types:</br>
	 * 1. String.
	 * 2. Integer.
	 * 3. Float and
	 * 4. Double
	 * 
	 * @param ontoFactory
	 * @param propertyName
	 * @param literal
	 * @return
	 * @throws OntoException
	 */
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String propertyName, String literal) throws OntoException{
		if(propertyName.startsWith(basePathTBox.concat(HASH))){
			propertyName = propertyName.replace(basePathTBox.concat(HASH), EMPTY);
		}
		if(dataPropertyNameVsTypeMap.containsKey(propertyName.toLowerCase())){
			if(dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()).equals("string")){
				return ontoFactory.getOWLLiteral(literal);
			} else if(dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()).equals("integer")){
				try{
					return ontoFactory.getOWLLiteral(Integer.parseInt(literal));
				}catch(NumberFormatException e){
					throw new OntoException("The following value is not an integer:"+literal);
				}
			} else if(dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()).equals("float")){
				try{
					return ontoFactory.getOWLLiteral(Float.parseFloat(literal));
				}catch(NumberFormatException e){
					throw new OntoException("The following value is not a float:"+literal);
				}
			} else if(dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()).equals("double")){
				try{
					return ontoFactory.getOWLLiteral(Double.parseDouble(literal));
				}catch(NumberFormatException e){
					throw new OntoException("The following value is not a double:"+literal);
				}
			}
		}
		throw new OntoException("The following data type could not be recognised:"+dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()));
	}
	
	/**
	 * Adds the CTML comment or a material and its comment to the OWL ontology
	 * being created
	 * 
	 * @param comment
	 */
	public void createMechanismComment(String comment) {
		if (ctmlCommentParseStatus.isMaterial()) {
			try{
				addMaterial(basePath, ctmlComment.getMaterial());
				addMaterialComment(basePath, ctmlComment.getMaterial(), comment);
			}catch(OntoException e){
				logger.error("The following material could not be created:"+ctmlComment.getMaterial());
			}
		} else {
			try {
				addDataProperty(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), comment, appConfigOntokin.getOntokinMechanism()
						.concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)));
			} catch (OntoException e) {
				logger.error("The comment about a mechanism could not be created.");
			}
		}
		ctmlCommentParseStatus.setComment(false);
	}

	/**
	 * Adds a mechanism and its ontological relation with the CTML Knowledge
	 * Representation Language.
	 * 
	 * @throws OntoException
	 */
	public void createMechanism() throws OntoException {
		mechanismInstanceId++;
		// Creates the OWL class Representation Language
		OWLClass kr = createOWLClass(dataFactory, basePathTBox,
				appConfigOntokin.getRepresentationLanguage());
		// Creates the OWL class Mechanism
		OWLClass mechanism = createOWLClass(dataFactory, basePathTBox,
				appConfigOntokin.getOntokinMechanism());
		// Creates the extracted from object property
		OWLObjectProperty extractedFrom = dataFactory
				.getOWLObjectProperty(basePathTBox.concat("#").concat(appConfigOntokin.getExtractedFrom()));
		// Creates the CTML instance
		OWLIndividual ctml = createOWLIndividual(dataFactory, basePathABox, appConfigOntokin.getCtml());
		// Creates the mechanism instance
		OWLIndividual processedMechanism = createOWLIndividual(dataFactory, basePath, 
				appConfigOntokin.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)));
		// Adds to the ontology the fact that the relation
		// extractedFrom
		// has a domain which is Mechanism
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyDomainAxiom(extractedFrom, mechanism)));
		// Adds to the ontology the fact that the relation
		// extractedFrom
		// has a range which is Representation Language
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLObjectPropertyRangeAxiom(extractedFrom, kr)));
		// Adds to the ontology the fact that CTML is a
		// Knowledge Representation Language
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(kr, ctml)));
		// Adds to the ontology the fact that a specific
		// mechanism is a Mechanism
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(mechanism, processedMechanism)));
		// Adds to the onotlogy the fact that a specific
		// mechanism has been extracted from CTML
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(extractedFrom, processedMechanism, ctml)));
	}

	
	/**
	 * Adds an instance of phase to the OWL representation being created. 
	 * 
	 * @param phaseDimension
	 * @throws OntoException
	 */
	public void addPhase(String basePath) throws OntoException {
		detectTypeProperty();
		writePhaseInstance();
		writeAllPhaseProperties();
	}
	
	/**
	 * A method that forwards calls to the methods that perform the 
	 * following functions:
	 * 1. Writes phase metadata;
	 * 2. Writes data arrays; and
	 * 3. Writes data models.
	 * 
	 * @throws OntoException
	 */
	private void writeAllPhaseProperties() throws OntoException{
		writePhaseMechanismRelation();
		writePhaseMetadata();
		addPhaseComment();
		writeDataArrays();
		if(phaseState.getValue()!=null && !phaseState.getValue().isEmpty()){
			writePhaseState();
		}
		writeDataModels();
	}

	/**
	 * A method that forwards calls to the methods that perform the</br>
	 * following functions:</br>
	 * 1. Writes a thermo model;</br>
	 * 2. Writes a site density;</br>
	 * 3. Writes a kinetics model; and</br>
	 * 4. Writes a transport model.  
	 * 
	 * @throws OntoException
	 */
	private void writeDataModels() throws OntoException {
		writeThermoModel();
		writeSiteDensityData();
		writeSiteDensityUnits();
		writeKineticsModel();
		writeTransportModel();
	}
	
	/**
	 * A method that forwards calls to the methods that perform the</br>
	 * following functions:</br>
	 * 1. Writes data arrays; and</br>
	 * 2. Writes data sources of some of the data arrays. 
	 * 
	 * @throws OntoException
	 */
	private void writeDataArrays() throws OntoException {
		writeElementArrayData();
		writeElementDataSource();
		createSpeciesArrayInOntology();
		writeSpeciesDataSource();
		if(reactionArray.getDatasrc()!=null && !reactionArray.getDatasrc().isEmpty()){
			writeReactionDataSource();
		}
		writePhaseArrayData();
	}
	
	/**
	 * A method that forwards calls to the methods that perform the</br>
	 * following functions:</br>
	 * 1. Writes the id of phase;</br>
	 * 2. Writes the dimension of a phase; and</br>
	 * 3. Writes the material of a phase. 
	 * 
	 * @throws OntoException
	 */
	private void writePhaseMetadata() throws OntoException {
		writePhaseId();
		writePhaseDimension();
		writePhaseMaterial();
	}

	/**
	 * Writes the state of a phase.
	 * 
	 */
	private void writePhaseState() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getPhaseState(), 
					phaseState.getValue(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
		} catch (OntoException e) {
			logger.error("The state of a phase could not be created.");
		}
	}
	
	/**
	 * Writes the names of the phases, of which species can participate in 
	 * reactions with species from the phase currently being parsed. 
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writePhaseArrayData() {
		try {
			if(phaseMD.getPhaseArray()!=null){
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getPhaseArray(), 
					phaseMD.getPhaseArray(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("Phase array data could not be created.");
		}
	}
	
	/**
	 * Writes the transport model belonging to a phase.
	 */
	private void writeTransportModel() {
		try {
			if (transportProperty != null && transportProperty.getModel() != null) {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getTransportModel(), 
					transportProperty.getModel(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("The following transport property model could not be created:"
					+ transportProperty.getModel());
		}
	}
	
	/**
	 * Writes the kinetics model belonging to a phase.
	 */
	private void writeKineticsModel() {
		try {
			if (kinetics != null && kinetics.getModel() != null) {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getKineticsModel(), 
					 kinetics.getModel(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("The following kinetics model could not be created:"
					+ kinetics.getModel());
		}
	}

	/**
	 * Writes the units of the site density in a phase
	 */
	private void writeSiteDensityUnits() {
		try {
			if (siteDensity != null && siteDensity.getUnits() != null) {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getSiteDensityUnits(), 
					siteDensity.getUnits(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("The site density units of a phase could not be created.");
		}
	}
	
	/**
	 * Writes the value of the site density in a phase.
	 */
	private void writeSiteDensityData() {
		try {
			if (siteDensity != null && siteDensity.getValue() != null) {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getSiteDensity(),
						siteDensity.getValue(),
						appConfigOntokin.getOntokinPhase().concat(UNDERSCORE)
						.concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("The site density of a phase could not be created.");
		}
	}
	
	/**
	 * Writes the thermo propety model belonging to a phase.
	 * 
	 */
	private void writeThermoModel() {
		try {
			if (thermoProperty != null && thermoProperty.getModel() != null) {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getThermoModel(), 
					thermoProperty.getModel(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("The following thermodynamic property could not be created:"
					+ thermoProperty.getModel());
		}
	}

	/**
	 * Writes that a phase is contained in a chemical kinetic reaction
	 * mechanism.
	 * 
	 */
	private void writePhaseMechanismRelation() {
		try {
			if(CtmlConverterUtils.getPhase()!=null && CtmlConverterUtils.getPhase().equals(appConfigOntokin.getClassGasPhase())){
			addObjectProperty(basePath, appConfigOntokin.getObjectPropertyContainedIn(),
					phaseType, appConfigOntokin.getOntokinMechanism(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)),
					appConfigOntokin.getOntokinMechanism().concat(UNDERSCORE)
							.concat(Long.toString(mechanismInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("The contained in relation between the gas phase and a mechanism could not be created.");
		}
	}

	/**
	 * Reads the data source of the reactions belonging to a
	 * phase
	 * 
	 * @throws OntoException
	 */
	private void writeReactionDataSource() {
		try {
			// While parsing reaction array data source within a phase, it creates 
			// a record in the HashMap called reactionArrayVsSpeciesArrayMap 
			// to use it later at reaction blocks to establish links 
			// between any coverage species/reactants/products and phases
			// they belong to.
			CtmlConverterUtils.addSpeciesArrayReactionArrayMap(reactionArrayVsSpeciesArrayMap);
			// While parsing phase block, this map is created to extract the
			// material name that corresponds to a material reaction array
			// data source.
			if(phaseMD.getMaterial()!=null){
				CtmlConverterUtils.addReactionArrayMaterialMap(reactionArrayVsMaterialMap);
			}
			// Holds the gas phase species and reaction data sources as the keys and
			// the corresponding phase instance id as their value separately. 
			CtmlConverterUtils.addGasPhaseDataSrcVsInstanceMap(gasPhaseDataSrcVsInstanceMap);
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getReactionDataSource(), 
					reactionArray.getDatasrc(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
		} catch (OntoException e) {
			logger.error("The following reaction array data source could not be created:"
					+ reactionArray.getDatasrc());
		}
	}

	
	/**
	 * Extracts and assigns a more specific phase (e.g. Gas Phase or 
	 * Bulk Phase) to the global variable phaseType.
	 * 
	 * Following the assignment of the specific phase, it forwards the
	 * call to the method that creates an instance of Phase. 
	 */
	private void detectTypeProperty() {
		try {
			if (!CtmlConverterState.createdPhase) {
				phaseType = CtmlConverterUtils.getPhase();
				CtmlConverterState.createdPhase = true;
			}
		} catch (OntoException e) {
			logger.error("Phase type could not be identified.");
		}
	}

	/**
	 * Creates an instance of phase in the mechanism OWL ontology being generated.
	 */
	private void writePhaseInstance() {
		phaseInstanceId++;
		try {
			addInstanceType(basePath, phaseType,
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)));
			// Adds the source comment (if available) at the time of creating
			// a phase instance.
			if(phaseMD.getSourceComment()!=null){
				addDataProperty(basePath, HASH, appConfigOntokin.getSourceComment(), phaseMD.getSourceComment(),
						appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error(
					"An OWL instance for a phase could not be created.");
		}
	}

	
	/**
	 * Creates the id of a phase.
	 */
	private void writePhaseId(){
		try {
			if (phaseMD.getId() != null) {
				iOwlConstructWriter.addDataProperty(DublinCoreVocabulary
						.IDENTIFIER.getIRI(), phaseMD.getId(),
						appConfigOntokin.getOntokinPhase().concat(UNDERSCORE)
						.concat(Long.toString(phaseInstanceId)));
			}
		} catch (OntoException e) {
			logger.error("Phase id could not be created.");
		}
	}

	/**
	 * Writes the dimension of a phase
	 */
	private void writePhaseDimension() {
		try {
			iOwlConstructWriter.addPhaseDimension(basePath, phaseMD.getDimension());
		} catch (OntoException e) {
			logger.error("The phase dimension metadata could not be created.");
		}
	}

	/**
	 * Writes the material of a phase
	 */
	private void writePhaseMaterial() {
		try {
			if(phaseMD.getMaterial()!=null){
				iOwlConstructWriter.addPhaseMaterial(basePath, phaseMD.getMaterial());
			}
		} catch (OntoException e) {
			logger.error(
					"The following phase material data could not be created:" + phaseMD.getMaterial());
		}
	}

	/**
	 * Reads elements from an element array
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeElementArrayData() {
		try {
			iOwlConstructWriter.addElementArray(basePath, elementArray.getValue());
		} catch (OntoException e) {
			logger.error("The following element array data could not be created:" + elementArray);
		}
	}
	
	/**
	 * Reads the data source of the elements
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeElementDataSource() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getElementDataSource(),
					elementArray.getDatasrc(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
		} catch (OntoException e) {
			logger.error("The following element array data source could not be created:"
					+ elementArray.getDatasrc());
		}
	}
	
	/**
	 * Forwards the call to the method that creates species
	 * array in the ontology being created.</br>
	 * 
	 * Also calls the method that creates an in-memory
	 * species-phase key-value pair.
	 * 
	 * @param speciesArray
	 */
	private void createSpeciesArrayInOntology() {
		if (speciesArray.getValue() != null) {
			// Calls the method that creates the species array
			// data property to a phase
			creatSpeciesArrayDataProperty(speciesArray.getValue());
			// Forwards the call to the method that creates
			// species-phase pairs in a HashMap
			creatSpeciesPhasePairs(speciesArray.getValue());
		}
	}

	
	/**
	 * Writes the data source of a species
	 */
	private void writeSpeciesDataSource() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getSpeciesDataSource(), 
					speciesArray.getDatasrc(),
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
					concat(Long.toString(phaseInstanceId)));
		} catch (OntoException e) {
			logger.error("The following species array data source could not be created:"
					+ speciesArray.getDatasrc());
		}
	}
	
	/**
	 * Adds a comment about a phase.
	 * 
	 * @throws OntoException
	 */
	public void addPhaseComment() {
		if (phaseMD.getComment() != null) {
			try {
				addDataProperty(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), phaseMD.getComment(),
						appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)));
				phaseParseStatus.setPhaseComment(false);
			} catch (OntoException e) {
				logger.error("Phase comment could not be created.");
			}
		}
	}
	
	/**
	 * Adds a phase dimension to the OWL representation of 
	 * the mechanism
	 * 
	 * @param phaseDimension
	 * @throws OntoException
	 */
	public void addPhaseDimension(String basePath, String phaseDimension) throws OntoException {
		OWLDataProperty phaseDimensionProperty = createOWLDataProperty(dataFactory, EMPTY,
				appConfigOntokin.getPhaseDimension(), EMPTY);
		OWLLiteral literal = createOWLLiteral(dataFactory, appConfigOntokin.getPhaseDimension(), phaseDimension);
		OWLIndividual phaseIndividual = dataFactory.getOWLNamedIndividual(basePath.concat("#")
				.concat(appConfigOntokin.getOntokinPhase()).concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(phaseDimensionProperty, phaseIndividual, literal)));
	}
	
	/**
	 * Links a phase to the material in which it exists.</br>
	 * Also links the material to the mechanism.  
	 * 
	 * @param basePath
	 * @param phaseMaterial
	 */
	public void addPhaseMaterial(String basePath, String phaseMaterial) throws OntoException {
		if(!materialVsInstanceMap.containsKey(phaseMaterial)){
			addMaterial(basePath, phaseMaterial);
		}
		// Creates the exists in link between a phase and a material.
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyExistsIn(), phaseType,
				appConfigOntokin.getClassMaterial(),
				appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)),
				appConfigOntokin.getClassMaterial().concat(UNDERSCORE)
						.concat(CtmlConverterUtils.extractMaterialInstanceId(phaseMaterial)));
		// Creates the contained in link between the material and the mechanism.
		addObjectProperty(basePath, appConfigOntokin.getObjectPropertyContainedIn(),
				appConfigOntokin.getClassMaterial(), appConfigOntokin.getOntokinMechanism(),
				appConfigOntokin.getClassMaterial().concat(UNDERSCORE)
						.concat(CtmlConverterUtils.extractMaterialInstanceId(phaseMaterial)),
				appConfigOntokin.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)));
	}
	
	/**
	 * Adds an instance of Material to the OWL ontology being created.
	 * 
	 * @param basePath
	 * @param material
	 */
	public void addMaterial(String basePath, String material) throws OntoException {
		// Creates the material class if it is not already created.
		OWLClass materialClass = createOWLClass(dataFactory, basePathTBox, appConfigOntokin.getClassMaterial());
		// Creates an instance of material, if it is not already created.
		OWLIndividual materialIndividual = dataFactory
				.getOWLNamedIndividual(basePathABox.concat("#").concat(appConfigOntokin.getClassMaterial()
						.concat(UNDERSCORE).concat(CtmlConverterUtils.extractMaterialInstanceId(material))));
		// Adds to the ontology the fact that a specific material is a material.
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(materialClass, materialIndividual)));
		// Adds the name of a material
		addMaterialName(material);
	}
	
	/**
	 * Links a phase to the material in which it exists.
	 * 
	 * @param basePath
	 * @param material
	 * @param comment
	 */
	public void addMaterialComment(String basePath, String material, String comment) throws OntoException {
		addDataProperty(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), comment, appConfigOntokin.getClassMaterial()
				.concat(UNDERSCORE).concat(CtmlConverterUtils.extractMaterialInstanceId(material)));
	}
	
	/**
	 * Adds the name to the OWL representation of an element.
	 * </br>
	 * Also links the element to its meta data.
	 * 
	 * @param basePath
	 * @param name
	 * @throws OntoException
	 */
	public void addElementName(String basePath, String name) throws OntoException {
		// Links a chemical element object to the related element meta data 
		// property object by hasElementMetadata property.
		addObjectProperty(basePath, appConfigOntokin.getElementMetadataProperty(), appConfigOntokin.getOntokinElement(),
				appConfigOntokin.getElementMetadata(),
				appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(name), appConfigOntokin
						.getElementMetadata().concat(UNDERSCORE).concat(Long.toString(elementMetaDataInstanceId)));
		// Adds the name of an element 
		addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), name,
				appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(name));
		// Adds the source comment to the current element.
		if(elementDataElement.getSourceComment()!=null){
			addDataProperty(basePath, HASH, appConfigOntokin.getSourceComment(), elementDataElement.getSourceComment(), appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(name));
			elementDataElement.setSourceComment(null);
		}
	}

	/**
	 * Creates an object property with the domain and range specified.
	 * Applies the property to connect two objects.
	 * 
	 * @param comment
	 * @throws OntoException
	 */
	public OWLIndividual addObjectProperty(String basePath, String objectPropertyName, String domain, String range, String domainInstance, String rangeInstance) throws OntoException {
		// Creates the range OWL class for the object property being created.
		OWLClass rangeClass = createOWLClass(dataFactory, basePathTBox, range);
		// Creates the domain OWL class for the object property being created.
		OWLClass domainClass = createOWLClass(dataFactory, basePathTBox, domain);
		// Creates the object property
		OWLObjectProperty objectProperty;
		if(objectPropertyName!=null && (objectPropertyName.trim().startsWith("http://") || objectPropertyName.trim().startsWith("https://"))){
			objectProperty = dataFactory
					.getOWLObjectProperty(objectPropertyName.trim());			
		}else{
			objectProperty = dataFactory
				.getOWLObjectProperty(basePathTBox.concat("#").concat(objectPropertyName));
		}
		// Creates the range instance
		OWLIndividual rangeIndividual = createOWLIndividual(dataFactory, basePathABox, rangeInstance);
		// Creates the domain instance
		OWLIndividual domainIndividual = createOWLIndividual(dataFactory, basePathABox,
				domainInstance);
		// Adds the domain of the object property to the ontology being created
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, domainClass)));
		// Adds the range of the object property to the ontology being created
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty, rangeClass)));
		// Adds to the ontology the instance of the range class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(rangeClass, rangeIndividual)));
		// Adds to the ontology the instance of the domain class
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(domainClass, domainIndividual)));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainIndividual, rangeIndividual)));
		return domainIndividual;
		
	}
	
	/**
	 * Creates an object property with domain and range defined.
	 * </p>
	 * Returns the created property.
	 * </p> 
	 * @param basePath
	 * @param objectPropertyName
	 * @param domain
	 * @param range
	 * @return
	 * @throws OntoException
	 */
	public OWLObjectProperty createObjectProperty(String basePath, String objectPropertyName, String domain, String range) throws OntoException{
		// Creates the object property
		OWLObjectProperty objectProperty;
		if(objectPropertyName!=null && (objectPropertyName.trim().startsWith("http://") || objectPropertyName.trim().startsWith("https://"))){
			objectProperty = dataFactory
					.getOWLObjectProperty(objectPropertyName.trim());						
		}else{
			objectProperty = dataFactory
					.getOWLObjectProperty(basePathTBox.concat("#").concat(objectPropertyName));			
		}
		return objectProperty;
	}
	
	/**
	 * Given an object property (of type OWLObjectProperty), className and 
	 * instance id, it creates  
	 * 
	 * @param basePath
	 * @param objProperty
	 * @param className
	 * @param instance
	 * @throws OntoException
	 */
	public void assertObjectProperty(String basePath, OWLObjectProperty objProperty, String domainClassName,
			String domainInstance, String rangeClassName, String rangeInstance) throws OntoException {
		OWLIndividual domainIndividual = createOWLIndividual(dataFactory, basePathABox, domainInstance);
		OWLIndividual rangeIndividual = createOWLIndividual(dataFactory, basePathABox, rangeInstance);
		// Creates the domain OWL class to assign it as the type of domain
		// instance.
		OWLClass domainClass = createOWLClass(dataFactory, basePathTBox, domainClassName);
		// Creates the domain OWL class to assign it as the type of range
		// instance.
		OWLClass rangeClass = createOWLClass(dataFactory, basePathTBox, rangeClassName);
		// Adds to the ontology the instance of the domain class.
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(domainClass, domainIndividual)));
		// Adds to the ontology the instance of the range class.
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(rangeClass, rangeIndividual)));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objProperty, domainIndividual, rangeIndividual)));
	}
	
	/**
	 * Creates an object property with a domain that has already been 
	 * created and a range to be created newly.
	 * 
	 * @param comment
	 * @throws OntoException
	 */
	public OWLIndividual addObjectProperty(String basePath, String objectPropertyName, OWLIndividual domainIndividual, String range, String rangeInstance) throws OntoException {
		// Creates the range OWL class for the object property being created.
		OWLClass rangeClass = createOWLClass(dataFactory, basePathTBox, range);
		// Creates the object property
		OWLObjectProperty objectProperty;
		if(objectPropertyName!=null && (objectPropertyName.trim().startsWith("http://") || objectPropertyName.trim().startsWith("https://"))){
			objectProperty = dataFactory
					.getOWLObjectProperty(objectPropertyName);			
		}else{
			objectProperty = dataFactory
					.getOWLObjectProperty(basePathTBox.concat("#").concat(objectPropertyName));			
		}
		// Creates the range instance
		OWLIndividual rangeIndividual = createOWLIndividual(dataFactory, basePathABox, rangeInstance);
		// Adds the range of the object property to the ontology being created
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty, rangeClass)));
		// Adds to the ontology the instance of the range class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(rangeClass, rangeIndividual)));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainIndividual, rangeIndividual)));
		return domainIndividual;
		
	}

	/**
	 * Creates an object property with a range that has already been 
	 * created and a domain to be created newly.
	 * 
	 * @param comment
	 * @throws OntoException
	 */
	public OWLIndividual addObjectProperty(String basePath, String objectPropertyName, String domain, String domainInstance, OWLIndividual rangeIndividual) throws OntoException {
		// Creates the domain OWL class for the object property being created.
		OWLClass domainClass = createOWLClass(dataFactory, basePathTBox, domain);
		// Creates the object property
		OWLObjectProperty objectProperty;
		if(objectPropertyName!=null && (objectPropertyName.trim().startsWith("http://") || objectPropertyName.trim().startsWith("https://"))){
			objectProperty = dataFactory
					.getOWLObjectProperty(objectPropertyName);			
		}else{
			objectProperty = dataFactory
					.getOWLObjectProperty(basePathTBox.concat("#").concat(objectPropertyName));			
		}
		// Creates the domain instance
		OWLIndividual domainIndividual = createOWLIndividual(dataFactory, basePathABox, domainInstance);
		// Adds the domain of the object property to the ontology being created
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, domainClass)));
		// Adds to the ontology the instance of the domain class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(domainClass, domainIndividual)));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainIndividual, rangeIndividual)));
		return domainIndividual;
		
	}
	
	/**
	 * Creates a data property, adds the value to it and adds it to
	 * an individual.
	 * 
	 * @param basePath
	 * @param domainInstance
	 * @param dataPropertyIRI
	 * @param dataPropertyValue
	 */
	public void addDataPropertyToIndividual(String basePath, OWLIndividual individual, IRI dataPropertyIRI, String dataPropertyValue) throws OntoException{
		// Creates the value of the data property being created.
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyIRI.getIRIString(), dataPropertyValue);
		// Reads the data property.
		OWLDataProperty dataProperty = dataFactory
				.getOWLDataProperty(dataPropertyIRI);
		// Adds to the ontology the comment about a mechanism in CTML.
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal)));		
	}
	
	public void addElementDataProperty(String basePath, String id) throws OntoException {
		OWLIndividual individual = addObjectProperty(basePathABox, appConfigOntokin.getOntokinElement(),
				appConfigOntokin.getElementMetadataProperty(), appConfigOntokin.getElementMetadata(),
				appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(elementDataElement.getName()),
				appConfigOntokin.getElementMetadata().concat(UNDERSCORE).concat(Long.toString(elementMetaDataInstanceId)));
		addDataPropertyToIndividual(basePath, individual, DublinCoreVocabulary.IDENTIFIER.getIRI(), id);
	}
	
	public void addSpeciesName(String basePath, String name) throws OntoException {
		String speciesId = speciesUniqueIDMap.get(species.getName().concat(UNDERSCORE).concat(speciesData.getId()));
		addProperty(basePath, appConfigOntokin.getClassSpecies(), speciesId,
				OWLRDFVocabulary.RDFS_LABEL.getIRI(), name);
		addObjectProperty(basePath, appConfigOntokin.getSpeciesMetadataProperty(), appConfigOntokin.getClassSpecies(),
				appConfigOntokin.getSpeciesMetadata(), speciesId, appConfigOntokin.getSpeciesMetadata()
						.concat(UNDERSCORE).concat(Long.toString(speciesMetaDataInstanceId)));
		// Adds the source comment to the current species.
		if (species.getSourceComment() != null) {
			addDataProperty(basePath, HASH, appConfigOntokin.getSourceComment(), species.getSourceComment(), speciesId);
			species.setSourceComment(null);
		}
	}
	
	public void addSpeciesToPhase(String basePath, String keyToSpeciesPhaseMap) throws OntoException {
		if (!speciesPhaseMap.containsKey(keyToSpeciesPhaseMap)) {
			logger.error("A species was not found in the speciesPhase map.");
			throw new OntoException("A species was not found in the speciesPhase map.");
		}
		addObjectProperty(basePath, appConfigOntokin.getOntokinBelongsToPhase(), appConfigOntokin.getClassSpecies(),
				phaseType, speciesId, speciesPhaseMap.get(keyToSpeciesPhaseMap));
	}
	
	public void addNasaPCoeffsInOntology(String basePath, String coefficients) throws OntoException {
		createToAddNasaPCoeffsToOntology(null, coefficients);
		// Adds the more specific type (e.g. NASA) of a Thermo Model
		addInstanceType(basePath, appConfigOntokin.getOntokinNasaPolyCoefficient(), appConfigOntokin
				.getClassThermoModel().concat(UNDERSCORE).concat(Long.toString(nasaPolyCoeffsInstanceId)));
	}
	
	private void createToAddNasaPCoeffsToOntology(String key, String coefficients) throws OntoException {
		nasaPolyCoeffsInstanceId++;
		String range = appConfigOntokin.getClassThermoModel();
		try {
			OWLObjectProperty objectProperty = createObjectProperty(basePath,
					appConfigOntokin.getObjectPropertyHasThermoModel(), appConfigOntokin.getClassSpecies(),
					appConfigOntokin.getClassThermoModel());
			assertObjectProperty(basePath, objectProperty, appConfigOntokin.getClassSpecies(), speciesId,
					appConfigOntokin.getOntokinNasaPolyCoefficient(), range.concat(UNDERSCORE).concat(Long.toString(nasaPolyCoeffsInstanceId)));			
		} catch (OntoException e) {
			logger.error("NASA Polynomial Coefficients of a species could not be created.");
		}
		addAllMetadataToNASAPCoeffs(coefficients);
	}
	
	private void addAllMetadataToNASAPCoeffs(String coefficients){
		addCoefficientValue(coefficients);
		addTmaxToNASAPCoeffs(nasa.getTmax());
		addTminToNASAPCoeffs(nasa.getTmin());
		addP0ToNASAPCoeffs(nasa.getP0());
		addNameToNASAPCoeffsArray(coeffArray.getName());
		addSizeToNASAPCoeffsArray(coeffArray.getSize());
	}
	
	public void addChebyshevRateCoeffs(String basePath, String coefficients)  throws OntoException{
		addChebyshebCoeffsValue(basePath, coefficients);
		addChebyshebCoeffsName(basePath, rateCoeffArray.getName());
		addChebyshebCoeffsUnits(basePath, rateCoeffArray.getUnits());
		addChebyshebCoeffsdegreeT(basePath, rateCoeffArray.getDegreeT());
		addChebyshebCoeffsdegreeP(basePath, rateCoeffArray.getDegreeP());
	}

	private void addCoefficientValue(String coefficients){
		if (coefficients != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyHasCoeffValues(),
						coefficients, appConfigOntokin.getClassThermoModel(), nasaPolyCoeffsInstanceId);
			} catch (OntoException e) {
				logger.error("The NASA Polynomial Coefficients for a species could not be created..");
			}
		}
	}
	
	private void addTmaxToNASAPCoeffs(String Tmax){
		if (Tmax != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getOntokinNASACoefficientTmax(), Tmax,
						appConfigOntokin.getClassThermoModel(), nasaPolyCoeffsInstanceId);
			} catch (OntoException e) {
				logger.error(
						"The maximum temperature at which the NASA Polynomial Coefficients for a species is invalid could not be created.");
			}
		}
	}

	private void addTminToNASAPCoeffs(String Tmin){
		if (Tmin != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getOntokinNASACoefficientTmin(), Tmin,
						appConfigOntokin.getClassThermoModel(), nasaPolyCoeffsInstanceId);
			} catch (OntoException e) {
				logger.error(
						"The minimum temperature below which the NASA Polynomial Coefficients for a species is invalid could not be created.");
			}
		}
	}

	private void addP0ToNASAPCoeffs(String P0){
		if (P0 != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getOntokinNASACoefficientP0(), P0,
						appConfigOntokin.getClassThermoModel(), nasaPolyCoeffsInstanceId);
			} catch (OntoException e) {
				logger.error(
						"The pressure at which the NASA Polynomial Coefficients for a species is valid could not be created..");
			}
		}
	}

	private void addNameToNASAPCoeffsArray(String name){
		if (name != null) {
			try {
				addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), name,
						appConfigOntokin.getClassThermoModel().concat(UNDERSCORE)
								.concat(Long.toString(nasaPolyCoeffsInstanceId)));
			} catch (OntoException e) {
				logger.error("The name of the NASA Polynomial Coefficient array for a species could not be created.");
			}
		}
	}

	private void addSizeToNASAPCoeffsArray(String size) {
		if (size != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getOntokinHasNumberOfCoefficients(), size,
						appConfigOntokin.getClassThermoModel(), nasaPolyCoeffsInstanceId);
			} catch (OntoException e) {
				logger.error("The name of the NASA Polynomial Coefficient array for a species could not be created.");
			}
		}
	}

	private void addChebyshebCoeffsValue(String basePath, String coefficients) {
		if (coefficients != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyChebyshebRateCoeffsValue(),
						coefficients, appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The Chebysheb rate coefficients for a reaction could not be created..");
			}
		}
	}

	private void addChebyshebCoeffsName(String basePath, String name) {
		if (name != null) {
			try {
				addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), name, appConfigOntokin.getClassCHEBCoefficient()
						.concat(UNDERSCORE).concat(Long.toString(rateCoeffChebInstanceId)));
			} catch (OntoException e) {
				logger.error("The Chebysheb rate coefficients for a reaction could not be created..");
			}
		}
	}
	
	private void addChebyshebCoeffsUnits(String basePath, String units) {
		if (units != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyChebyshebRateCoeffsUnits(), units,
						appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The Chebysheb rate coefficients' units for a reaction could not be created..");
			}
		}
	}
	
	private void addChebyshebCoeffsdegreeT(String basePath, String degreeT) {
		if (degreeT != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyChebyshebRateCoeffsTempPoints(),
						degreeT, appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error(
						"The Chebysheb rate coefficients' temperature points for a reaction could not be created..");
			}
		}
	}
	
	private void addChebyshebCoeffsdegreeP(String basePath, String degreeP) {
		if (degreeP != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyChebyshebRateCoeffsPressurePoints(),
						degreeP, appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The Chebysheb rate coefficients' pressure points for a reaction could not be created..");
			}
		}
	}
	
	public void addTransportProperty() {
		try {
			OWLIndividual domainIndividual = dataFactory.getOWLNamedIndividual(basePath.concat(HASH).concat(speciesId));
			addObjectProperty(basePath, appConfigOntokin.getOntokinHasTransportParameter(), domainIndividual,
					appConfigOntokin.getOntokinTransportParameter(), appConfigOntokin.getOntokinTransportParameter()
							.concat(UNDERSCORE).concat(Long.toString(idTransportParameter)));
			createdTransportParameter = true;
		} catch (OntoException e) {
			logger.error("The hasTransportParameter object property of a species could not be created.");
		}
	}
	
	/**
	 * Creates the comment about an element in the ontology being created.
	 * 
	 * @param comment
	 */
	public void createElementComment(String comment) {
		if (comment != null) {
			try {
				addProperty(basePath,
						appConfigOntokin.getOntokinElement(), 
						appConfigOntokin.getOntokinElement().concat(UNDERSCORE)
								.concat(elementDataElement.getName()),
						OWLRDFVocabulary.RDFS_COMMENT.getIRI(), comment);
			} catch (OntoException e) {
				logger.error("Comment about an element could not be created.");
			}
		}
	}

	/**
	 * Creates the comment about the thermodynamic properties of a 
	 * a species in the ontology being created.
	 * 
	 * @param comment
	 */
	public void createThermoCommentInOntology(String comment) {
		if (comment != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getOntokinThermoComment(), comment, speciesId);
			} catch (OntoException e) {
				logger.error("Comment about the thermodynamic properties could not be created.");
			}
		}
		speciesThermoParseStatus.setThermoComment(false);
	}

	/**
	 * Creates the comment about a reaction in the ontology being created.
	 * 
	 * @param comment
	 */
	public void createReactionComment(String comment) {
		if (comment != null) {
			try {
				addProperty(basePath,
						reactionType, CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
								.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
								.concat(Long.toString(reactionSerialNo)),
						OWLRDFVocabulary.RDFS_COMMENT.getIRI(), comment);
			} catch (OntoException e) {
				logger.error("Comment about a reaction could not be created.");
			}
		}
		speciesThermoParseStatus.setThermoComment(false);
	}
	
	/**
	 * Forwards the call to the method that creates the comment about the
	 * transport parameters of a a species in the ontology being created.
	 * 
	 * @param comment
	 */
	public void createTransportCommentInOntology(String comment) {
		if (comment != null) {
			try {
				addDataProperty(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), comment, 
						appConfigOntokin.getOntokinTransportParameter()
						.concat(UNDERSCORE)
						.concat(Long.toString(idTransportParameter)));

			} catch (OntoException e) {
				logger.error("Comment about transport properties could not be created.");
			}
		}
		speciesTransportParseStatus.setComment(false);
	}
		
	/**
	 * Reads the comment attached to a species.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	public void readSpeciesComment(String comment) {
		if(comment!=null){
			try {
				addDataProperty(OWLRDFVocabulary.RDFS_COMMENT.getIRI(),
						comment, speciesId);
			} catch (OntoException e) {
				logger.error("Comment about a species could not be created.");
			}
		}
	}

	/**
	 * Writes the equation of a reaction.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	public void writeEquation(String equation) {
		if (equation != null) {
			try {
				addDataProperty(basePath, HASH, appConfigOntokin.getOntoKinEquation(), equation,
						CtmlConverterUtils.getClassName(appConfigOntokin.getClassReaction()).concat(UNDERSCORE)
								.concat(Long.toString(reactionInstanceId)).concat(UNDERSCORE)
								.concat(Long.toString(reactionSerialNo)));
			} catch (OntoException e) {
				logger.error("Equation of a reaction could not be created.");
			}
		}
	}
		
	/**
	 * Adds the species array data property to a phase.
	 * 
	 * @param speciesArrayData
	 */
	public void creatSpeciesArrayDataProperty(String speciesArrayData) {
		if (speciesArrayData != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, 
						appConfigOntokin.getPhaseSpeciesArray(),
						speciesArrayData,
						appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).
						concat(Long.toString(phaseInstanceId)));
			} catch (OntoException e) {
				logger.error("The following species array could not be created:" + speciesArrayData);
			}
		}
	}

	/**
	 * Creates species-phase id pairs in a hashmap.
	 * 
	 * @param speciesArrayData
	 */
	public void creatSpeciesPhasePairs(String speciesArrayData) {
		try {
			String normalisedSpecies = CtmlConverterUtils.normaiseMultiLineData(speciesArrayData);
			if (speciesArray.getDatasrc() == null) {
				logger.error("The data source of a " + "SpeciesArray is empty.");
			}
			String speciesArrayDataSrc = speciesArray.getDatasrc();
			speciesArrayDataSrc = CtmlConverterUtils.removeSpecificString(speciesArrayDataSrc, HASH, EMPTY);
			CtmlConverterUtils.createKeyValuePairs(speciesPhaseMap, speciesVsPhaseClassMap, normalisedSpecies,
					appConfigOntokin.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)),
					UNDERSCORE.concat(speciesArrayDataSrc));
		} catch (OntoException e) {
			logger.error("Species-phase pairs could not be created.");
		}
	}
	
	/**
	 * Forwards the call to the method that creates the NASA
	 * Polynomial coefficients of a species in the
	 * ontology being created.
	 * 
	 * @param nasaPCoeffs
	 */
	public void createNasaPCoeffsInOntology(String nasaPCoeffs) {
		if (nasaPCoeffs != null) {
			try {
				addNasaPCoeffsInOntology(basePath, nasaPCoeffs);
			} catch (OntoException e) {
				logger.error("NASA Polynomial Coefficients of a species could not be created.");
			}
		}
		coeffArrayParseStatus.setFloatArray(false);
	}
	
	/**
	 * Forwards the call to the method that creates the
	 * Chebyshev Rate Coefficients of a reaction species in the
	 * ontology being created.
	 * 
	 * @param nasaPCoeffs
	 */
	public void createChebyshevRateCoeffs(String chebRateCoeffs) {
		if (chebRateCoeffs != null) {
			try {
				addChebyshevRateCoeffs(basePath, chebRateCoeffs);
			} catch (OntoException e) {
				logger.error("Chebyshev Rate Coefficients for " + "approximating the rate constant of "
						+ "a reaction could not be created.");
			}
		}
		coeffArrayParseStatus.setFloatArray(false);
	}
	
	/**
	 * Saves an ontology created for codifying a chemical mechanism.
	 */
	public void saveOntology() throws OWLOntologyStorageException {
		try {
			// Adds the import clause to the OntoChem TBox
			OWLImportsDeclaration importDeclaration=OWLManager.getOWLDataFactory().getOWLImportsDeclaration(IRI.create(ontoChemKB.getOntoKinKbTBoxIri()));
			manager.applyChange(new AddImport(ontology, importDeclaration));
			// Adds commit hash to the ontology, if it can be retrieved. 
			representCommitHash();
			// Saves the ontology in the file system
			manager.saveOntology(ontology, ontologyIRIFileSave);
			logger.info("Started removing TBox from the mechanism OWL file.");
			removeTBox(ontologyIRIFileSave);
			logger.info("Finished removing TBox from the mechanism OWL file.");
		} catch (OWLOntologyStorageException e1) {
			logger.error("The ontology could not be saved.");
			throw new OWLOntologyStorageException("The ontology could not be saved.");
		}
	}
	
	/**
	 * Represents the current commit hash using OWL.
	 * 
	 * @throws OntoException
	 */
	private void representCommitHash(){
		String commitHash = null;
		try{
			commitHash = CtmlConverterUtils.gitCommitHash();
		}catch(OntoException e){}
		if (commitHash != null && !commitHash.isEmpty()) {
			OWLLiteral label = dataFactory.getOWLLiteral(commitHash);
			OWLAnnotationProperty commit = dataFactory.getOWLAnnotationProperty(IRI.create(ontoChemKB
					.getOntoKinKbTBoxIri().concat("#").concat(appConfigOntokin.getCompChemGitCommitHash())));
			OWLAnnotation commitAttribute = dataFactory.getOWLAnnotation(commit, label);
			manager.applyChange(new AddOntologyAnnotation(ontology, commitAttribute));
		}
	}
	
	/**
	 * Removes TBox(es) from the generated mechanism ontology  
	 * 
	 * @param ontologyIRIFileSave
	 */
	public void removeTBox(IRI ontologyIRIFileSave){
		try{
			String ontologyFileSaveIRI = ontologyIRIFileSave.toString();
			ontologyFileSaveIRI = ontologyFileSaveIRI.replace("file:/", "");
			ontologyFileSaveIRI = getFilePath(ontologyFileSaveIRI);
			String owlFileNameTemp = ontologyFileSaveIRI.replace(".owl", "_temp.owl");
			String owlFileNameOriginal = ontologyFileSaveIRI;
			BufferedReader br = CtmlConverterUtils.openSourceFile(owlFileNameOriginal);
			BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(owlFileNameTemp), "UTF-8"));
			String line;
			while ((line = br.readLine()) != null) {
				if ((line.trim().startsWith("<!--") && line.endsWith("-->")) || ((line.contains("<owl:DatatypeProperty")
						|| line.contains("<owl:ObjectProperty") || line.contains("</owl:ObjectProperty")
						|| line.contains("<owl:Class") || line.contains("<rdfs:domain") || line.contains("<rdfs:range")
						|| line.trim().isEmpty()))) {
				} else {
					bw.write(line.concat("\n"));
				}
			}
			bw.close();
			br.close();
			delete(owlFileNameOriginal, owlFileNameTemp);
		}catch(IOException e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Returns a valid file path.
	 * 
	 * @param filePath
	 * @return
	 */
	private String getFilePath(String filePath){
		return filePath.replaceAll("%20", " ");
	}
	
	/**
	 * Deletes the original file and renames the temporary file to the 
	 * original file name. 
	 * 
	 * @param owlFileNameOriginal
	 * @param owlFileNameTemp
	 */
	private void delete(String owlFileNameOriginal, String owlFileNameTemp){
		File fileOriginal = new File(owlFileNameOriginal);
		if(fileOriginal.delete()){
			fileOriginal = new File(owlFileNameOriginal);
			File fileTemp = new File(owlFileNameTemp);
			if(fileTemp.renameTo(fileOriginal)){
			}else{
				logger.error("A mechanism OWL ontology file could not be renamed.");
			}
		}else{
			logger.error("A mechanism OWL ontology file could not be deleted.");
		}
	}
	
	/**
	 * Creates a list of references.
	 * 
	 * @param references a list of references
	 */
	public void createReference(List<Reference> references) throws OntoException {
		if (references != null) {
			for (Reference reference : references) {
				if (reference.getContributor() != null) {
					List<Creator> creators = reference.getContributor();
					for (Creator creator : creators) {
						createCreator(creator, appConfigOntokin.getClassPerson());
					}
				}
				if (reference.getPublicationSpecification() != null) {
					createPublicationSpecificiation(reference.getPublicationSpecification(), appConfigOntokin.getClassJournalSpecification());
				}
				referenceInstanceId++;
				journalSpecInstanceId++;
				journalInstanceId++;
			}
		}
	}
	
	/**
	 * Creates a creator of the current reaction mechanism. It also creates</br>
	 * the data and meta data of the creator.  
	 * 
	 * @param creator
	 */
	private void createCreator(Creator creator, String type) throws OntoException {
		// Adds the creators to a reaction mechanism
		addCreator(creator, type);
		// Adds the contributors to a reference
		addContributor(creator, appConfigOntokin.getClassPerson());
		if (type.equalsIgnoreCase(appConfigOntokin.getClassPerson())) {
			addPersonInfo(creator);
		}
		mechanismCreatorInstanceId++;
	}
	
	/**
	 * Adds a creator to the current reaction mechanism. 
	 * 
	 * @param creator
	 * @param type
	 * @throws OntoException
	 */
	private void addCreator(Creator creator, String type) throws OntoException{
		OWLObjectProperty objectPropertyCreator = createObjectProperty(basePath,
				appConfigOntokin.getObjectPropertyHasCreator(), appConfigOntokin.getOntokinMechanism(),
				appConfigOntokin.getClassAgent());
		assertObjectProperty(basePath, objectPropertyCreator, appConfigOntokin.getOntokinMechanism(),
				appConfigOntokin.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)),
				type, appConfigOntokin.getClassAgent().concat(UNDERSCORE)
						.concat(Long.toString(mechanismCreatorInstanceId)));
		assertObjectProperty(basePath, objectPropertyCreator, appConfigOntokin.getOntokinMechanism(),
				appConfigOntokin.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)),
				type, appConfigOntokin.getClassAgent().concat(UNDERSCORE)
						.concat(Long.toString(mechanismCreatorInstanceId)));
	}

	/**
	 * Adds a contributor to the current reference. 
	 * 
	 * @param creator
	 * @param type
	 * @throws OntoException
	 */
	private void addContributor(Creator creator, String type) throws OntoException{
		OWLObjectProperty objectPropertyContributor = createObjectProperty(basePath,
				appConfigOntokin.getObjectPropertyHasContributor(), appConfigOntokin.getClassReference(),
				appConfigOntokin.getClassAgent());
		assertObjectProperty(basePath, objectPropertyContributor, appConfigOntokin.getClassReference(),
				appConfigOntokin.getClassReference().concat(UNDERSCORE).concat(Long.toString(referenceInstanceId)),
				type, appConfigOntokin.getClassAgent().concat(UNDERSCORE)
						.concat(Long.toString(mechanismCreatorInstanceId)));
		assertObjectProperty(basePath, objectPropertyContributor, appConfigOntokin.getClassReference(),
				appConfigOntokin.getClassReference().concat(UNDERSCORE).concat(Long.toString(referenceInstanceId)),
				type, appConfigOntokin.getClassAgent().concat(UNDERSCORE)
						.concat(Long.toString(mechanismCreatorInstanceId)));
	}

	/**
	 * Adds the following information to the person being created:</br>
	 * 1. Name.</br>
	 * 2. Given name.</br>
	 * 3. Family name.</br>
	 * 
	 * @param creator one of the creators of a mechanism. 
	 * @throws OntoException
	 */
	private void addPersonInfo(Creator creator) throws OntoException {
		// Adds the family name of one of the persons created the current
		// mechanism
		addDataProperty(IRI.create(appConfigOntokin.getDataPropertyFamilyName()), creator.getFamilyName(),
				appConfigOntokin.getClassAgent().concat(UNDERSCORE).concat(Long.toString(mechanismCreatorInstanceId)));
		// Adds the given name of one of the persons created the current
		// mechanism
		addDataProperty(IRI.create(appConfigOntokin.getDataPropertyGivenName()), creator.getGivenName(),
				appConfigOntokin.getClassAgent().concat(UNDERSCORE).concat(Long.toString(mechanismCreatorInstanceId)));
		// Adds the name of one of the persons created the current mechanism
		addDataProperty(IRI.create(appConfigOntokin.getDataPropertyName()), creator.getName(),
				appConfigOntokin.getClassAgent().concat(UNDERSCORE).concat(Long.toString(mechanismCreatorInstanceId)));
	}

	/**
	 * Creates a publication specification which includes the following:</br>
	 * 1. A link to an actual publication, e.g. Journal or Proceedings.</br>
	 * 2. Publication info including data and meta data.
	 * @param publicationSpecification
	 */
	private void createPublicationSpecificiation(PublicationSpecification publicationSpecification, String type)
			throws OntoException {
		OWLObjectProperty objectPropertyPubSpec = createObjectProperty(basePath,
				appConfigOntokin.getObjectPropertyHasPublicationSpecification(), appConfigOntokin.getClassReference(),
				appConfigOntokin.getClassPublicationSpecification());
		assertObjectProperty(basePath, objectPropertyPubSpec, appConfigOntokin.getClassReference(),
				appConfigOntokin.getClassReference().concat(UNDERSCORE).concat(Long.toString(referenceInstanceId)),
				type, appConfigOntokin.getClassJournalSpecification().concat(UNDERSCORE)
						.concat(Long.toString(journalSpecInstanceId)));
		addPublicationInfo(publicationSpecification, type);
	}
	
	/**
	 * Creates data and meta data of a publication. 
	 * 
	 * @param publicationSpecification
	 * @param type
	 * @throws OntoException
	 */
	private void addPublicationInfo(PublicationSpecification publicationSpecification, String type) throws OntoException{
		JournalSpecification journalSpec = (JournalSpecification) publicationSpecification;
		if(journalSpec!=null && type!=null && type.equalsIgnoreCase(appConfigOntokin.getClassJournalSpecification())){
			// Adds the volume of the journal in which the current reference has been published
			addDataProperty(IRI.create(appConfigOntokin.getDataPropertyJournalVolume()), Integer.toString(journalSpec.getVolume()), appConfigOntokin.getDataPropertyJournalVolume(),
					appConfigOntokin.getClassJournalSpecification().concat(UNDERSCORE).concat(Long.toString(journalSpecInstanceId)));
			// Adds the page start of the reference in the journal  
			addDataProperty(IRI.create(appConfigOntokin.getDataPropertyDocumentPageStart()), Integer.toString(journalSpec.getPageStart()), appConfigOntokin.getDataPropertyDocumentPageStart(),
					appConfigOntokin.getClassJournalSpecification().concat(UNDERSCORE).concat(Long.toString(journalSpecInstanceId)));			
			// Adds the page end of the reference in the journal
			addDataProperty(IRI.create(appConfigOntokin.getDataPropertyDocumentPageEnd()), Integer.toString(journalSpec.getPageEnd()), appConfigOntokin.getDataPropertyDocumentPageEnd(),
					appConfigOntokin.getClassJournalSpecification().concat(UNDERSCORE).concat(Long.toString(journalSpecInstanceId)));
			addJournalInfo(journalSpec, type);
		}
	}
	
	/**
	 * Adds journal title and issn to the OWL representation.
	 * 
	 * @param journalSpec
	 * @param type
	 * @throws OntoException
	 */
	private void addJournalInfo(JournalSpecification journalSpec, String type) throws OntoException{
		OWLObjectProperty objectPropertyHasJournal = createObjectProperty(basePath,
				appConfigOntokin.getObjectPropertyHasJournal(), type,
				appConfigOntokin.getClassJournal());
		assertObjectProperty(basePath, objectPropertyHasJournal, type,
				type.concat(UNDERSCORE).concat(Long.toString(journalSpecInstanceId)),
				appConfigOntokin.getClassJournal(), appConfigOntokin.getClassJournal().concat(UNDERSCORE)
						.concat(Long.toString(journalInstanceId)));
		// Adds the title of the journal  
		addDataProperty(IRI.create(appConfigOntokin.getDataPropertyTitle()), journalSpec.getJournal().getTitle(), appConfigOntokin.getDataPropertyTitle(),
				appConfigOntokin.getClassJournal().concat(UNDERSCORE).concat(Long.toString(journalInstanceId)));			
		// Adds the issn of the journal  
		addDataProperty(IRI.create(appConfigOntokin.getDataPropertyIssn()), journalSpec.getJournal().getiSSN(), appConfigOntokin.getDataPropertyIssn(),
				appConfigOntokin.getClassJournal().concat(UNDERSCORE).concat(Long.toString(journalInstanceId)));
	}
}