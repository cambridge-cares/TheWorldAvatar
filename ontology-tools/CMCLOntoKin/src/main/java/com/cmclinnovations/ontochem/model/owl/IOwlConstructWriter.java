package com.cmclinnovations.ontochem.model.owl;

import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.reference.data.structure.Reference;

public interface IOwlConstructWriter {
	public void addProperty(String basePath, String clasName, String instance, IRI dataPropertyIRI, String dataPropertyValue) throws OntoException;
	public void addProperty(String basePath, String className, String instance, String dataPropertyName, String dataPropertyValue) throws OntoException;
	public void addInstanceType(String basePath, String clasName, String instance) throws OntoException;
	public void addElementId(String basePath, String id) throws OntoException;
	public void addElementArray(String basePath, String elementArray) throws OntoException;
	public void addSpeciesId(String basePath, String id) throws OntoException;
	public void addReactionDataId(String basePath, String id) throws OntoException;
	public void addReactionId(String basePath, String id) throws OntoException;
	public void addReaction(String basePath) throws OntoException;
	public void addArrheniusCoefficient(String basePath) throws OntoException;
	public void addStickingCoefficient(String basePath) throws OntoException;
	public void addLandauTellerCoefficient(String basePath) throws OntoException;
	public void addEfficiencyInstance(String basePath) throws OntoException;
	public void addFallOffInstance(String basePath) throws OntoException;
	public void addFallOffType(String basePath) throws OntoException;
	public void addSpeciesEfficiency(String species, String efficiency)
			throws OntoException;
	public void addNamedThirdBodySpecies(String species) throws OntoException;
	public void addCoverageSpecies(String basePath) throws OntoException;
	public void addDefaultEfficiency(String defaultEfficiency) throws OntoException;
	public void addCovDependencyCoefficient(String basePath) throws OntoException;
	public void addChebCoefficient(String basePath) throws OntoException;
	public void addArrheniusCoeffName(String name, String coefficientType, Long instanceId) throws OntoException;
	public void addReactionOrder(String basePath) throws OntoException;
	public void addReactionOrderSpecies(String basePath, String direction, String species) throws OntoException;
	public void addDuplicateInfo(String basePath, String duplicate) throws OntoException;
	public void addReversibleInfo(String basePath, String reversible) throws OntoException;
	public void addDataProperty(String iri, String pathSeparator, String property, String propertyValue, String individialName) throws OntoException;
	public void addDataProperty(IRI iri, String propertyValue, String individialName) throws OntoException;
	public void addDataProperty(IRI iri, String propertyValue, String concept, long instanceId) throws OntoException;
	public void addDataProperty(String iri, String pathSeparator, String property, String propertyValue, String concept, long instanceId) throws OntoException;
	public void addPhase(String basePath) throws OntoException;
	public void addPhaseDimension(String basePath, String phaseDimension) throws OntoException;
	public void addPhaseMaterial(String basePath, String phaseMaterial) throws OntoException;
	public void addElementName(String basePath, String name) throws OntoException;
	public OWLIndividual addObjectProperty(String basePath, String objectPropertyName, String domain, String range, String domainInstance, String rangeInstance) throws OntoException;
	public OWLIndividual addObjectProperty(String basePath, String objectPropertyName, OWLIndividual domainIndividual, String range, String rangeInstance) throws OntoException;
	public void addDataPropertyToIndividual(String basePath, OWLIndividual individual, IRI dataPropertyIRI, String dataPropertyValue) throws OntoException;
	public void addElementDataProperty(String basePath, String id) throws OntoException;
	public void addSpeciesName(String basePath, String name) throws OntoException;
	public void addSpeciesToPhase(String basePath, String keyToSpeciesPhaseMap) throws OntoException;
	public void addAtomArray(String basePath, String atomArray) throws OntoException;
	public void addProducts(String basePath, String products) throws OntoException;
	public void addReactants(String basePath, String products) throws OntoException;
	public void addNasaPCoeffsInOntology(String basePath, String coefficients) throws OntoException;
	public void addChebyshevRateCoeffs(String basePath, String coefficients)  throws OntoException;
	public void addTransportProperty();
	public void createThermoCommentInOntology(String comment);
	public void createElementComment(String comment);
	public void createReactionComment(String comment);
	public void createTransportCommentInOntology(String comment);
	public void createMechanismComment(String comment);
	public void createMechanism() throws OntoException;
	public void readSpeciesComment(String comment);
	public void writeEquation(String equation);
	public void creatSpeciesArrayDataProperty(String speciesArrayData);
	public void creatSpeciesPhasePairs(String speciesArrayData);
	public void createNasaPCoeffsInOntology(String nasaPCoeffs);
	public void createChebyshevRateCoeffs(String chebRateCoeffs);
	public void saveOntology() throws OWLOntologyStorageException;
	public void createReference(List<Reference> reference) throws OntoException;
	public void removeTBox(IRI ontologyIRIFileSave);
}
