package org.cam.ceb.como.nist.converter;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.Date;
import java.time.LocalDate;

import org.cam.ceb.como.nist.model.exception.OntoSpeciesException;
import org.cam.ceb.como.nist.model.utils.NISTConverterUtils;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;

public class NISTOWLWriter extends NISTConverter implements INISTOWLWriter{
	private Logger logger = org.slf4j.LoggerFactory.getLogger(NISTOWLWriter.class);
	
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
	public void addDataProperty(IRI iri, String propertyValue, String individialName) throws OntoSpeciesException {
		// Reads the data property from the OWL API data factory
		OWLDataProperty identifierProperty = dataFactory.getOWLDataProperty(iri);
		addDataProperty(identifierProperty, propertyValue, individialName);
	}

	/**
	 * Attaches a data property value to an instance.
	 * 
	 * @param identifierProperty
	 * @param propertyValue
	 * @param individialName
	 * @throws OntoException
	 */
	private void addDataProperty(OWLDataProperty identifierProperty, String propertyValue, String individialName) throws OntoSpeciesException {
		OWLLiteral literal = createOWLLiteral(dataFactory, identifierProperty.toStringID(), propertyValue);
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, individialName);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(identifierProperty, individual, literal)));
	}

	
	/**
	 * Creates a data property, adds the value to it and adds it to
	 * an individual.
	 * 
	 * @param individual
	 * @param dataPropertyName
	 * @param propertyPathSeparator
	 * @param dataPropertyValue
	 * @throws OntoSpeciesException
	 */
	public void addDataPropertyToIndividual(OWLIndividual individual, String dataPropertyName, String propertyPathSeparator, String dataPropertyValue) throws OntoSpeciesException{
		OWLDataProperty dataProperty = null;
		if(dataPropertyName!=null && !dataPropertyName.isEmpty() && (dataPropertyName.startsWith("http://") || dataPropertyName.startsWith("https://"))){
			dataProperty = createOWLDataProperty(dataFactory, dataPropertyName,
				EMPTY, propertyPathSeparator);
		}else{
			dataProperty = createOWLDataProperty(dataFactory, basePathTBox,
					dataPropertyName, propertyPathSeparator);			
		}
		// Creates the value of the data property being created.
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyName, dataPropertyValue);
		// Adds to the ontology the comment about a mechanism in CTML.
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal)));		
	}
	
	/**
	 * Creates a data property, adds the value to it and adds it to
	 * an individual.
	 * 
	 * @param individual
	 * @param dataPropertyIRI
	 * @param dataPropertyName
	 * @param propertyPathSeparator
	 * @param dataPropertyValue
	 * @throws OntoSpeciesException
	 */
	public void addDataPropertyToIndividual(OWLIndividual individual, String dataPropertyIRI, String dataPropertyName, String propertyPathSeparator, String dataPropertyValue) throws OntoSpeciesException{
		OWLDataProperty dataProperty = createOWLDataProperty(dataFactory, dataPropertyIRI, dataPropertyName, propertyPathSeparator);
		// Creates the value of the data property being created.
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyIRI.concat(propertyPathSeparator).concat(dataPropertyName), dataPropertyValue);
		// Adds to the ontology the comment about a mechanism in CTML.
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal)));		
	}
	
	/**
	 * Creates an instance of a class.
	 * 
	 * @param instance
	 * @param type
	 * @return
	 */
	public OWLIndividual createInstance(String instance, String type){
		// Creates a class.
		OWLClass clas = createOWLClass(dataFactory, basePathTBox, type);
		// Creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Reads the data property
		// Adds to the ontology the instance of the class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clas, individual)));
		return individual;
	}

	/**
	 * 
	 * 
	 * @param tBoxPathOfExternalOntology
	 * @param instance
	 * @param type
	 * @return
	 */
	public OWLIndividual createInstance(String externalOntologyIRI, String instance, String type){
		// Creates a class.
		OWLClass clas = createOWLClass(dataFactory, externalOntologyIRI, type);
		// Creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Reads the data property
		// Adds to the ontology the instance of the class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clas, individual)));
		return individual;
	}

	
	/**
	 * Following the creation of an instance it adds the name to it.
	 * 
	 * @param name
	 * @throws OntoSpeciesException
	 */
	public void addInstanceName(String instanceIRI, String name) throws OntoSpeciesException{
		try {
			// Adds the name of the mechanism
			addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), name, instanceIRI);
		} catch (OntoSpeciesException e) {
			logger.error(
					"The mechanism name of a single species mechanism could not be created.");
		}
	}
	
	/**
	 * Creates an object property with the domain and range specified.
	 * Applies the property to connect two objects.
	 * 
	 * @param comment
	 * @throws OntoSpeciesException
	 */
	public void linkInstance(String objectPropertyName, String domainInstance, String rangeInstance) throws OntoSpeciesException {
		// Creates the object property if not created yet
		OWLObjectProperty objectProperty;
		if(objectPropertyName!=null && (objectPropertyName.trim().startsWith("http://") || objectPropertyName.trim().startsWith("https://"))){
			objectProperty = dataFactory
					.getOWLObjectProperty(objectPropertyName.trim());			
		}else{
			objectProperty = dataFactory
				.getOWLObjectProperty(basePathTBox.concat("#").concat(objectPropertyName));
		}
		// Creates the range instance if not created yet
		OWLIndividual rangeIndividual = createOWLIndividual(dataFactory, basePathABox, rangeInstance);
		// Creates the domain instance if not created yet
		OWLIndividual domainIndividual = createOWLIndividual(dataFactory, basePathABox,
				domainInstance);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainIndividual, rangeIndividual)));		
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
	 * @throws OntoSpeciesException
	 */
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String propertyName, String literal) throws OntoSpeciesException{
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
					throw new OntoSpeciesException("The following value is not an integer:"+literal);
				}
			} else if(dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()).equals("float")){
				try{
					return ontoFactory.getOWLLiteral(Float.parseFloat(literal));
				}catch(NumberFormatException e){
					throw new OntoSpeciesException("The following value is not a float:"+literal);
				}
			} else if(dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()).equals("double")){
				try{
					return ontoFactory.getOWLLiteral(Double.parseDouble(literal));
				}catch(NumberFormatException e){
					throw new OntoSpeciesException("The following value is not a double:"+literal);
				}
			} else if(dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()).equals("date")){
				try{
					return ontoFactory.getOWLLiteral(literal);
				}catch(NumberFormatException e){
					throw new OntoSpeciesException("The following value is not a date:"+literal);
				}
			}
		}
		throw new OntoSpeciesException("The following data type could not be recognised:"+dataPropertyNameVsTypeMap.get(propertyName.toLowerCase()));
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
		return ontoFactory.getOWLClass(owlFilePath.concat("#").concat(className));
	}
	
	private OWLIndividual createOWLIndividual(OWLDataFactory ontoFactory, String owlFilePath, String individualName){
		return ontoFactory.getOWLNamedIndividual(owlFilePath.concat("#").concat(individualName));
	}
	
	private OWLIndividual createOWLIndividual(OWLDataFactory ontoFactory, String owlFilePath){
		return ontoFactory.getOWLNamedIndividual(owlFilePath);
	}
	
	private OWLDataProperty createOWLDataProperty(OWLDataFactory dataFactory, String iri, String propertyName, String separator){
		return dataFactory.getOWLDataProperty(iri.concat(separator).concat(propertyName));
	}
	
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String literal){
		return ontoFactory.getOWLLiteral(literal);
	}

	public void init(){
		
	}
	
	/**
	 * Removes TBox(es) from the generated species ontology  
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
			BufferedReader br = NISTConverterUtils.openSourceFile(owlFileNameOriginal);
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
				logger.error("A species OWL ontology file could not be renamed.");
			}
		}else{
			logger.error("A species OWL ontology file could not be deleted.");
		}
	}

}
