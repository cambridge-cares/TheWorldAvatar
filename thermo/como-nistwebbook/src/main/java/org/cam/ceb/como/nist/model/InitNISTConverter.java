package org.cam.ceb.como.nist.model;

import org.cam.ceb.como.nist.converter.NISTConverter;
import org.cam.ceb.como.nist.converter.NISTOWLWriter;
import org.semanticweb.owlapi.apibinding.OWLManager;

/**
 * Implemented the following method of the IInitNISTConverter interface:</br>
 * - the init method;</br>
 * 
 * @author msff2
 *
 */
public class InitNISTConverter extends NISTConverter implements IInitNISTConverter {
	public void init() {
		
		if (iNISTOwlWriter == null) {
			iNISTOwlWriter = new NISTOWLWriter();
		}
		
		if (ontoSKBPropreties == null) {
			ontoSKBPropreties = PropertiesManager.loadProperties(NISTConverterState.class.getClassLoader().getResourceAsStream("ontospecies.kb.properties"));
		}
		
		if (ontoSKBTBoxIRI == null) {
			ontoSKBTBoxIRI = ontoSKBPropreties.getProperty("ontospecies.tbox.iri").toString();
		}

		if (ontoKinTBoxIRI == null) {
			ontoKinTBoxIRI = ontoSKBPropreties.getProperty("ontokin.tbox.iri").toString();
		}

		if (ontoSKBIRI == null) {
			ontoSKBIRI = ontoSKBPropreties.getProperty("ontospecies.kb.iri").toString();
		}

		if (ontoSKBNS == null) {
			ontoSKBNS = ontoSKBPropreties.getProperty("ontospecies.ns").toString();
		}

		if (ontoSKBRDF4JServerURL == null) {
			ontoSKBRDF4JServerURL = ontoSKBPropreties.getProperty("ontospecies.rdf4j.server.url").toString();
		}

		if (ontoSKBRepositryID == null) {
			ontoSKBRepositryID = ontoSKBPropreties.getProperty("ontospecies.repository.id").toString();
		}

		if (ontoSKBRepositryURL == null) {
			ontoSKBRepositryURL = ontoSKBPropreties.getProperty("ontospecies.rdf4j.repository.url").toString();
		}
		
		if (ontoSCSVPropreties == null) {
			ontoSCSVPropreties = PropertiesManager.loadProperties(NISTConverterState.class.getClassLoader().getResourceAsStream("ontospecies.csv.file.properties"));
		}
		
		if (ontoSCSVFileName == null) {
			ontoSCSVFileName = ontoSCSVPropreties.getProperty("tbox.file.name").toString();
		}
		
		if (ontoSCSVPKeyColumnNo == 0) {
			ontoSCSVPKeyColumnNo = Integer.parseInt(ontoSCSVPropreties.getProperty("tbox.csv.file.datatype.key.column.index").toString());
		}

		if (ontoSCSVPValueColumnNo == 0) {
			ontoSCSVPValueColumnNo = Integer.parseInt(ontoSCSVPropreties.getProperty("tbox.csv.file.datatype.value.column.index").toString());
		}

		if (ontoSCSVTermTypeColumnNo == 0) {
			ontoSCSVTermTypeColumnNo = Integer.parseInt(ontoSCSVPropreties.getProperty("tbox.csv.file.source.type.column.index").toString());
		}

		if (ontoSCSVDataPropertyName == null) {
			ontoSCSVDataPropertyName = ontoSCSVPropreties.getProperty("tbox.csv.file.source.type.data.property").toString();
		}
		
		dataFactory = OWLManager.getOWLDataFactory();
		manager = OWLManager.createOWLOntologyManager();
		
		ontology = null;
		ontologyIRI = null;
		ontologyIRIFileSave = null;
		basePathTBox = "";
		basePathABox = "";
		speciesUniqueID = "";
		individual = null;
		iNistOWLWriter = new NISTOWLWriter();
	}	
}
