package org.cam.ceb.como.nist.model;

import java.util.HashMap;
import java.util.Properties;

import org.cam.ceb.como.nist.converter.INISTOWLWriter;
import org.cam.ceb.como.nist.converter.NISTOWLWriter;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

/**
 * This class maintains the current state of all the member variables
 * used in the Species to OWL conversion.
 * 
 * @author msff2
 *
 */
public class NISTConverterState {
	// Following ontoSKB member variables are used to read properties
	// from the ontospecies.kb.properties file.
	public static Properties ontoSKBPropreties;
	public static String ontoSKBTBoxIRI;
	public static String ontoKinTBoxIRI;
	public static String ontoSKBIRI;
	public static String ontoSKBNS;
	public static String ontoSKBRDF4JServerURL;
	public static String ontoSKBRepositryID;
	public static String ontoSKBRepositryURL;		
	// Following ontoSCSV member variables are used to read properties
	// from the ontospecies.csv.file.properties file.
	public static Properties ontoSCSVPropreties;
	// The name of the CSV file.
	public static String ontoSCSVFileName;
	// The column number where the name of property is provided
	// in the CSV file.
	public static int ontoSCSVPKeyColumnNo;
	// The column number where the data type of property is provided
	// in the CSV file.
	public static int ontoSCSVPValueColumnNo;
	// The column number where the type of the current vocabulary item, which
	// can be a class, object property or data property, is provided
	// in the CSV file.
	public static int ontoSCSVTermTypeColumnNo;
	// The name which is used to refer to the data property in the type
	// column in the CSV file.
	public static String ontoSCSVDataPropertyName;

	// Declared to serve as the seed for generating species UUID (Universally Unique Identifier).
	public static long speciesId = System.nanoTime();
	public static long weblinkId = System.nanoTime();
	public static long enthalpyId = System.nanoTime();
	public static long temperatureId = System.nanoTime();
	
	public static String uniqueSpeciesId = "";
	public static INISTOWLWriter iNISTOwlWriter;
	public static final String EMPTY = "";
	public static final String HASH = "#";
	public static final String SPACE = " ";
	public static final String SPACE_3 = "   ";
	public static final String NEWLINE = "\n";
	public static final String COLON = ":";
	public static final String UNDERSCORE = "_";
	public static final String BACKSLASH = "/";
	public static final String FRONTSLASH = "\\";
	public static final String COMMA = ",";
	public static final String DOT = ".";
	public static final String TAB = "\t";
	public static OWLDataFactory dataFactory = OWLManager.getOWLDataFactory();
	public static OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	public static OWLOntologyManager managerKB = OWLManager.createOWLOntologyManager();
	public static OWLOntology ontology;
	public static OWLOntology kb;
	public static IRI ontologyIRI;
	public static IRI kbIRI;
	public static IRI ontologyIRIFileSave;
	public static final String RDFS = "rdfs";
	public static final String RDFS_LABEL = "label";
	public static final String RDFS_COMMENT = "comment";
	public static final String RDFS_URL = "http://www.w3.org/2000/01/rdf-schema#";
	public static final String RDF = "rdf";
	public static final String RDF_TYPE = "type";
	public static final String RDF_URL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";	
	public static final String OWL = "owl";
	public static final String OWL_VERSIONINFO = "versionInfo";
	public static final String OWL_URL = "http://www.w3.org/2002/07/owl#";
	public static final String OWL_SAME_AS = "sameAs";
	public static final String DUBLIN_CORE = "dc";
	public static final String DUBLIN_CORE_ID = "identifier";
	public static final String DUBLIN_CORE_URL = "http://purl.org/dc/elements/1.1";
	public static final String GEOSPARQL = "geosparql";
	public static final String GEOSPARQL_DIMENSION = "dimension";
	public static final String GEOSPARQL_URL = "http://www.opengis.net/ont/geosparql#";
	public static final String SKOS_URL = "http://www.w3.org/2004/02/skos/core";
	
	
	public static String basePathTBox;
	public static String basePathABox;
	public static String speciesUniqueID;
	public static HashMap<String, String> dataPropertyNameVsTypeMap = new HashMap<String, String>();
	public static boolean dataTypePopulated = false;
	public static IInitNISTConverter initNISTConverter;
	public static OWLIndividual individual;
	public static INISTOWLWriter iNistOWLWriter;
	// A member variable created to hold the instance of the class that
	// implemented OWL constructs, i.e. class, instance, object property
	// and data, creation methods. 
	public static INISTOWLWriter iNISTOWLWriter;
}
