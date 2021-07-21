package uk.ac.cam.cares.jps.base.query.sparql;

/**
 * This interface defines common prefixes for standard ontologies and JPS specific prefixes.
 * The JPS specific prefixes start with 3 letters that describe the "category" and 
 * that are followed by 4 letters that usually coincide with the first four letters of the OWL file itself, e.g.
 * OCPMATH defines the prefix for http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#"
 * where OPC stands for OntoCAPE and MATH for mathematical_model.owl.
 */
public interface Prefixes {
	
	// standard ontologies
	String DBPEDIA_O = "dbpediao";
	String DBPEDIA_P = "dbpediap";
	String DBPEDIA_R = "dbpediar";
	String DC = "dc";
	String DCAM = "dcam";
	String DCTERMS = "dcterms";
	String FOAF = "foaf";
	String OWL = "owl";
	String RDF = "rdf";
	String RDFS = "rdfs";
	String SKOS = "skos";
	String XSD = "xsd";
	String TIME = "time";
	
	// OntoCape OCP
	String OCPBEHA = "OCPBEHA";
	String OCPGEOM = "OCPGEOM";
	String OCPMATE = "OCPMATE";
	String OCPMATH = "OCPMATH";
	String OCPPHAS = "OCPPHAS";
	String OCPSPAC = "OCPSPAC";
	String OCPSYST = "OCPSYST";
	String OCPTECH = "OCPTECH";
	String OCPTOPO = "OCPTOPO";	
	//****(the following are added by @wx243)****
	String OCPDSIU = "OCPDSIU";
	String OCPST = "OCPST";
	String OCPNTSYS = "OCPNS";
	String OCPPHYDMS = "OCPPHYDMS";
	String OCPCOORSYS = "OCPCOORSYS";
	String OCPSIU = "OCPSIU";
	String OCPMATHRLT = "OCPMATHRLT";
	String MATAMDLTOPO = "MATAMDLTOPO";
	
	// OntoPowerSys OPS
	String OPSBEHA = "OPSBEHA";
	String OPSMODE = "OPSMODE";
	String OPSREAL = "OPSREAL";
	//****OntoEIP OEIP (the following are added by @wx243)****
	String OPSFUNC = "OPSFUNC";
	String OPSPERFM = "OPSPERFM";

	// other JPS ontologies 
	String JPSAGEN = "JPSAGEN";
	String JPSLAND = "JPSLAND";
	
	//****OntoEIP OEIP (the following are added by @wx243)****
	String OEIPSYSREAL = "OEIPSYSREAL";
	String OEIPSYSFUNC = "OEIPSYSFUNC";
	String OEIPPOWPLANT = "OEIPPOWPLANT";
	String OEIPUPSV1 = "OEIPUPSV1";
	String OEISYSPERFM = "OEISYSPERFM";
	String OEISYSRQRM = "OEISYSRQRM";
	
	
}
