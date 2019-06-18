package uk.ac.cam.cares.jps.base.query.sparql;

public interface Prefixes {
	
	// standard ontologies
	String DC = "dc";
	String DCAM = "dcam";
	String DCTERMS = "dcterms";
	String OWL = "owl";
	String RDF = "rdf";
	String RDFS = "rdfs";
	String SKOS = "skos";
	String XSD = "xsd";
	String TIME = "time";
	
	// OntoCape
	String OCPBEHA = "OCPBEHA";
	String OCPGEOM = "OCPGEOM";
	String OCPMATE = "OCPMATE";
	String OCPMATH = "OCPMATH";
	String OCPPHAS = "OCPPHAS";
	String OCPSPAC = "OCPSPAC";
	String OCPSYST = "OCPSYST";
	String OCPTECH = "OCPTECH";
	String OCPTOPO = "OCPTOPO";
	
	// OntoPowerSys
	String OPSMODE = "OPSMODE";
	String OPSBEHA = "OPSBEHA";
	String OPSREAL = "OPSREAL";

	// other JPS ontologies
	String OXXLAND = "OXXLAND";
}
