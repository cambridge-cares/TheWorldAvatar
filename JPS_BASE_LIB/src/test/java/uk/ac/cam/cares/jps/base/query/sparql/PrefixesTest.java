package uk.ac.cam.cares.jps.base.query.sparql;


import junit.framework.TestCase;

public class PrefixesTest extends TestCase {

	public void testPrefixes() {
		
		
		// Test standard ontologies
		assertEquals(Prefixes.DBPEDIA_O,"dbpediao");
		assertEquals(Prefixes.DBPEDIA_P,"dbpediap");
		assertEquals(Prefixes.DBPEDIA_R,"dbpediar");
		assertEquals(Prefixes.DC,"dc");
		assertEquals(Prefixes.DCAM,"dcam");
		assertEquals(Prefixes.DCTERMS,"dcterms");
		assertEquals(Prefixes.FOAF,"foaf");
		assertEquals(Prefixes.OWL,"owl");
		assertEquals(Prefixes.RDF,"rdf");
		assertEquals(Prefixes.RDFS,"rdfs");
		assertEquals(Prefixes.SKOS,"skos");
		assertEquals(Prefixes.XSD,"xsd");
		assertEquals(Prefixes.TIME,"time");
		
		// Test OntoCape OCP
		assertEquals(Prefixes.OCPBEHA,"OCPBEHA");
		assertEquals(Prefixes.OCPGEOM,"OCPGEOM");
		assertEquals(Prefixes.OCPMATE,"OCPMATE");
		assertEquals(Prefixes.OCPMATH,"OCPMATH");
		assertEquals(Prefixes.OCPPHAS,"OCPPHAS");
		assertEquals(Prefixes.OCPSPAC,"OCPSPAC");
		assertEquals(Prefixes.OCPSYST,"OCPSYST");
		assertEquals(Prefixes.OCPTECH,"OCPTECH");
		assertEquals(Prefixes.OCPTOPO,"OCPTOPO");
		
		// Test OntoPowerSys OPS
		assertEquals(Prefixes.OPSBEHA,"OPSBEHA");
		assertEquals(Prefixes.OPSMODE,"OPSMODE");
		assertEquals(Prefixes.OPSREAL,"OPSREAL");
		
		// Test other JPS ontologies 
		assertEquals(Prefixes.JPSAGEN,"JPSAGEN");
		assertEquals(Prefixes.JPSLAND,"JPSLAND");
	}

}
