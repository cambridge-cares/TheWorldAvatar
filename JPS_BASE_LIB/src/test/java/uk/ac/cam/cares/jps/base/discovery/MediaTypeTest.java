package uk.ac.cam.cares.jps.base.discovery;

import junit.framework.TestCase;

public class MediaTypeTest extends TestCase {

	public void testMediaType() {		
		assertEquals(MediaType.TEXT_CSV.type,"text/csv");
		assertEquals(MediaType.TEXT_TURTLE.type, "text/turtle");
		assertEquals(MediaType.APPLICATION_JSON.type, "application/json");
		assertEquals(MediaType.APPLICATION_LD_JSON.type, "application/ld+json");
		assertEquals(MediaType.APPLICATION_SPARQL.type, "application/sparql-results+json");
		assertEquals(MediaType.APPLICATION_SPARQL_RESULTS_XML.type, "application/sparql-results+xml");
		assertEquals(MediaType.APPLICATION_SPARQL_UPDATE.type, "application/sparql-update");
		assertEquals(MediaType.APPLICATION_RDF_XML.type, "application/rdf+xml");
		assertEquals(MediaType.APPLICATION_X_WWW_FORM_URLENCODED.type, "application/x-www-form-urlencoded");
		
	}
}
