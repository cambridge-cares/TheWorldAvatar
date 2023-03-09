package uk.ac.cam.cares.jps.base.query.sparql.test;


import org.junit.Assert;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

public class PrefixToUrlMapTest {
	//Test getPrefixUrl()
	@Test
	public void testgetPrefixUrl()
	{
		//prefixes for standard ontologies
		Assert.assertEquals("http://dbpedia.org/ontology/",PrefixToUrlMap.getPrefixUrl("dbpediao"));
		Assert.assertEquals("http://dbpedia.org/property/",PrefixToUrlMap.getPrefixUrl("dbpediap"));
		Assert.assertEquals("http://dbpedia.org/resource/",PrefixToUrlMap.getPrefixUrl("dbpediar"));
		Assert.assertEquals("http://purl.org/dc/elements/1.1/",PrefixToUrlMap.getPrefixUrl("dc"));
		Assert.assertEquals("http://purl.org/dc/dcam/",PrefixToUrlMap.getPrefixUrl("dcam"));
		Assert.assertEquals("http://purl.org/dc/terms/",PrefixToUrlMap.getPrefixUrl("dcterms"));
		Assert.assertEquals("http://xmlns.com/foaf/0.1/",PrefixToUrlMap.getPrefixUrl("foaf"));
		Assert.assertEquals("http://www.w3.org/2002/07/owl#",PrefixToUrlMap.getPrefixUrl("owl"));
		Assert.assertEquals("http://www.w3.org/1999/02/22-rdf-syntax-ns#",PrefixToUrlMap.getPrefixUrl("rdf"));
		Assert.assertEquals("http://www.w3.org/2000/01/rdf-schema#",PrefixToUrlMap.getPrefixUrl("rdfs"));
		Assert.assertEquals("http://www.w3.org/2004/02/skos/core#",PrefixToUrlMap.getPrefixUrl("skos"));
		Assert.assertEquals("https://www.w3.org/2006/time#",PrefixToUrlMap.getPrefixUrl("time"));
		Assert.assertEquals("http://www.w3.org/2001/XMLSchema#",PrefixToUrlMap.getPrefixUrl("xsd"));

		// prefixes for OntoCape
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#",PrefixToUrlMap.getPrefixUrl("OCPBEHA"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#",PrefixToUrlMap.getPrefixUrl("OCPGEOM"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#",PrefixToUrlMap.getPrefixUrl("OCPMATE"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#",PrefixToUrlMap.getPrefixUrl("OCPMATH"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#",PrefixToUrlMap.getPrefixUrl("OCPPHAS"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#",PrefixToUrlMap.getPrefixUrl("OCPSPAC"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#",PrefixToUrlMap.getPrefixUrl("OCPSYST"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#",PrefixToUrlMap.getPrefixUrl("OCPTECH"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#",PrefixToUrlMap.getPrefixUrl("OCPTOPO"));
		
		// prefixes for OntoPowerSys
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#",PrefixToUrlMap.getPrefixUrl("OPSBEHA"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#",PrefixToUrlMap.getPrefixUrl("OPSMODE"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#",PrefixToUrlMap.getPrefixUrl("OPSREAL"));
		
		// prefixes for other JPS ontologies
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#",PrefixToUrlMap.getPrefixUrl("JPSAGEN"));
		Assert.assertEquals("http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#",PrefixToUrlMap.getPrefixUrl("JPSLAND"));

	}
	
	//Test getPrefixForSPARQL()
	@Test
	public void testgetPrefixforSPARQL()
	{
		//prefixes for standard ontologies
		//Test whether the string contains the appropriate characters such as PREFIX, : , < , >
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("dbpediao").contains("PREFIX"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("dbpediao").contains(":<"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("dbpediao").contains(">"));
		//Test whether getPrefixForSPARQL method is able to build the string correctly		
		Assert.assertEquals("PREFIX dbpediao:<http://dbpedia.org/ontology/>\r\n",PrefixToUrlMap.getPrefixForSPARQL("dbpediao")); 
		
	}

	@Test
	public void testGetPrefixesForSPARQL() {
		String actual = PrefixToUrlMap.getPrefixesForSPARQL(Prefixes.DC, Prefixes.WIKIDATAT, Prefixes.WIKIDATA);
		String expected = "PREFIX dc:<http://purl.org/dc/elements/1.1/>\r\n"
				+ "PREFIX wdt:<http://www.wikidata.org/prop/direct/>\r\n"
				+ "PREFIX wd:<http://www.wikidata.org/entity/>\r\n";
		Assert.assertEquals(expected, actual);
	}
}