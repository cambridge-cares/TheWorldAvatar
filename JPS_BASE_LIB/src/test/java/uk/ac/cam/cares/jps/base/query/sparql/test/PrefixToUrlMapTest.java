package uk.ac.cam.cares.jps.base.query.sparql.test;


import org.junit.Assert;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;

public class PrefixToUrlMapTest{
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
		Assert.assertEquals("PREFIX dbpediao:<http://dbpedia.org/ontology/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("dbpediao")); //Test whether getPrefixForSPARQL method is able to build the string correctly
		//Subsequent tests on whether the string contains the appropriate characters such as PREFIX, : , < , >
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("dbpediap").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("dbpediap").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("dbpediap").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("dbpediar").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("dbpediar").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("dbpediar").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("dc").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("dc").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("dc").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("dcam").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("dcam").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("dcam").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("foaf").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("foaf").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("foaf").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("rdf").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("rdf").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("rdf").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("rdfs").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("rdfs").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("rdfs").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("skos").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("skos").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("skos").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("time").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("time").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("time").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("xsd").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("xsd").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("xsd").contains(">"));
		
	
		// prefixes for OntoCape
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPBEHA").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPBEHA").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPBEHA").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPGEOM").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPGEOM").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPGEOM").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPMATE").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPMATE").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPMATE").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPMATH").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPMATH").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPMATH").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPPHAS").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPPHAS").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPPHAS").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPSPAC").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPSPAC").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPSPAC").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPSYST").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPSYST").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPSYST").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPTECH").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPTECH").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPTECH").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OCPTOPO").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OCPTOPO").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OCPTOPO").contains(">"));
		

		// prefixes for OntoPowerSys
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OPSBEHA").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OPSBEHA").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OPSBEHA").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OPSMODE").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OPSMODE").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OPSMODE").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("OPSREAL").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("OPSREAL").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("OPSREAL").contains(">"));
		
		
		// prefixes for other JPS ontologies
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("JPSAGEN").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("JPSAGEN").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("JPSAGEN").contains(">"));
		Assert.assertTrue(PrefixToUrlMap.getPrefixForSPARQL("JPSLAND").contains("PREFIX") && PrefixToUrlMap.getPrefixForSPARQL("JPSLAND").contains(":<") && PrefixToUrlMap.getPrefixForSPARQL("JPSLAND").contains(">"));
		
	}

}