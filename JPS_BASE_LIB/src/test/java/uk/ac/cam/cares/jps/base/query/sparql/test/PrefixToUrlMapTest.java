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
		Assert.assertEquals("PREFIX dbpediao:<http://dbpedia.org/ontology/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("dbpediao"));
		Assert.assertEquals("PREFIX dbpediap:<http://dbpedia.org/property/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("dbpediap"));
		Assert.assertEquals("PREFIX dbpediar:<http://dbpedia.org/resource/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("dbpediar"));
		Assert.assertEquals("PREFIX dc:<http://purl.org/dc/elements/1.1/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("dc"));
		Assert.assertEquals("PREFIX dcam:<http://purl.org/dc/dcam/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("dcam"));
		Assert.assertEquals("PREFIX dcterms:<http://purl.org/dc/terms/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("dcterms"));
		Assert.assertEquals("PREFIX foaf:<http://xmlns.com/foaf/0.1/> \r\n",PrefixToUrlMap.getPrefixForSPARQL("foaf"));
		Assert.assertEquals("PREFIX owl:<http://www.w3.org/2002/07/owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("owl"));
		Assert.assertEquals("PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("rdf"));
		Assert.assertEquals("PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("rdfs"));
		Assert.assertEquals("PREFIX skos:<http://www.w3.org/2004/02/skos/core#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("skos"));
		Assert.assertEquals("PREFIX time:<https://www.w3.org/2006/time#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("time"));
		Assert.assertEquals("PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("xsd"));
	
		// prefixes for OntoCape
		Assert.assertEquals("PREFIX OCPBEHA:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPBEHA"));
		Assert.assertEquals("PREFIX OCPGEOM:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPGEOM"));
		Assert.assertEquals("PREFIX OCPMATE:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPMATE"));
		Assert.assertEquals("PREFIX OCPMATH:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPMATH"));
		Assert.assertEquals("PREFIX OCPPHAS:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPPHAS"));
		Assert.assertEquals("PREFIX OCPSPAC:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPSPAC"));
		Assert.assertEquals("PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPSYST"));
		Assert.assertEquals("PREFIX OCPTECH:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPTECH"));
		Assert.assertEquals("PREFIX OCPTOPO:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OCPTOPO"));
		
		// prefixes for OntoPowerSys
		Assert.assertEquals("PREFIX OPSBEHA:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OPSBEHA"));
		Assert.assertEquals("PREFIX OPSMODE:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OPSMODE"));
		Assert.assertEquals("PREFIX OPSREAL:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("OPSREAL"));
		
		// prefixes for other JPS ontologies
		Assert.assertEquals("PREFIX JPSAGEN:<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("JPSAGEN"));
		Assert.assertEquals("PREFIX JPSLAND:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> \r\n",PrefixToUrlMap.getPrefixForSPARQL("JPSLAND"));
	}

}