package uk.ac.cam.cares.jps.base.query.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;

public class TestSPARQL extends TestCase implements Prefixes, Paths, ITestConstants {
	
	public void testPrefixMap() {
		String prefixUrl = PrefixToUrlMap.getPrefixUrl(OCPSPAC);
		assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#", prefixUrl);
	}
	
	public void testPrefixMapUnknownPrefixUrl() {
		boolean exceptionWasThrown = false;
		try {
			PrefixToUrlMap.getPrefixUrl("UNKNOWN");
		} catch (JPSRuntimeException e) {
			exceptionWasThrown = true;
		}
		assertTrue(exceptionWasThrown);
	}
	
	public void testQueryBuilderNppLandlots() {
		
		String lotsInfo= "PREFIX OXXLAND:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?xvalue ?yvalue ?areavalue ?distancevalue "
				
				+ "WHERE {?entity  a  OXXLAND:Landlot  ." 
				+ "?entity   j5:hasSurfaceGeometry ?sur ."
				+ "?sur   j5:has_area ?surarea ."
				+ "?surarea   j2:hasValue ?vsurarea ."
				+ "?vsurarea   j2:numericalValue ?areavalue ."
				
				+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
				+ "?coorsys   j7:hasProjectedCoordinate_x ?x ."
				+ "?x   j2:hasValue ?xval ."
				+ "?xval   j2:numericalValue ?xvalue ."
				+ "?coorsys   j7:hasProjectedCoordinate_y ?y ."
				+ "?y   j2:hasValue ?yval ."
				+ "?yval   j2:numericalValue ?yvalue ."
				
				+ "?entity   OXXLAND:hasDistanceToClosestWaterSources ?distance ."
				+ "?distance   j2:hasValue ?distval ."
				+ "?distval   j2:numericalValue ?distancevalue ."
								
				+ "}";	   	
		
		
		QueryBuilder builder = new QueryBuilder();
		builder.select("?entity", "?xvalue", "?yvalue", "?areavalue", "?distancevalue");
		builder.a("?entity", OXXLAND, "Landlot");
		builder.prop("?entity", "?areavalue", OCPGEOM, "hasSurfaceGeometry", OCPGEOM, "has_area", OCPSYST, "hasValue", OCPSYST, "numericalValue");
		builder.prop("?entity", "?xvalue", PGISCOORDX);
		builder.prop("?entity", "?yvalue", PGISCOORDY);
		builder.prop("?entity", "?distancevalue", OXXLAND, "hasDistanceToClosestWaterSources", OCPSYST, "hasValue", OCPSYST, "numericalValue");
		
		System.out.println(builder.build());
		
		String expected = "PREFIX OXXLAND:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> \r\n" + 
				"PREFIX OCPGEOM:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#> \r\n" + 
				"PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n" + 
				"PREFIX OCPSPAC:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> \r\n" + 
				"SELECT ?entity ?xvalue ?yvalue ?areavalue ?distancevalue \r\n" + 
				"WHERE {\r\n" + 
				"?entity a OXXLAND:Landlot .\r\n" + 
				"?entity OCPGEOM:hasSurfaceGeometry/OCPGEOM:has_area/OCPSYST:hasValue/OCPSYST:numericalValue ?areavalue .\r\n" + 
				"?entity OCPSPAC:hasGISCoordinateSystem/OCPSPAC:hasProjectedCoordinate_x/OCPSYST:hasValue/OCPSYST:numericalValue ?xvalue .\r\n" + 
				"?entity OCPSPAC:hasGISCoordinateSystem/OCPSPAC:hasProjectedCoordinate_y/OCPSYST:hasValue/OCPSYST:numericalValue ?yvalue .\r\n" + 
				"?entity OXXLAND:hasDistanceToClosestWaterSources/OCPSYST:hasValue/OCPSYST:numericalValue ?distancevalue .\r\n" + 
				"}";
		
		assertEquals(expected, builder.build().toString());
	}
	
	private OntModel createModelForEGen() {
		String path = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/EGen-001_template.owl";
		return JenaHelper.createModel(path); 
	}
	
	public void testJenaWrapperGetPropertyValue() {
		
		OntModel m = createModelForEGen();
		JenaModelWrapper w = new JenaModelWrapper(m, null);
		String startSubject = "http://www.theworldavatar.com/EGen-001_template.owl#EGen-001";
		
		RDFNode o = w.getPropertyValue(startSubject, PGISCOORDX);
		assertEquals(11.111, o.asLiteral().getDouble());
		
		o = w.getPropertyValue(startSubject, PGISCOORDY);
		assertEquals(22.222, o.asLiteral().getDouble());
	}
	
	public void testJenaWrapperUpdatePropertyValue() {
		
		OntModel m = createModelForEGen();
		JenaModelWrapper w = new JenaModelWrapper(m, null);
		String startSubject = "http://www.theworldavatar.com/EGen-001_template.owl#EGen-001";
		
		RDFNode o = w.getPropertyValue(startSubject, PGISCOORDX);
		assertEquals(11.111, o.asLiteral().getDouble());
		
		o = w.setPropertyValue(startSubject, 14.5, PGISCOORDX);
		assertEquals(14.5, o.asLiteral().getDouble());
		
		o = w.getPropertyValue(startSubject, PGISCOORDX);
		assertEquals(14.5, o.asLiteral().getDouble());
	}
	
	public void testJenaWrapperRemoveSubtree() {
		
		OntModel m = createModelForEGen();
		JenaModelWrapper w = new JenaModelWrapper(m, null);
		String startSubject = "http://www.theworldavatar.com/EGen-001_template.owl#EGen-001";
		RDFNode coordSystem = w.getPropertyValue(startSubject, OCPSPAC, "hasGISCoordinateSystem");
		assertNotNull(coordSystem);
		
		w.removeSubtree(startSubject, OCPSPAC, "hasGISCoordinateSystem");
		
		assertNull(w.getPropertyValue(startSubject, OCPSPAC, "hasGISCoordinateSystem"));
	}
	
	public void testJenaWrapperCreateNewPropertyValue() {
		
		OntModel m = createModelForEGen();
		JenaModelWrapper w = new JenaModelWrapper(m, null);
		String startSubject = "http://www.theworldavatar.com/EGen-001_template.owl#EGen-001";
		w.removeSubtree(startSubject, OCPSPAC, "hasGISCoordinateSystem");
				
		// set new x coordinate
		RDFNode o = w.setPropertyValue(startSubject, 14.32, PGISCOORDX);
		assertEquals(14.32, o.asLiteral().getDouble());
		
		// set new y coordinate
		o = w.setPropertyValue(startSubject, 7.39, PGISCOORDY);
		assertEquals(7.39, o.asLiteral().getDouble());
		
		// set new x coordinate
		o = w.setPropertyValue(startSubject, 23.76, PGISCOORDX);
		assertEquals(23.76, o.asLiteral().getDouble());
		
		System.out.println(JenaHelper.writeToString(m));
	}
}
