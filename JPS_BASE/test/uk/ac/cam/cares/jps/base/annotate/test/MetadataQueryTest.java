package uk.ac.cam.cares.jps.base.annotate.test;


import static org.junit.Assert.assertNotNull;

import java.net.URISyntaxException;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;


public class MetadataQueryTest {
	private String iriofnetwork = null;
	
	@Before
	public void setUp() throws URISyntaxException {
		iriofnetwork 	= "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	}
	
	
	// Runtime Exception when an invalid argument is given (Eg. an IRI instead of a valid sparql query)
	@Test(expected = JPSRuntimeException.class)
	public void testLoadBadFile() {
		MetaDataQuery.query(iriofnetwork);
	}
	
	
	//Pass a test query which is valid  
	@Test
	public void testQuery() {
		String queryString = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")				
				.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#")
				.addVar("?entity").addVar("?valueofx").addVar("?valueofy").addVar("?BusNumbervalue")
				.addWhere("?entity" ,"a", "j1:PowerGenerator").addWhere("?entity" ,"j9:realizes", "<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SolarGeneration>")
				.addWhere("?entity" ,"j2:isModeledBy", "?model").addWhere("?model" ,"j5:hasModelVariable", "?num")
				.addWhere("?num" ,"a", "j3:BusNumber").addWhere("?num" ,"j2:hasValue", "?vnum")
				.addWhere("?vnum" ,"j2:numericalValue", "?BusNumbervalue")
				
				.addWhere("?entity" ,"j7:hasGISCoordinateSystem", "?coorsys")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_x", "?x")
				.addWhere("?x" ,"j2:hasValue", "?xval").addWhere("?xval" ,"j2:numericalValue", "?valueofx")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_y", "?y")
				.addWhere("?y" ,"j2:hasValue", "?yval").addWhere("?yval" ,"j2:numericalValue", "?valueofy").buildString();

		String trueQuery = "SELECT ?p ?o\r\n" + 
				"{ \r\n" + 
				"  <http://nasa.dataincubator.org/spacecraft/1968-089A> ?p ?o\r\n" + 
				"}";
		assertNotNull(MetaDataQuery.query(queryString));
		assertNotNull(MetaDataQuery.query(trueQuery));
		
	}
}
