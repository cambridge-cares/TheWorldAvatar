package uk.ac.cam.cares.jps.powsys.retrofit.test;

import org.junit.Test;
import static org.junit.Assert.*;
import org.apache.jena.ontology.OntModel;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import uk.ac.cam.cares.jps.powsys.retrofit.GeneralRetrofitAgent;
import uk.ac.cam.cares.jps.powsys.retrofit.BusInfo;
import uk.ac.cam.cares.jps.powsys.retrofit.GeneratorInfo;

public class TestGeneralRetrofitAgent {
	private String jpsENIRI, testENIRI;
	private JSONArray busIRI = new JSONArray();
	private JSONArray genIRI = new JSONArray();
	private List<String[]> expectedBusQuery = new ArrayList<>();

	@Before
	public void setUp () {
		//TODO Fill up the IRI in the variables.
	}

	@Nested
	public class testQuery {

		@Before
		public class testQueryMaker {
			@Test
			public void testGetQueryForBuses () {

			}

			@Test
			public void testGetQueryForGenerator(){

			}
		}

		@Test
		public void testQueryBuses () {

		}
		
	}

	@Nested
	public class testFindFirstSlackBus {
		@Test
		public void testFindFirstSlackBus_Success () {

		}

		@Test(expected = JPSRuntimeException.class)
		public void testFindFirstSlackBus_JPSRuntimeException () {

		}
	}
	
	@Test
	public void testDeletePowerGeneratorsFromElectricalNetwork () {

	}

	@Test
	public void testCompletePowerGenerator () {

	}

	@Test
	public void testAddGeneratorsToElectricalNetwork () {

	}

	@Test
	public void testConnectGeneratorToBus () {

	}

	@Test
	public void testConnectGeneratorToOptimalBus () {

	}

	@Test
	public void testConnectNuclearPowerGeneratorsOfPlantsToOptimalBus () {

	}


}