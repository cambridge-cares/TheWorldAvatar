package uk.ac.cam.cares.jps.testsuite;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import uk.ac.cam.cares.jps.agents.test.TestAgentDescriptions;
import uk.ac.cam.cares.jps.agents.test.TestAgentOntology;
import uk.ac.cam.cares.jps.agents.test.TestAgentWebAPI;
import uk.ac.cam.cares.jps.composition.test.RequestTest;
import uk.ac.cam.cares.jps.composition.test.ServiceCompositionEngineTest;
import uk.ac.cam.cares.jps.composition.test.ServiceDiscoveryTest;
import uk.ac.cam.cares.jps.composition.test.ServiceModelTest;
import uk.ac.cam.cares.jps.composition.test.TestCityToWeather;
import uk.ac.cam.cares.jps.composition.test.TestCompositionJSON;
import uk.ac.cam.cares.jps.composition.test.TestGetNamesUnderMessagePart;
import uk.ac.cam.cares.jps.composition.test.TestLocalQueryEndPoint;
import uk.ac.cam.cares.jps.composition.test.TestNestedMessageParts;
import uk.ac.cam.cares.jps.composition.test.TestRegionToCity;
import uk.ac.cam.cares.jps.composition.test.UtilTest;

@RunWith(Suite.class)

@SuiteClasses({
	TestAgentDescriptions.class,
	TestAgentOntology.class,
	TestAgentWebAPI.class,
	RequestTest.class,
	ServiceCompositionEngineTest.class,
	ServiceDiscoveryTest.class,
	ServiceModelTest.class,
	TestCityToWeather.class,
	TestCompositionJSON.class,
	TestGetNamesUnderMessagePart.class,
	TestLocalQueryEndPoint.class,
	TestNestedMessageParts.class,
	TestRegionToCity.class,
	UtilTest.class
})
public class TestSuiteJPS_COMPOSITION {

}
