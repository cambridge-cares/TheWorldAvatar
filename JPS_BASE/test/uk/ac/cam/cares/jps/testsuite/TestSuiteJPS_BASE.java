package uk.ac.cam.cares.jps.testsuite;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import uk.ac.cam.cares.jps.base.config.test.TestKeyValueServer;
import uk.ac.cam.cares.jps.base.discovery.test.TestAgentCaller;
import uk.ac.cam.cares.jps.base.discovery.test.TestDiscovery;
import uk.ac.cam.cares.jps.base.util.test.TestUtils;

@RunWith(Suite.class)

@SuiteClasses({
	TestKeyValueServer.class,
	//TestLogging.class,
	TestAgentCaller.class,
	TestDiscovery.class,
	TestUtils.class
})
public class TestSuiteJPS_BASE {

}
