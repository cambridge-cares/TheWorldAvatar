package uk.ac.cam.cares.jps.base.query.fed.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
    SimpleMultiIndexTest.class,
    ServiceDescriptionIndexerTest.class,
	DataSourceSelectorTest.class,
	DatasetGeneratorTest.class,
	BlazegraphRepositoryWrapperIntegrationTest.class, 
	FedXIntegrationTest.class,
	FedQueryBlazegraphGivenEndpointsIntegrationTest.class,
	FedQueryRdf4jSourceSelectionIntegrationTest.class,
	//FedQueryRdf4jGivenEndpointsIntegrationTest.class
})

public class TestSuite {
}
