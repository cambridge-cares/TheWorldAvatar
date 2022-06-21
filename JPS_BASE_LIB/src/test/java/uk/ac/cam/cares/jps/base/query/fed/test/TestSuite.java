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
	//FedQueryBlazegraphSourceSelectionIntegrationTest.class,
	FedQueryBlazegraphGivenEndpointsIntegrationTest.class,
	FedQueryRdf4jGivenEndpointsIntegrationTest.class,
	FedQueryRdf4jSourceSelectionIntegrationTest.class
})

public class TestSuite {
}
