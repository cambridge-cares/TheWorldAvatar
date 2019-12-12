package uk.ac.cam.cares.ess.test;

import java.io.IOException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.des.DistributedEnergySystem;


public class Test_DES extends TestCase{
	
	public void testrunpython() throws IOException {
		DistributedEnergySystem a = new DistributedEnergySystem();
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_DES";
		a.runOptimization(baseUrl);
	}

	
	
}
