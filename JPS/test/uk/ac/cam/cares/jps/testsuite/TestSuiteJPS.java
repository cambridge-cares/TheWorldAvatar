package uk.ac.cam.cares.jps.testsuite;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import uk.ac.cam.cares.jps.building.test.DrawShapes;
import uk.ac.cam.cares.jps.building.test.TestBuildingQueryAgent;
import uk.ac.cam.cares.jps.building.test.TestBuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.test.TestCTSTransformer;
import uk.ac.cam.cares.jps.building.test.TestPolygonUtil;
import uk.ac.cam.cares.jps.building.test.TestSPARQLQuery;
import uk.ac.cam.cares.jps.config.test.TestADMSAgent;
import uk.ac.cam.cares.jps.config.test.TestADMSHelper;
import uk.ac.cam.cares.jps.config.test.TestADMSOutput;
import uk.ac.cam.cares.jps.config.test.TestADMSPowerPlantGetter;
import uk.ac.cam.cares.jps.config.test.TestADMSStarter;
import uk.ac.cam.cares.jps.coordination.test.TestCoordinationAgent;
import uk.ac.cam.cares.jps.servicespool.test.TestADMSWithJSON;
import uk.ac.cam.cares.jps.servicespool.test.TestBuildingData;
import uk.ac.cam.cares.jps.servicespool.test.TestBuildingJSON;
import uk.ac.cam.cares.jps.servicespool.test.TestGetBuildingDataForSimulation;
import uk.ac.cam.cares.jps.servicespool.test.TestGetPlantsInRegion;
import uk.ac.cam.cares.jps.servicespool.test.TestJSONFlattenTool;
import uk.ac.cam.cares.jps.servicespool.test.TestRegionModel;
import uk.ac.cam.cares.jps.servicespool.test.TestWholeADMS;
import uk.ac.cam.cares.jps.servicespool.test.TestWriteMet;

@RunWith(Suite.class)

@SuiteClasses({
	DrawShapes.class,
	TestBuildingQueryAgent.class,
	TestBuildingQueryPerformer.class,
	TestCTSTransformer.class,
	TestPolygonUtil.class,
	TestSPARQLQuery.class,
	TestADMSAgent.class,
	TestADMSHelper.class,
	TestADMSOutput.class,
	TestADMSPowerPlantGetter.class,
	TestADMSStarter.class,
	TestCoordinationAgent.class,
	TestADMSWithJSON.class,
	TestBuildingData.class,
	TestBuildingJSON.class,
	TestGetBuildingDataForSimulation.class,
	TestGetPlantsInRegion.class,
	TestJSONFlattenTool.class,
	TestRegionModel.class,
	TestWholeADMS.class,
	TestWriteMet.class
})
public class TestSuiteJPS {
}
