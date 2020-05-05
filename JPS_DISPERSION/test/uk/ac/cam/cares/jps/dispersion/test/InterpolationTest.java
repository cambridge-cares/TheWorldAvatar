package uk.ac.cam.cares.jps.dispersion.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.dispersion.interpolation.InterpolationAgent;

public class InterpolationTest extends TestCase{

	public void testepisoderunTestinSequence() {
		String baseUrl= QueryBroker.getLocalDataPath();
		InterpolationAgent ag = new InterpolationAgent();
		String coordinates = "[380000 150000 0]";
		String gasType = "['NO NO2'],1,'"+baseUrl+"/','3D_instantanous_mainconc_center.dat'";
		ag.copyTemplate(baseUrl,"3D_instantanous_mainconc_center.dat");
		ag.copyTemplate(baseUrl, "virtual_sensor.m");
	
		try {
			ag.createBat(baseUrl, coordinates,gasType );
			ag.runModel(baseUrl);
		}catch (Exception e) {
			e.printStackTrace();
		}
	
		}
}