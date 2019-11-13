package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.AggregationEmissionAgent;

public class TestAggregation extends TestCase{

	public void xxxtestsumagg() {
		JSONObject x= new AggregationEmissionAgent().sumEmissionResult("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		int size=x.getJSONArray("plant").length();
		System.out.println(x.getJSONArray("plant").get(2));
		System.out.println("total actco2 for plant 1= " + x.getJSONArray("emission").getDouble(2));
		//assert
	}
}
