package uk.ac.cam.cares.des.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.des.ResidentialAgent;

public class Test_AgentsNew extends TestCase{
	public void testResidentialAgent() {
		String iriofnetworkdistrict =  "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
		new ResidentialAgent().extractResidentialData(iriofnetworkdistrict,"whatever");
	}
}
