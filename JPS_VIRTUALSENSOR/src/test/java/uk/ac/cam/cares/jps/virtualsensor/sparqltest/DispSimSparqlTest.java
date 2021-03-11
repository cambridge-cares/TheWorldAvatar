package uk.ac.cam.cares.jps.virtualsensor.sparqltest;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.objects.Scope;
import uk.ac.cam.cares.jps.virtualsensor.objects.DispSim;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;

public class DispSimSparqlTest extends TestCase{
    public void testInitSim() {
    	for (int i=1; i<6; i++) {
	    	JSONObject jo_region = new JSONObject();
	        Region.putRegion(jo_region, i);
	    	Scope sc = new Scope(jo_region.getJSONObject("region"));
	    	
	    	DispSim sim = new DispSim();
	    	sim.setScope(sc);
	    	sim.setNx(10);
	    	sim.setNy(10);
	    	DispSimSparql.InitSim(i, sim);
    	}
    }
}
