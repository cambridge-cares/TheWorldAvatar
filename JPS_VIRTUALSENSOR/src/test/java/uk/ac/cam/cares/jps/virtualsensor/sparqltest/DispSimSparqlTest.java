package uk.ac.cam.cares.jps.virtualsensor.sparqltest;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
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
    
    public void testGetScope() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	DispSimSparql.GetScope(sim_iri);
    }
    
    public void testAddMainStation() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	String station_iri = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation5";
    	DispSimSparql.AddMainStation(sim_iri, station_iri);
    }
    
    public void testAddSubStations() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	String[] station_iri_string = new String [2];
    	station_iri_string[0] = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation1";
    	station_iri_string[1] = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation2";
    	DispSimSparql.AddSubStations(sim_iri,station_iri_string);
    }
    
    public void testAddShip() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	
    }
}
