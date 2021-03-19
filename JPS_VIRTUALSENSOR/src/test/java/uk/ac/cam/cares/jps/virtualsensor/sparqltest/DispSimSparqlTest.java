package uk.ac.cam.cares.jps.virtualsensor.sparqltest;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.objects.DispSim;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;

public class DispSimSparqlTest extends TestCase{
	String episode_iri = "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service";
    public void testInitSim() {
    	String[] SimCRS = new String[5];
    	SimCRS[0] = "EPSG:3414";
    	SimCRS[1] = "EPSG:32648";
    	SimCRS[2] = "EPSG:2326";
    	SimCRS[3] = "EPSG:32650";
    	SimCRS[4] = "EPSG:32630";
    	double[] dz = {10,10,15,25,40,100,300,500,500,500,500,500,500};
    	for (int i=1; i<6; i++) {
	    	JSONObject jo_region = new JSONObject();
	        Region.putRegion(jo_region, i);
	    	Scope sc = new Scope(jo_region.getJSONObject("region"));
	    	
	    	DispSim sim = new DispSim();
	    	sim.setScope(sc);
	    	sim.setNx(10);
	    	sim.setNy(10);
	    	sim.setNumSubStations(1);
	    	sim.setServiceAgent(episode_iri);
	    	sim.setSimCRS(SimCRS[i-1]);
	    	sim.setDz(dz);
	    	DispSimSparql.InitSim(i, sim);
    	}
    }
    
    public void testGetScope() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	DispSimSparql.GetScope(sim_iri);
    }
    
    public void testAddMainStation() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	String station_iri = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation4";
    	DispSimSparql.AddMainStation(sim_iri, station_iri);
    }
    
    public void testAddSubStations() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	String[] station_iri_string = new String [2];
    	station_iri_string[0] = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation4";
    	station_iri_string[1] = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation5";
    	DispSimSparql.AddSubStations(sim_iri,station_iri_string);
    }
    
    public void testAddShips() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	String[] ship_iri = new String[2];
    	ship_iri[0] = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship41";
    	ship_iri[1] = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship5";
    	DispSimSparql.AddEmissionSources(sim_iri, ship_iri);
    }
    
    public void testGetMainStation() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	DispSimSparql.GetMainStation(sim_iri);
    }
    
    public void testGetSubStations() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	DispSimSparql.GetSubStations(sim_iri);
    }
    
    public void testGetEmissionSources() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	DispSimSparql.GetEmissionSources(sim_iri);
    }
    
    public void testGetNumSubstations() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	DispSimSparql.GetNumSubStations(sim_iri);
    }
    
    public void testInitService() {
    	String service_iri = episode_iri;
    	String httpURL = "http://localhost:8080/JPS_VIRTUALSENSOR/EpisodeAgent";
    	DispSimSparql.InitService(service_iri, httpURL);
    }
    
    public void testGetServiceURL() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
    	DispSimSparql.GetServiceURL(sim_iri);
    }
    
    public void testGetSimCRS() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	DispSimSparql.GetSimCRS(sim_iri);
    }
    
    public void testAddOutputPath() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	String dataPath = QueryBroker.getLocalDataPath();
    	DispSimSparql.AddOutputPath(sim_iri, dataPath,1);
    }
    
    public void testGetLatestOutputPath() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	DispSimSparql.GetLatestOutputPath(sim_iri);
    }
    
    public void testGetNumOutput() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	DispSimSparql.GetNumOutput(sim_iri);
    }
}
