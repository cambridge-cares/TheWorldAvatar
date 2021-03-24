package uk.ac.cam.cares.jps.virtualsensor.sparqltest;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.objects.Scope;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.virtualsensor.objects.DispSim;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

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
    
    public void testInitService() {
    	String service_iri = episode_iri;
    	String httpURL = "http://localhost:8080/JPS_VIRTUALSENSOR/EpisodeAgent";
    	DispSimSparql.InitService(service_iri, httpURL);
    }
    
    public void testGetScope() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1";
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
    
    public void testGetDz() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	DispSimSparql.GetDz(sim_iri);
    }
    
    public void testGetNx() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	DispSimSparql.GetNx(sim_iri);
    }
    
    public void testGetNy() {
    	String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
    	DispSimSparql.GetNy(sim_iri);
    }
    
    public void testGetNumSim() {
    	DispSimSparql.GetNumSim();
    }
    
    public void testCreateDispSim() { 
    	double[] centre= {-1.913913,52.803439}; 
    	double[] dimension = {20*1e3,20*1e3}; // [x,y]
    	Scope sc = DispSimSparql.createScopeEpisode(centre, dimension);
    	
    	//create 1 dummy ship at the centre
        int mmsi, al, aw, ts; double ss, cu, lat, lon; String type;
        for (int i = 1; i < 2; i++) {
            mmsi = 1;
            type = "unknown type";
            al = 37;
            aw = 8;
            ss = 0.1;
            cu = 220.2;
            lat = centre[1];
            lon = centre[0];
            ts = 1;
            int shipindex = ShipSparql.GetNumShips() + 1;
            ShipSparql.createShip(shipindex,mmsi,type,al,aw,ss,cu,lat,lon,ts);
        }
        
        // create dummy weather stations
        // create main station at the centre
        int numstn = SensorSparql.GetNumWeatherStation();
        double stnhgt = 10; // fixed height for now
        double[] xyz_main = {centre[0],centre[1],stnhgt};
        SensorSparql.createWeatherStation(numstn+1, xyz_main);
        
        // create sub station 100m away from bottom corner
        double[] xy_sub = {sc.getLowerCorner().getX()+100 ,sc.getLowerCorner().getY()+100};
        xy_sub = CRSTransformer.transform(sc.getSrsName(), CRSTransformer.EPSG_4326, xy_sub);
        double[] xyz_sub = {xy_sub[0],xy_sub[1],stnhgt};
        SensorSparql.createWeatherStation(numstn+2, xyz_sub);
        
        double[] dz = {10,10,15,25,40,100,300,500,500,500,500,500,500};
        DispSim sim = new DispSim();
    	sim.setScope(sc);
    	sim.setNx(10);
    	sim.setNy(10);
    	sim.setNumSubStations(1);
    	sim.setServiceAgent(episode_iri);
    	sim.setSimCRS(sc.getSrsName());
    	sim.setDz(dz);
    	DispSimSparql.InitSim(DispSimSparql.GetNumSim()+1, sim);
    }
}
