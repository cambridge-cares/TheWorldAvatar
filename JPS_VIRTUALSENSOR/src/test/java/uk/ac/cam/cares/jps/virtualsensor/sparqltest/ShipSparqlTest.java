package uk.ac.cam.cares.jps.virtualsensor.sparqltest;

import java.util.List;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.virtualsensor.objects.Chimney;
import uk.ac.cam.cares.jps.virtualsensor.objects.Ship;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

public class ShipSparqlTest extends TestCase {
    /**
     * This script might go into an infinite loop if you have not set things up correctly because of the
     * while - try - catch loop at the end, it's done like that because sometimes the connection to the 
     * endpoint fails for no apparent reason. 
     * Comment it out first and test if it runs before using it to upload tons of ship data.
     * @throws InterruptedException 
     */
    public void testCreateShip () throws InterruptedException {
        String csvPath = AgentLocator.getPathToWorkingDir(this) + "/ship_latest_consolidated_plymouth.csv";
        String csvFile=new QueryBroker().readFileLocal(csvPath);
        List<String[]> csv_array = MatrixConverter.fromCsvToArray(csvFile);
        int mmsi, al, aw, ts, tst; double ss, cu, lat, lon; String type;
        boolean success;
        for (int i = 1; i < csv_array.size(); i++) {
        	success = false;
            mmsi = Integer.parseInt(csv_array.get(i)[0]);
            type = csv_array.get(i)[1];
            al = Integer.parseInt(csv_array.get(i)[2]);
            aw = Integer.parseInt(csv_array.get(i)[3]);
            ss = Double.parseDouble(csv_array.get(i)[4]);
            cu = Double.parseDouble(csv_array.get(i)[5]);
            lat = Double.parseDouble(csv_array.get(i)[6]);
            lon = Double.parseDouble(csv_array.get(i)[7]);

            // ts and tst are timestamps from different websites
            try {
                ts = Integer.parseInt(csv_array.get(i)[8]);
            } catch (NumberFormatException e) {
                ts = -1;
            }

            try {
                tst = Integer.parseInt(csv_array.get(i)[9]);
            } catch (NumberFormatException e) {
                tst = -1;
            }

            while (!success) {
            	try {
            		if (ts > tst) {
            			ShipSparql.createShip(i,mmsi,type,al,aw,ss,cu,lat,lon,ts);
                    } else {
                    	ShipSparql.createShip(i,mmsi,type,al,aw,ss,cu,lat,lon,tst);
                    }
            		success = true;
            	} catch (JPSRuntimeException e) {
            		Thread.sleep(1000);
            	}
            }
        }
    }
    
    public void testQueryShipIRI() {
    	JSONObject jo = new JSONObject();
        Region.putRegion(jo, 5);
        
        Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
        ShipSparql.GetShipIriWithinScope(sc);
    }
    
    public void testQueryShipProperties() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	ShipSparql.queryShipProperties(ship_iri);
    }

    public void testShipObject() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	new Ship(ship_iri,true);
    	new Ship(ship_iri,false);
    }
    
    public void testChimneyObject() {
		String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	Chimney chim2 = new Chimney(ship_iri);
    }
    
    public void testUpdateChimney() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	JSONObject request = new JSONObject();
    	request.put("shipIRI",ship_iri);
    	JSONObject result = new JSONObject(AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/SpeedLoadMapAgent", request.toString()));
    	Chimney chim = new Chimney(result);
    	ShipSparql.UpdateShipChimney(ship_iri, chim);
    }
    
    public void testQueryShipCoordinates() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship2";
    	ShipSparql.queryShipCoordinates(ship_iri);
    }
    
    public void testQueryChimneyProperties() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	ShipSparql.QueryChimneyProperties(ship_iri);
    }
    
    public void testQueryParticleIRI() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	ShipSparql.QueryParticleIRI(ship_iri);
    }
}
