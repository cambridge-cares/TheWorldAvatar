package uk.ac.cam.cares.jps.dispersion.sparqltest;

import java.util.List;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.dispersion.sparql.ShipSparql;

public class ShipSparqlTest {
    /**
     * This script might go into an infinite loop if you have not set things up correctly because of the
     * while - try - catch loop at the end, it's done like that because sometimes the connection to the 
     * endpoint fails for no apparent reason. 
     * Comment it out first and test if it runs before using it to upload tons of ship data.
     * @throws InterruptedException 
     */
    @Test
    public void testCreateShip () throws InterruptedException {
        String csvPath = AgentLocator.getPathToWorkingDir(this) + "/ship_latest_consolidated.csv";
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

    @Test
    public void testQuery() {
        JSONObject jo = new JSONObject();
        Region.putRegion(jo, 4);
        
        Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
        
        ShipSparql.queryShipWithinScope(sc);
    }
}
