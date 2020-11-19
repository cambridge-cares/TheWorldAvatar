package uk.ac.cam.cares.jps.ship.shipdata.test;

import java.util.List;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.ship.shipdata.ShipSparql;

public class ShipSparqlTest {
    /**
     * If you want to create new ship instances, replace csvPath with the following:
     * AgentLocator.getPathToWorkingDir(this) + "/ship_latest_consolidated.csv"
     * The reason that this test is written to *not* run out of the box is to prevent people from uploading
     * duplicate triples to the CMCL ship triple-store. Unlike RDF4J, Blazegraph does not check whether a triple 
     * already exists in the endpoint and proceeds to add them in anyway.
     * After changing the csvPath, make sure you change the ship_endpoint in ShipSparql to an endpoint that
     * you want to run this test.
     */
    @Test
    public void testCreateShip () {
        String csvPath = "D:\\JPS\\data\\ship data\\ship_latest_consolidated.csv";
        String csvFile=new QueryBroker().readFileLocal(csvPath);
        List<String[]> csv_array = MatrixConverter.fromCsvToArray(csvFile);
        int mmsi, al, aw, ts, tst; double ss, cu, lat, lon; String type;
        ShipSparql sparql = new ShipSparql();

        for (int i = 1; i < csv_array.size(); i++) {
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

            if (ts > tst) {
                sparql.createShip(i,mmsi,type,al,aw,ss,cu,lat,lon,ts);
            } else {
                sparql.createShip(i,mmsi,type,al,aw,ss,cu,lat,lon,tst);
            }
        }
    }

    @Test
    public void testQuery() {
        JSONObject jo = new JSONObject();
        Region.putRegion(jo, 2);
        
        Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
        
        ShipSparql sparql = new ShipSparql();
        sparql.queryShipWithinScope(sc);
    }

    @Test
    public void testClearEndpoint() {
        ShipSparql ss = new ShipSparql();
        ss.clearEndpoint();
    }
}
