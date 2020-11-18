package uk.ac.cam.cares.jps.ship.shipdata;

import javax.servlet.annotation.WebServlet;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;

@WebServlet(urlPatterns = {"/ShipDataAgent"})
public class ShipDataAgent extends JPSAgent {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final String KEY_COLLECTION = "collection";
    private static final String KEY_ITEMS = "items";

	@Override 
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject responseParams = new JSONObject();
        Scope sc = new Scope(requestParams.getJSONObject(Region.keyRegion));
        ShipSparql ss = new ShipSparql();
        JSONArray result = ss.queryShipWithinScope(sc);
        return responseParams;
    }
}
