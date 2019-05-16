package uk.cam.cares.jps.postgresql.test;

import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestPOSTGRESQL extends TestCase  {
	

	
	public void testextractdata() {
		double xmin=11560879/*.832*/;
		double ymin=140107/*.739*/;
		double xmax=11563323/*.926*/;
		double ymax=143305/*.896*/;
		
		 xmin=12706630.262/*.832*/;
		 ymin=2545539.172/*.739*/;
		 xmax=12708200.45/*.926*/;
		 ymax=2546850.028/*.896*/;
		   JSONObject jo = new JSONObject();
		   
		   JSONObject scope = new JSONObject();
		   JSONObject low = new JSONObject();
		   JSONObject up = new JSONObject();
		   up.put("upperx", xmax);
		   up.put("uppery", ymax);
		   low.put("lowerx", xmin);
		   low.put("lowery", ymin);
		   scope.put("lowercorner", low);
		   scope.put("uppercorner", up);
		   jo.put("region",scope);
		   
		   
		   
			JSONWriter jsonInput = new JSONStringer().object().
					key("region").object()
						.key("srsname").value("EPSG:3857")
						.key("lowercorner").object()
							.key("lowerx").value(xmin)
							.key("lowery").value(ymin).endObject()
						.key("uppercorner").object()
							.key("upperx").value(xmax)
							.key("uppery").value(ymax).endObject()
					.endObject()
					.key("reactionmechanism").value("http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001")
					.endObject(); 
		   
		   System.out.println("json="+jo.toString());
		   //System.out.println("json="+jsoninput.toString());
		   
		   String result = AgentCaller.executeGetWithJsonParameter("/JPS_POSTGRESQL/getEntitiesWithinRegion", jo.toString());

		   System.out.println("result of the ship= "+result);
	}
}
