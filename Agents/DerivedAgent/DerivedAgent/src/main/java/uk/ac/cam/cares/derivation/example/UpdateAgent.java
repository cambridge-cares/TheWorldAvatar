package uk.ac.cam.cares.derivation.example;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;

import uk.ac.cam.cares.derivation.config.Config;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = {UpdateAgent.URL_UpdateTimeDuration, UpdateAgent.URL_UpdateMinTimeCalc, UpdateAgent.URL_UpdateMaxTimeCalc})
public class UpdateAgent extends JPSAgent{
	/**
	 * 
	 */
	public static final String URL_UpdateTimeDuration = "/UpdateTimeDuration";
	public static final String URL_UpdateMinTimeCalc = "/UpdateMinTimeCalc";
	public static final String URL_UpdateMaxTimeCalc = "/UpdateMaxTimeCalc";
	private static final long serialVersionUID = 1L;

	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		DerivationClient devClient = new DerivationClient(storeClient);
		devClient.updateDerivation(InstancesDatabase.DerivedQuantityTimeDuration);
    	devClient.updateDerivation(InstancesDatabase.DerivedQuantityTimeDuration);
		return requestParams;	
	}
}
