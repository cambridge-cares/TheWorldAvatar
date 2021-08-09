package uk.ac.cam.cares.derivation.example;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;

import uk.ac.cam.cares.derivation.config.Config;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = {"/UpdateAgent"})
public class UpdateAgent extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		DerivationClient devClient = new DerivationClient(storeClient);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		
		if (InstancesDatabase.DerivedQuantityDifference == null) {
			InstancesDatabase.DerivedQuantityDifference = sparqlClient.getDerivationOfCalculatedDifference();
		}
		
		devClient.updateDerivation(InstancesDatabase.DerivedQuantityDifference);
		return requestParams;
	}
}
