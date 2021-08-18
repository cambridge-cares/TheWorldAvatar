package uk.ac.cam.cares.derivation.example;

import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = {"/UpdateDerivations"})
public class UpdateDerivations extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Logger LOGGER = LogManager.getLogger(UpdateDerivations.class);

	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		DerivationClient devClient = new DerivationClient(storeClient);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		
		if (InstancesDatabase.DerivedDifference == null) {
			String calcDiff = sparqlClient.getDifference();
			InstancesDatabase.DerivedDifference = devClient.getDerivationOf(calcDiff);
		}
		
		if (InstancesDatabase.DerivedAverage == null) {
			String average = sparqlClient.getAverageIRI();
			InstancesDatabase.DerivedAverage = devClient.getDerivationOf(average);
		}
		
		devClient.updateDerivation(InstancesDatabase.DerivedDifference);
		devClient.updateDerivation(InstancesDatabase.DerivedAverage);
		
		String res_msg = "Updated derivation of difference <" + InstancesDatabase.DerivedDifference + "> and derivation of average <" + InstancesDatabase.DerivedAverage + ">";
		LOGGER.info(res_msg);
		
		JSONObject response = new JSONObject();
		response.put("status", res_msg);
		return response;
	}
}
