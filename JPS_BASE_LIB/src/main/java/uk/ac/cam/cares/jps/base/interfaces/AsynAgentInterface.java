package uk.ac.cam.cares.jps.base.interfaces;

import java.util.List;

import org.json.JSONObject;

/**
 * This interface to be implemented by AsynAgent.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
public interface AsynAgentInterface {
	/**
	 * Shall implement logic to monitor the derivation that isDerivedUsing the instantiated agent.
	 * @param agentIRI
	 */
    void monitorDerivation(String agentIRI);
    
    /**
     * Shall implement logic transforming request to response. The request is same as requestParams.
     * @param requestParams
     * @return
     */
	List<String> setupJob(JSONObject requestParams);
}
