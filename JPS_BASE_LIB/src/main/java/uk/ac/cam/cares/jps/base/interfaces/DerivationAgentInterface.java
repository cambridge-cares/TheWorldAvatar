package uk.ac.cam.cares.jps.base.interfaces;

import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;

/**
 * This interface to be implemented by DerivationAgent.
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */

public interface DerivationAgentInterface {
	/**
	 * Shall implement logic transforming derivationInputs to derivationOutputs.
	 * 
	 * @param derivationInputs
	 * @param derivationOutputs
	 * @return
	 */
	void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs);

	/**
	 * Shall implement input validation logic.
	 * 
	 * @param requestParams
	 * @return
	 * @throws BadRequestException
	 */
	boolean validateInput(JSONObject requestParams) throws BadRequestException;

	/**
	 * Shall implement logic to monitor the asynchronous derivation that
	 * isDerivedUsing the instantiated agent.
	 * 
	 * @param agentIRI
	 * @param periodicalTimescale
	 */
	void monitorAsyncDerivations(String agentIRI, long periodicalTimescale);
}
