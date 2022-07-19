package uk.ac.cam.cares.jps.accessagent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.StoreRouter;

public class AccessHandler {

	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AccessHandler.class);
	
    public AccessHandler() {}
  
    
	/**
	 * Instantiate a store client using StoreRouter
	 * @param targetIRI
	 * @param isQuery
	 * @param isUpdate
	 * @return
	 */
	public StoreClientInterface getStoreClient(String targetIRI, boolean isQuery, boolean isUpdate) {
		try {
			StoreClientInterface storeClient = StoreRouter.getStoreClient(targetIRI, isQuery, isUpdate);
			if (storeClient == null) {
				throw new RuntimeException();
			}
			return storeClient;
		}catch (RuntimeException e) {
			LOGGER.error("Failed to instantiate StoreClient");
			throw new JPSRuntimeException("Failed to instantiate StoreClient");
		}	 
	}
	
}
