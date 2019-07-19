package uk.ac.cam.cares.jps.thermo.manager;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;


/**
 * @author nk510
 * The Class ExecutorManager.
 */
public class ExecutorManager {

	/**
	 * 
	 * @author nk510
	 * @param executorService The executor service. Shutdown executor service. 
	 * 
	 */
	public void shutdownExecutorService(ExecutorService executorService) {

		executorService.shutdown();

		try {

			if (!executorService.awaitTermination(1, TimeUnit.SECONDS)) {
				executorService.shutdownNow();
			}
		} catch (InterruptedException ie) {

			executorService.shutdownNow();

			
			Thread.currentThread().interrupt();
		}
	}
	
}
