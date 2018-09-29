package uk.ac.ceb.como.molhub.model;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class ExecutorManager {

	/**
	 * @author nk510
	 * @param executorService
	 * 
	 *            Shutdown executor.
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
