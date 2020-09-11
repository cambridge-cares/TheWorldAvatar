package uk.ac.ceb.como.molhub.model;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import org.apache.log4j.Logger;

import uk.ac.ceb.como.molhub.action.UploadLogAction;

/**
 * @author nk510
 * The Class ExecutorManager.
 */
public class ExecutorManager {
	
	final static Logger logger = Logger.getLogger(ExecutorManager.class.getName());
	/**
	 * 
	 *
	 * @author nk510
	 * @param executorService
	 * <p>shutdown executor service.</p>
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
	
	
	/**
	 * @param commandLine command line that contains python run command, log file path, arguments, and output folder path where generated owl file will be saved. 
	 * @throws IOException the io exception.
	 * @throws InterruptedException the interrupted exception.
	 * 
	 * Runs Python parser using command line in Windows machine. 
	 * 
	 */
	public void runParser(String commandLine) throws IOException, InterruptedException {
		
	    logger.info("command line : " + commandLine);
	    
	    Process process = Runtime.getRuntime().exec(commandLine);
	    
	    /**
	     * Input and error streams.
	     */

	    BufferedReader inputStream = new BufferedReader(new InputStreamReader(process.getInputStream()));
	    BufferedReader errorStream = new BufferedReader(new InputStreamReader(process.getErrorStream()));
	    
	    String inputStreamCommandLine = "";
	    String lineSeparator = System.getProperty("line.separator");

	    boolean isInputStreamReady = false;
	    boolean isErrorReady = false;
	    boolean isProcessAlive = false;

	    boolean outputError = true;
	    boolean errorExistance = true;


	    logger.info("Reads command line:  ");
	    
	    while (process.isAlive()) {

	        do {
	        	
	            isInputStreamReady = inputStream.ready();
	            logger.info("is output stream ready: " + isInputStreamReady);
	            
	            outputError = true;
	            errorExistance = true;

	            if (isInputStreamReady) {
	                inputStreamCommandLine = inputStream.readLine();
	                outputError = false;
	                logger.info("input stream command line: " + inputStreamCommandLine + lineSeparator);
	            }
	            
	            isErrorReady = errorStream.ready();
	            
	            if (isErrorReady) {
	                inputStreamCommandLine = errorStream.readLine();
	                errorExistance = false;
	                logger.info("error stream: " + inputStreamCommandLine + lineSeparator);

	            }
	            
	            isProcessAlive = process.isAlive();
	            
	            if (!isProcessAlive) {
	            	
	                inputStreamCommandLine = null;
	                errorExistance = false;
	                process.waitFor(1000, TimeUnit.MILLISECONDS);
	            }

	        } while (inputStreamCommandLine != null);

	        /**
	         * If remains nothing  to read then process waits 100 milliseconds.
	         */
	        logger.info("process is waiting for 100 milliseconds: " );
	        
	        process.waitFor(100, TimeUnit.MILLISECONDS);
	    }
	    
	    logger.info("Parser's task is completed!");
	}
	
}
