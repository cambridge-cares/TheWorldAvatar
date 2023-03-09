package uk.ac.ceb.como.molhub.model;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import org.apache.log4j.Logger;



/**
 * 
 * The Class ExecutorManager.
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 *  
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
	 * @param commandLine command line that contains Python run command, Log file path, arguments, and output folder path where generated OWL file will be saved. 
	 * @throws IOException the IO exception.
	 * @throws InterruptedException the interrupted exception.
	 * 
	 * Runs parser implemented in Python by using command line on Windows machine. 
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
	    boolean isErrorStreamReady = false;
	    boolean isProcessAlive = false;

	    boolean outputFlag = true;
	    boolean errorFlag = true;
	    
	    logger.info("Reads command line:  ");
	    
	    while (process.isAlive()) {

	        do {
	        	
	            isInputStreamReady = inputStream.ready();
	            
	            logger.info("is output stream ready: " + isInputStreamReady);
	            
	            outputFlag = true;
	            errorFlag = true;

	            if (isInputStreamReady) {
	                
	            	inputStreamCommandLine = inputStream.readLine();
	                
	                outputFlag = false;
	            
	                logger.info("input stream command line: " + inputStreamCommandLine + lineSeparator + " output flag: " + outputFlag);
	            }
	            
	            isErrorStreamReady = errorStream.ready();
	            
	            if (isErrorStreamReady) {
	            	
	                inputStreamCommandLine = errorStream.readLine();
	                
	                errorFlag = false;
	                
	                
	                logger.info("error stream: " + inputStreamCommandLine + lineSeparator + " error flag: " + errorFlag);

	            }
	            
	            isProcessAlive = process.isAlive();
	            
	            if (!isProcessAlive) {
	            	
	            	errorFlag = false;
	            	
	            	logger.info("error flag: " + errorFlag);
	            	
	                inputStreamCommandLine = null;	                
	                
	                process.waitFor(1000, TimeUnit.MILLISECONDS);
	            }

	        } while (inputStreamCommandLine != null);

	        /**
	         * If remains nothing  to read then the process waits 100 milliseconds.
	         */      
	        
	        process.waitFor(100, TimeUnit.MILLISECONDS);
	    }
	    
	    logger.info("Parser's task is completed!");
	}
	
}
