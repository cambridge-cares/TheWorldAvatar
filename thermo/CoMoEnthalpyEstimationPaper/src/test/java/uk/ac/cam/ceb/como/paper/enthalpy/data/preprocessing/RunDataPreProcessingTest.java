package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

/**
 *
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * Runs all tests implemented in DataPreProcessingTest.
 *
 */

public class RunDataPreProcessingTest {

	public static void main(String[] args) {
		
		Result testResult = JUnitCore.runClasses(DataPreProcessingTestSuite.class);
		
		for(Failure failure : testResult.getFailures()){
		
		System.out.println(failure.toString());
		
		}
		
        System.out.println("Junit test result was successful: " + testResult.wasSuccessful());
	}
}