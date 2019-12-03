package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class RunDataPreProcessingTest {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		Result testResult = JUnitCore.runClasses(DataPreProcessingTestSuite.class);
		
		for(Failure failure : testResult.getFailures()) {
			
			System.out.println(failure.toString());
		}
		
		System.out.println(testResult.wasSuccessful());

	}

}
