package uk.ac.cam.cares.jps.config.test;

import static org.junit.Assert.*;

import org.apache.http.client.utils.URIBuilder;
import org.junit.Test;

public class TestADMSAgent {

	@Test
	public void test() {

 
		String myHost = "localhost";
		int myPort = 8080;
		String ADMSAgentPath = "/JPS/ADMSAgent";
		
		String testADMSInput = "";
		
		
		URIBuilder builderGenerateInput = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(ADMSAgentPath)
				.setParameter("value", testADMSInput);
			
	
	
	}

}
