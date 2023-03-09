package uk.ac.cam.cares.jps.agent.test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/** To test the execution of gPROMS agent */
public class Matlab_test  {

  public static final String TEST_FILE =
      "C:\\JParkSimulator-git\\JPS_DIGITAL_TWIN\\src\\test\\resources\\matlab.csv";

  @Ignore("Integration test that requires setup of a tomcat server running different agents as well as Matlab installed on the machine")
  @Test
  public void testFCQueryAgent() {
    JSONObject jo = new JSONObject();
    File dest = new File(System.getProperty("user.home") + "\\matlab\\matlab.csv");
    File file = new File(TEST_FILE);
    try {
      Files.copy(file.toPath(), dest.toPath());
    } catch (IOException ioe) {
      throw new JPSRuntimeException(ioe.getMessage());
    }
    try {
      String resultStart =
          AgentCaller.executeGetWithJsonParameter("ElChemoAgent/JPSMatlabAgent", jo.toString());
      System.out.println(resultStart);

    } catch (Exception e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }
}
