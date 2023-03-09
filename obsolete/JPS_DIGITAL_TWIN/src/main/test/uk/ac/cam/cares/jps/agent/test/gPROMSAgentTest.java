package uk.ac.cam.cares.jps.agent.test;

import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/** To test the execution of gPROMS agent */
public class gPROMSAgentTest  {

  @Ignore("Integration test that requires setup of a tomcat server running different agents as well as Matlab and gPROMS installed on the machine")
  @Test
  public void testFCQueryAgent() {
    JSONObject jo = new JSONObject();
    try {
      String resultStart =
          AgentCaller.executeGetWithJsonParameter("ElChemoAgent/job/request", jo.toString());
      System.out.println(resultStart);

    } catch (Exception e) {
      throw new JPSRuntimeException(e.getMessage());
    }
    try {
      String resultStart =
          AgentCaller.executeGetWithJsonParameter("ElChemoAgent/job/statistics", jo.toString());
      System.out.println(resultStart);

    } catch (Exception e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }
}
