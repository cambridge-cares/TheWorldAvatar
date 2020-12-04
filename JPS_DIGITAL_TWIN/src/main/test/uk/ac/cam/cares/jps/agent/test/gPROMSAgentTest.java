package uk.ac.cam.cares.jps.agent.test;

import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class gPROMSAgentTest extends TestCase {

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
