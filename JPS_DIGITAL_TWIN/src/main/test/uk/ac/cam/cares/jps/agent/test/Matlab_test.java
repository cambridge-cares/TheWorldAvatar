package uk.ac.cam.cares.jps.agent.test;

import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class Matlab_test extends TestCase {

  public void testFCQueryAgent() {
    JSONObject jo = new JSONObject();
    try {
      String resultStart =
          AgentCaller.executeGetWithJsonParameter("ElChemoAgent/JPSMatlabAgent", jo.toString());
      System.out.println(resultStart);

    } catch (Exception e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }
}

