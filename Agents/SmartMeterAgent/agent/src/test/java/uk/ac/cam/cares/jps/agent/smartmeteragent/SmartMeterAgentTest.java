package uk.ac.cam.cares.jps.agent.smartmeteragent;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

public class SmartMeterAgentTest {

    @Test
    public void tempTest() {
        SmartMeterAgent agent = new SmartMeterAgent();
        JSONObject requestParams = new JSONObject()
                                .put("dataSource", "database")
                                .put("dataRequired", "latest")
                                .put("microgrid", "http://localhost:48888/microgrid")
                                .put("dataBefore", "2022-11-08 12:33:00");
        JSONObject result = agent.processRequestParameters(requestParams);
        return;
    }

}
