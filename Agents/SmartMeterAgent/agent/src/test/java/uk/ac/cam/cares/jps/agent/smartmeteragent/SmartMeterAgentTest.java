package uk.ac.cam.cares.jps.agent.smartmeteragent;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

public class SmartMeterAgentTest {

    @Test
    public void tempTest() {
        SmartMeterAgentLauncher agent = new SmartMeterAgentLauncher();
        JSONObject requestParams = new JSONObject()
                                .put("dataSource", "database")
                                .put("dataRequired", "historical")
                                .put("microgrid", "http://localhost:48888/microgrid")
                                .put("dataBefore", "2023-03-28 18:19:00")
                                .put("dataAfter", "2023-03-28 18:01:00");
        JSONObject result = agent.processRequestParameters(requestParams);
        return;
    }

    @Test
    public void tempTest2() throws IOException {
        SmartMeterAgentLauncher agent = new SmartMeterAgentLauncher();
        JSONObject requestParams = new JSONObject()
                                .put("dataSource", "csv")
                                .put("dataRequired", "historical")
                                .put("microgrid", "http://localhost:48888/microgrid")
                                .put("dataBefore", "2022-11-09 21:04:00")
                                .put("dataAfter", "2022-11-09 21:09:00");
        JSONObject result = agent.processRequestParameters(requestParams);
        return;
    }

    @Test
    public void testValidateResult() {
        List<String> devices = new ArrayList<String>();
        JSONArray resultArray = new JSONArray();
        JSONObject result1 = new JSONObject()
                            .put("time", "2023-03-28 18:19:00")
                            .put("Pd", "0.01")
                            .put("device", "Load")
                            .put("current", "0.02")
                            .put("voltage", "0.03")
                            .put("frequency", "0.04");
        JSONObject result2 = new JSONObject()
                            .put("time", "2023-03-28 18:19:00")
                            .put("Pd", "0.01")
                            .put("device", "Load")
                            .put("current", "0.02")
                            .put("voltage", "0.03")
                            .put("frequency", "0.04");
        JSONObject result3 = new JSONObject()
                            .put("time", "2023-03-28 18:20:00")
                            .put("Pd", "0.01")
                            .put("device", "PV")
                            .put("current", "0.02")
                            .put("voltage", "0.03")
                            .put("frequency", "0.04");
        devices.add("Load");
        devices.add("PV");
        SmartMeterAgent smAgent = new SmartMeterAgent();
        // When not all devices have readings retrieved
        resultArray.put(result1);
        resultArray.put(result2);
        assertFalse(smAgent.validateResult(devices, resultArray));
        // When readings retrieved have different time
        resultArray.remove(1);
        resultArray.put(result3);
        assertFalse(smAgent.validateResult(devices, resultArray));
    }

    @Test
    public void testProcessCsvReadings() {
        String[] reading = {"1", "2023-03-24 14:41:16.000000", "0", "0", "POWERMETER", "2023-03-24 14:41:16.000000", 
        "1.1", "1.2", "1.3", "", "","3.2", "3.4", "3.6", "226.1", "227.1", 
        "228.1", "", "", "", "393.37439", "394.527283", "393.128021", "49.99", "49.99", "49.99", 
        "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "Grid"};
        SmartMeterAgent smAgent = new SmartMeterAgent();
        JSONObject actualResult = smAgent.processCsvReadings(reading);

        assertEquals("2023-03-24T14:41:00+00:00", actualResult.getString("time"));
        assertEquals("Grid", actualResult.getString("device"));
        assertEquals(1.2, actualResult.getDouble("Pd"), 0.1);
        assertEquals(3.4, actualResult.getDouble("current"), 0.1);
        assertEquals(227.1, actualResult.getDouble("voltage"), 0.1);
        assertEquals(49.99, actualResult.getDouble("frequency"), 0.1);
    }
}
