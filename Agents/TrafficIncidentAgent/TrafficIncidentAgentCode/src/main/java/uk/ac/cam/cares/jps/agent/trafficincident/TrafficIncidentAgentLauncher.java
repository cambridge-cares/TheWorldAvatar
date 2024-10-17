package uk.ac.cam.cares.jps.agent.trafficincident;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.json.JSONObject;

// curl <IP_ADDRESS>/traffic-incident-agent/start
@WebServlet(urlPatterns = {"/start"})
public class TrafficIncidentAgentLauncher extends JPSAgent {
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        JSONObject jsonMessage = new JSONObject();

        ScheduledExecutorService scheduledThreadPool = Executors.newScheduledThreadPool(2);

        // perform data extraction for every two minutes
        scheduledThreadPool.scheduleAtFixedRate(new TrafficIncidentAgent(), 0, 1, TimeUnit.MINUTES);
    
        return jsonMessage;
    }
}