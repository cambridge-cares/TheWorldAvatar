package com.cmclinnovations.dispersion;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * updates all instances of SimulationTime to the given value
 * assumes that there is only one simulation instance for now (Plymouth)
 */
@WebServlet(urlPatterns = {"/UpdateSimulationTime"})
public class UpdateSimulationTime extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(UpdateSimulationTime.class);
    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        Long simTime = null;
        try {
            simTime = Long.parseLong(req.getParameter("simTime"));
        } catch (NumberFormatException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to parse simulation time, it should be an integer");
        }
        
        if (simTime != null) {
            EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
            RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
            QueryClient queryClient = new QueryClient(storeClient);
    
            queryClient.updateSimulationTime(simTime);
        }
    }
}
