package com.cmclinnovations.dispersion;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.ParseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * calls ship input agent to add 1 timestep worth of data
 * get the average value of the timestamp of added
 * updates *all* simulation time of dispersion derivations to the average value
 */
@WebServlet(urlPatterns = { "/UpdateShipsAndSimulationTime" })
public class UpdateShipsAndSimulationTime extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(UpdateShipsAndSimulationTime.class);
    private QueryClient queryClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        LOGGER.info("Recevied POST request to update ships and simulation time for dispersion");
        HttpPost httpPost = new HttpPost(Config.SHIP_INPUT_AGENT);
        CloseableHttpResponse response;
        long timestamp;

        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            response = httpClient.execute(httpPost);
            JSONObject json = new JSONObject(EntityUtils.toString(response.getEntity()));
            timestamp = json.getLong("averageTimestamp"); // key defined in ShipInputAgent
        } catch (JSONException | IOException | ParseException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Error from executing request to ship input agent");
            return;
        }

        queryClient.updateSimulationTime(timestamp);
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClient = new QueryClient(storeClient, null, null);
    }
}
