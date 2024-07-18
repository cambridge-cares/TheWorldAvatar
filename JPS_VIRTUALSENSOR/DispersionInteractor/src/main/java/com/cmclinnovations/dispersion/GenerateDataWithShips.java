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

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * combination of UpdateShipsAndSimulationTime & TriggerUpdateDispersion
 */
@WebServlet(urlPatterns = { "/GenerateDataWithShips" })
public class GenerateDataWithShips extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GenerateDataWithShips.class);
    private QueryClient queryClient;
    private DerivationClient devClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        HttpPost httpPost = new HttpPost(Config.SHIP_INPUT_AGENT);
        long timestamp;

        int numsteps = Integer.parseInt(req.getParameter("numsteps"));
        String derivation = req.getParameter("derivation");

        LOGGER.info("Recevied POST request to generate dispersion data for {} timesteps", numsteps);

        for (int i = 0; i < numsteps; i++) {
            try (CloseableHttpClient httpClient = HttpClients.createDefault();
                    CloseableHttpResponse response = httpClient.execute(httpPost);) {
                JSONObject json = new JSONObject(EntityUtils.toString(response.getEntity()));
                timestamp = json.getLong("averageTimestamp"); // key defined in ShipInputAgent
            } catch (JSONException | IOException | ParseException e) {
                LOGGER.error(e.getMessage());
                LOGGER.error("Error from executing request to ship input agent");
                return;
            }

            queryClient.updateSimulationTime(derivation, timestamp);
            devClient.updatePureSyncDerivation(derivation);
        }
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClient = new QueryClient(storeClient, null, null);
        devClient = new DerivationClient(storeClient, QueryClient.PREFIX);
    }
}
