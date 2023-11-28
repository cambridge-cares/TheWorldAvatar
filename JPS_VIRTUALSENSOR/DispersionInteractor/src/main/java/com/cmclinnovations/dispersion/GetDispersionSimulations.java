package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.entity.ContentType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { "/GetDispersionSimulations" })
public class GetDispersionSimulations extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GetDispersionSimulations.class);
    private QueryClient queryClient;
    RemoteRDBStoreClient remoteRDBStoreClient;

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) {
        LOGGER.info("Received GET request to obtain list of dispersion simulations.");

        JSONObject results = new JSONObject();

        List<DispersionSimulation> dispersionSimulations = queryClient.getDispersionSimulations();
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            queryClient.removeNonExistentPollutantsAndSetSimTimes(dispersionSimulations, conn);
        } catch (SQLException e) {
            String errmsg = "Probably error in closing connection while getting dispersion outputs";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
        }

        dispersionSimulations.forEach(
                dispersionSimulation -> results.put(dispersionSimulation.getLabel(), dispersionSimulation.toJson()));

        try {
            resp.getWriter().print(results);
            resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
            resp.setCharacterEncoding("UTF-8");
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to write HTTP response");
        } catch (JSONException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to create JSON object for HTTP response");
        }
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        TimeSeriesClient<Instant> tsClientInstant = new TimeSeriesClient<>(storeClient,
                Instant.class);
        queryClient = new QueryClient(storeClient, remoteRDBStoreClient, tsClient, tsClientInstant);
        queryClient.setOntopUrl(endpointConfig.getOntopUrl());
    }
}
