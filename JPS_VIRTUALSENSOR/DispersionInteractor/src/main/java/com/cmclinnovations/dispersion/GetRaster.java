package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.io.OutputStream;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

@WebServlet(urlPatterns = { "/GetRaster" })
public class GetRaster extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GetRaster.class);
    QueryClient queryClient;

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) {
        String filename = req.getParameter("filename");
        String scopeIri = req.getParameter("scopeIri");
        byte[] rasterBytes = queryClient.getRaster(filename, scopeIri);

        try (OutputStream out = resp.getOutputStream()) {
            resp.setContentType("image/tiff");
            // Set headers for the response to prompt download
            resp.setHeader("Content-Disposition", "attachment; filename=\"image.tif\"");
            out.write(rasterBytes);
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
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient,
                new TimeSeriesRDBClientWithReducedTables<>(Long.class));
        TimeSeriesClient<Instant> tsClientInstant = new TimeSeriesClient<>(storeClient,
                new TimeSeriesRDBClientWithReducedTables<>(Instant.class));
        queryClient = new QueryClient(storeClient, remoteRDBStoreClient, tsClient, tsClientInstant);
        queryClient.setOntopUrl(endpointConfig.getOntopUrl());
    }
}
