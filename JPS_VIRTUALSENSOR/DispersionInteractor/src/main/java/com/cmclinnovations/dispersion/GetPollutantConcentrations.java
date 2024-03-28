package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.entity.ContentType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.postgis.Point;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { "/GetPollutantConcentrations" })
public class GetPollutantConcentrations extends HttpServlet {
    QueryClient queryClient;
    RemoteRDBStoreClient remoteRDBStoreClient;
    private static final Logger LOGGER = LogManager.getLogger(GetPollutantConcentrations.class);

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        double lat = Double.parseDouble(req.getParameter("lat"));
        double lon = Double.parseDouble(req.getParameter("lon"));

        Point location = new Point(lon, lat);
        location.setSrid(4326);

        List<String> scopes = queryClient.getScopesIncludingPoint(location);

        if (scopes.isEmpty()) {
            resp.setStatus(500);
            resp.getWriter().print("There is no dispersion data within the specified location");
            resp.setCharacterEncoding("UTF-8");
        } else {
            if (scopes.size() > 1) {
                LOGGER.info("More then one scope detected at the specified point: {}", scopes);
                LOGGER.info("Picking <{}>", scopes.get(0));
            }

            List<String> dispersionOutputs = queryClient.getOutputsAtLowestHeight(scopes.get(0));
            Map<String, String> pollutantToDispRaster = queryClient.getDispersionRasterIris(dispersionOutputs);

            JSONObject overallResult = new JSONObject();
            try (Connection conn = remoteRDBStoreClient.getConnection()) {
                overallResult = queryClient.getConcentrationsFromRaster(pollutantToDispRaster, location,
                        conn);
            } catch (SQLException e) {
                LOGGER.error("Error while getting concentrations from raster");
                LOGGER.error(e.getMessage());
                resp.setStatus(500);
            }
            resp.getWriter().print(overallResult);
            resp.setCharacterEncoding("UTF-8");
            resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
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
    }
}
