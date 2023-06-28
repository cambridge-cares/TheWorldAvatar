package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { "/CreateVirtualSensors" })
public class CreateVirtualSensors extends HttpServlet {

    private static final Logger LOGGER = LogManager.getLogger(CreateVirtualSensors.class);
    private QueryClient queryClient;
    private DispersionPostGISClient dispersionPostGISClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {

        LOGGER.info("Received POST request to create new virtual sensors. ");

        // Check that at least one scope has been initialized
        try (Connection conn = dispersionPostGISClient.getConnection()) {
            if (!dispersionPostGISClient.tableExists(Config.SCOPE_TABLE_NAME, conn)) {
                LOGGER.error("At least one scope needs to be initialized by" +
                        "sending a POST request to InitialiseSimulation before any virtual sensors can be created. ");
                return;
            }

        } catch (SQLException e) {
            LOGGER.error("SQL state {}", e.getSQLState());
            LOGGER.error(e.getMessage());
            LOGGER.error("Probably failed to close SQL connection or failed to connect");
        }

        String[] virtualSensorLocationsString = req.getParameterValues("virtualSensorLocations");
        List<Point> virtualSensorLocations = new ArrayList<>();

        // Assuming that coordinates of all virtual sensor locations are in EPSG:4326
        // (latitude, longitude)
        // because their locations will be checked against the entries of the scopes
        // table later.

        Arrays.stream(virtualSensorLocationsString).forEach(p -> {
            try {
                virtualSensorLocations.add(new Point(p));
            } catch (SQLException e) {
                LOGGER.error(e.getMessage());
                LOGGER.error("Could not parse location of virtual sensor {}", p);
                return;
            }
        });

        // Retrieve the iri of the scope containing the virtual sensor location for each
        // virtual sensor.
        // For now, each virtual sensor location is only allowed to lie within one
        // scope.
        // Need to check if there is already a virtual sensor at any of the input
        // locations. If so, the existing sensor should not be duplicated.
        List<String> vsScopeList = new ArrayList<>();
        List<Point> vsLocationsInScope = new ArrayList<>();

        try (Connection conn = dispersionPostGISClient.getConnection()) {
            for (int i = 0; i < virtualSensorLocations.size(); i++) {
                Point vsLocation = virtualSensorLocations.get(i);
                List<String> scopeIriList = new ArrayList<>();
                scopeIriList = queryClient.getScopesIncludingPoint(vsLocation);
                if (scopeIriList.size() == 1 && !dispersionPostGISClient.sensorExists(vsLocation, conn)) {
                    vsScopeList.add(scopeIriList.get(0));
                    vsLocationsInScope.add(vsLocation);
                } else if (scopeIriList.isEmpty()) {
                    LOGGER.warn(" The specified virtual sensor location " +
                            "at {} does not fall within any existing scope. No sensor will be created at this location.",
                            vsLocation.toString());
                } else if (scopeIriList.size() > 1) {
                    LOGGER.warn(
                            " The specified virtual sensor location at {} is contained within more than one scope polygon."
                                    +
                                    " No sensor will be created at this location.",
                            vsLocation.toString());
                }
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        if (!vsLocationsInScope.isEmpty())
            queryClient.initializeVirtualSensors(vsScopeList, vsLocationsInScope);

    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());

        dispersionPostGISClient = new DispersionPostGISClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        TimeSeriesClient<Instant> tsClientInstant = new TimeSeriesClient<>(storeClient,
                Instant.class);
        queryClient = new QueryClient(storeClient, remoteRDBStoreClient, tsClient, tsClientInstant);
        queryClient.initialiseVirtualSensorAgent();
    }

}
