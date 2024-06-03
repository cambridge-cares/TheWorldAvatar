package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.nio.file.Path;
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
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { "/CreateVirtualSensor" })
public class CreateVirtualSensor extends HttpServlet {

    private static final Logger LOGGER = LogManager.getLogger(CreateVirtualSensor.class);
    private QueryClient queryClient;
    private DispersionPostGISClient dispersionPostGISClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        LOGGER.info("Received POST request to create new virtual sensors. ");

        double lat = Double.parseDouble(req.getParameter("lat"));
        double lng = Double.parseDouble(req.getParameter("lng"));
        List<String> pollutants = List.of(req.getParameterValues("pollutant")); // pollutant IRIs to instantiate

        Point virtualSensorLocation = new Point(lng, lat);
        virtualSensorLocation.setSrid(4326);

        // Retrieve the iri of the scope containing the virtual sensor location for each
        // virtual sensor. For now, each virtual sensor location is only allowed to lie
        // within one scope. Need to check if there is already a virtual sensor at any
        // of the input locations. If so, the existing sensor should not be duplicated.
        List<String> vsScopeList = new ArrayList<>();

        try (Connection conn = dispersionPostGISClient.getConnection()) {
            List<String> scopeIriList = new ArrayList<>();
            scopeIriList = queryClient.getScopesIncludingPoint(virtualSensorLocation);
            if (scopeIriList.size() == 1 && !dispersionPostGISClient.sensorExists(virtualSensorLocation, conn)) {
                vsScopeList.add(scopeIriList.get(0));
                boolean firstTime = false;
                if (!dispersionPostGISClient.tableExists(Config.SENSORS_TABLE_NAME, conn)) {
                    firstTime = true;
                }
                queryClient.initialiseVirtualSensors(vsScopeList, virtualSensorLocation, pollutants, conn);

                // upload obda after creating table
                if (firstTime) {
                    queryClient.initialiseVirtualSensorAgent();
                    initialiseObda();
                }
            } else if (scopeIriList.isEmpty()) {
                LOGGER.warn(" The specified virtual sensor location " +
                        "at {} does not fall within any existing scope. No sensor will be created at this location.",
                        virtualSensorLocation);
            } else if (scopeIriList.size() > 1) {
                LOGGER.warn(
                        " The specified virtual sensor location at {} is contained within more than one scope polygon."
                                + " No sensor will be created at this location.",
                        virtualSensorLocation);
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

    }

    private void initialiseObda() {
        Path obdaFile = null;
        try {
            obdaFile = new ClassPathResource("sensor.obda").getFile().toPath();
        } catch (IOException e) {
            LOGGER.error("Could not retrieve virtual sensor ontop.obda file.");
            throw new RuntimeException(e);
        }
        OntopClient ontopClient = OntopClient.getInstance();
        ontopClient.updateOBDA(obdaFile);
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
    }

}
