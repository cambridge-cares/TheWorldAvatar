package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * only works with ship input agent with live data enabled
 */
@WebServlet(urlPatterns = { "/RunDispersionDefinedInterval" })
public class RunDispersionDefinedInterval extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(RunDispersionDefinedInterval.class);
    private QueryClient queryClient;
    private DerivationClient devClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String ewkt = req.getParameter("ewkt");
        int nx = Integer.parseInt(req.getParameter("nx"));
        int ny = Integer.parseInt(req.getParameter("ny"));
        String scopeLabel = req.getParameter("label");
        String[] zArray = req.getParameterValues("z");
        String simulationTimeIri = req.getParameter("simulationTimeIri"); // optional
        String startTime = req.getParameter("startTime");
        String intervalMinutes = req.getParameter("intervalMinutes");
        String numSteps = req.getParameter("numSteps");
        String maxTimeMinutes = req.getParameter("maxTimeMinutes");

        long startTimeLong = Long.parseLong(startTime);
        Instant startTimeInstant = Instant.ofEpochSecond(startTimeLong);

        int numStepsInt = Integer.parseInt(numSteps);
        int maxTimeInt = Integer.parseInt(maxTimeMinutes);

        int interval;
        if (intervalMinutes != null) {
            interval = Integer.parseInt(intervalMinutes);
        } else {
            interval = 60;
        }
        LOGGER.info("Running simulations starting at {} at interval of {} minutes", startTimeInstant, interval);

        InitialiseSimulation initialiseSimulation = new InitialiseSimulation();

        LOGGER.info("Creating a new dispersion instance if it does not already exist");
        String derivation = initialiseSimulation.createSimulation(ewkt, nx, ny, null, scopeLabel, zArray,
                simulationTimeIri);

        LOGGER.info("Starting dispersion simulations for <{}>", derivation);

        Instant currentTimestep = startTimeInstant;
        for (int i = 0; i < numStepsInt; i++) {
            LOGGER.info("Time = {}", currentTimestep);
            LOGGER.info("Updating simulation time");
            queryClient.updateSimulationTime(derivation, currentTimestep.getEpochSecond());

            LOGGER.info("Sending request to update derivation");

            Thread thread = new Thread(() -> devClient.updatePureSyncDerivation(derivation));
            try {
                thread.start();
                thread.join(maxTimeInt * 60 * 1000); // convert to milliseconds
            } catch (Exception e) {
                LOGGER.error("Problem executing timestep = {}", currentTimestep);
                LOGGER.error(e.getMessage());
            }

            if (thread.isAlive()) {
                LOGGER.warn("Process took longer than 30 minutes, proceeding to the next run");
                thread.interrupt();
            }

            currentTimestep = currentTimestep.plus(interval, ChronoUnit.MINUTES);
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
