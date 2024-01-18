package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Executors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * only works with ship input agent with live data enabled
 */
@WebServlet(urlPatterns = { "/StartScheduledDispersion" })
public class StartScheduledDispersion extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(StartScheduledDispersion.class);
    private static final ScheduledExecutorService scheduler = Executors
            .newScheduledThreadPool(1);
    private QueryClient queryClient;
    private DerivationClient devClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String ewkt = req.getParameter("ewkt");
        int nx = Integer.parseInt(req.getParameter("nx"));
        int ny = Integer.parseInt(req.getParameter("ny"));
        String citiesNamespace = req.getParameter("citiesnamespace");
        String scopeLabel = req.getParameter("label");
        String[] zArray = req.getParameterValues("z");
        String simulationTimeIri = req.getParameter("simulationTimeIri"); // optional
        String delayMinutes = req.getParameter("delayMinutes"); // optional
        String intervalMinutes = req.getParameter("intervalMinutes"); // optional

        int delay;
        if (delayMinutes != null) {
            delay = Integer.parseInt(delayMinutes);
        } else {
            delay = 30;
        }

        int interval;
        if (intervalMinutes != null) {
            interval = Integer.parseInt(intervalMinutes);
        } else {
            interval = 60;
        }

        InitialiseSimulation initialiseSimulation = new InitialiseSimulation();

        LOGGER.info("Creating a new dispersion instance if it does not already exist");
        String derivation = initialiseSimulation.createSimulation(ewkt, nx, ny, citiesNamespace, scopeLabel, zArray,
                simulationTimeIri);

        LOGGER.info("Starting scheduled dispersion simulations for <{}>", derivation);

        if (delay > 0) {
            LOGGER.info("Starting first update in {} minutes", delay);
        }

        scheduler.scheduleAtFixedRate(() -> {
            try {
                Instant nextUpdate = Instant.now().plus(1, ChronoUnit.HOURS);

                // timestamp to run dispersion simulations
                long simTime = Instant.now().minus(delay, ChronoUnit.MINUTES).getEpochSecond();

                queryClient.updateSimulationTime(derivation, simTime);
                devClient.updatePureSyncDerivation(derivation);

                LOGGER.info("Next update will be at {}", nextUpdate);
            } catch (Exception ex) {
                LOGGER.error(ex.getMessage());
            }
        }, delay, interval, TimeUnit.MINUTES);
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClient = new QueryClient(storeClient, null, null);
        devClient = new DerivationClient(storeClient, QueryClient.PREFIX);
    }
}
