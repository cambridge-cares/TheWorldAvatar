package com.cmclinnovations.dispersion;

import java.io.IOException;
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
 * First calls ship input agent to add 1 timestep worth of data
 * Obtain average timestamp of the data added
 * Updates SimulationTime instance (input of derivation)
 * Call updateDerivation on given derivation
 */
@WebServlet(urlPatterns = {"/TriggerUpdateDispersion"})
public class TriggerUpdateDispersion extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(TriggerUpdateDispersion.class);
    private DerivationClient devClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        // IRI of the dispersion derivation to update
        String derivation = req.getParameter("derivation");
        LOGGER.info("Received PUT request to update derivation <{}>", derivation);
        devClient.updatePureSyncDerivation(derivation);
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        devClient = new DerivationClient(storeClient, QueryClient.PREFIX);
    }
}
