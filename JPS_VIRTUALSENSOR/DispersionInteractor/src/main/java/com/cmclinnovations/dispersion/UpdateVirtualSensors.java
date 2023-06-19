package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = { "/UpdateVirtualSensors" })
public class UpdateVirtualSensors extends HttpServlet {

    private static final Logger LOGGER = LogManager.getLogger(UpdateVirtualSensors.class);
    private DerivationClient devClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        // IRI of the dispersion derivation to update

        LOGGER.info("Received POST request to update virtual sensor derivations ");
        List<String> derivationList = QueryClient.getVirtualSensorDerivations();

        for (String derivation : derivationList) {
            devClient.updatePureSyncDerivation(derivation);
        }

    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        devClient = new DerivationClient(storeClient, QueryClient.PREFIX);
    }

}
