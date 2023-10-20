package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
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

@WebServlet(urlPatterns = { "/GenerateDataWithoutShips" })
public class GenerateDataWithoutShips extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GenerateDataWithoutShips.class);
    private QueryClient queryClient;
    private DerivationClient devClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String derivation = req.getParameter("derivation");

        List<Long> timesteps = new ArrayList<>();
        String[] timestepsFromInput = req.getParameterValues("timestep");

        if (timestepsFromInput == null) {
            timesteps.add(Instant.now().getEpochSecond());
        } else {
            for (String timestepFromInput : timestepsFromInput) {
                timesteps.add(Long.parseLong(timestepFromInput));
            }
        }

        timesteps.stream().forEach(timestep -> {
            queryClient.updateSimulationTime(derivation, timestep);
            devClient.updatePureSyncDerivation(derivation);
        });
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClient = new QueryClient(storeClient, null, null);
        devClient = new DerivationClient(storeClient, QueryClient.PREFIX);
    }
}
