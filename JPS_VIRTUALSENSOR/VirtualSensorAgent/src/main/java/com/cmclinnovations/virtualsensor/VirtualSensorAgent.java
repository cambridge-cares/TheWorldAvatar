package com.cmclinnovations.virtualsensor;

import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { "/" })
public class VirtualSensorAgent extends DerivationAgent {
    private static final Logger LOGGER = LogManager.getLogger(VirtualSensorAgent.class);
    private QueryClient queryClient;

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig();
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        DerivationClient derivationClient = new DerivationClient(storeClient, QueryClient.PREFIX);
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        queryClient = new QueryClient(storeClient, tsClient, derivationClient, remoteRDBStoreClient);
    }

    @Override
    public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {

        // Get latest time for which pollutant data has been stored
        String derivation = derivationInputs.getDerivationIRI();
        Long latestTime = queryClient.getLatestStationTime(derivation);
        Map<String, String> pollutantToDispMatrix = queryClient.getDispersionMatrixIris(derivation);
        queryClient.updateStation(latestTime, pollutantToDispMatrix);

    }

}