package com.cmclinnovations.aermod;

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

@WebServlet(urlPatterns = {"/"})
public class AermodAgent extends DerivationAgent {
    private static final Logger LOGGER = LogManager.getLogger(AermodAgent.class);
    private RemoteRDBStoreClient rdbStoreClient;
    private QueryClient queryClient;
    private TimeSeriesClient<Long> tsClient;

    @Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {   
        String weatherStationIri = derivationInputs.getIris(Constants.REPORTING_STATION).get(0);
        String nxIri = derivationInputs.getIris(Constants.NX).get(0);
        String nyIri = derivationInputs.getIris(Constants.NY).get(0);
        String scopeIri = derivationInputs.getIris(Constants.SCOPE).get(0);
        String simulationTimeIri = derivationInputs.getIris(Constants.SIMULATION_TIME).get(0);

        int nx = queryClient.getMeasureValueAsInt(nxIri);
        int ny = queryClient.getMeasureValueAsInt(nyIri);
        long simulationTime = queryClient.getMeasureValueAsLong(simulationTimeIri);

        // get ship within a scope
    }
    
    void create144File() {

    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig(); 
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        queryClient = new QueryClient(storeClient);
        super.devClient = new DerivationClient(storeClient, Constants.PREFIX_DISP);
    }
}