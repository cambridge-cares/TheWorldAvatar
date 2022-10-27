package com.cmclinnovations.aermod;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.locationtech.jts.geom.Geometry;

import com.cmclinnovations.aermod.objects.Ship;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = {"/"})
public class AermodAgent extends DerivationAgent {
    private static final Logger LOGGER = LogManager.getLogger(AermodAgent.class);
    private QueryClient queryClient;

    @Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {   
        String weatherStationIri = derivationInputs.getIris(QueryClient.REPORTING_STATION).get(0);
        String nxIri = derivationInputs.getIris(QueryClient.NX).get(0);
        String nyIri = derivationInputs.getIris(QueryClient.NY).get(0);
        String scopeIri = derivationInputs.getIris(QueryClient.SCOPE).get(0);
        String simulationTimeIri = derivationInputs.getIris(QueryClient.SIMULATION_TIME).get(0);
        
        long simulationTime = queryClient.getMeasureValueAsLong(simulationTimeIri);

        if (simulationTime == 0) {
            LOGGER.info("Simulation time = 0, this is from calling createSyncDerivationForNewInfo the first time");
            return;
        }

        int nx = queryClient.getMeasureValueAsInt(nxIri);
        int ny = queryClient.getMeasureValueAsInt(nyIri);

        // get ships within a scope and time
        Geometry scope = queryClient.getScopeFromOntop(scopeIri);
        List<Ship> ships = queryClient.getShipsWithinTimeAndScopeViaTsClient(simulationTime, scope);

        // update derivation of ships (on demand)
        List<String> derivationsToUpdate = queryClient.getDerivationsOfShips(ships);
        updateDerivations(derivationsToUpdate);

        // get emissions and set the values in the ships
        queryClient.setEmissions(ships);
    }
    
    void updateDerivations(List<String> derivationsToUpdate) {
        CompletableFuture<String> getAsync = null;
        for (String derivation : derivationsToUpdate) {
            getAsync = CompletableFuture.supplyAsync(() -> {
                try {
                    devClient.updatePureSyncDerivation(derivation);
                    return null; // need to return something, could not get it to work with CompletableFuture<Void>
                } catch (Exception e) {
                    LOGGER.error(e.getMessage());
                    LOGGER.error("Error occured while updating ship derivations");
                    return null;
                }
            });   
        }
        if (getAsync != null) {
            getAsync.join();
        }
    }

    void create144File() {

    }
    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig(); 

        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        RemoteStoreClient ontopStoreClient = new RemoteStoreClient(endpointConfig.getOntopurl());
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());

        queryClient = new QueryClient(storeClient, ontopStoreClient, rdbStoreClient);
        super.devClient = new DerivationClient(storeClient, QueryClient.PREFIX_DISP);
    }
}