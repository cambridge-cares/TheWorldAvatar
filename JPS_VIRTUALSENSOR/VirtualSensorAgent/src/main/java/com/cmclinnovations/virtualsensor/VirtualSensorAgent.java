package com.cmclinnovations.virtualsensor;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.postgis.Point;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

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
        TimeSeriesClient<Long> tsClientLong = new TimeSeriesClient<>(storeClient, Long.class);
        TimeSeriesClient<Instant> tsClientInstant = new TimeSeriesClient<>(storeClient, Instant.class);
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        queryClient = new QueryClient(storeClient, tsClientLong, tsClientInstant, remoteRDBStoreClient);
        super.devClient = new DerivationClient(storeClient, QueryClient.PREFIX);
        // add ontop mapping
        Path obdaFile = null;
        try {
            obdaFile = new ClassPathResource("ontop.obda").getFile().toPath();
        } catch (IOException e) {
            LOGGER.error("Could not retrieve virtual sensor ontop.obda file.");
        }
        OntopClient ontopClient = OntopClient.getInstance();
        ontopClient.updateOBDA(obdaFile);
    }

    @Override
    public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {

        LOGGER.info("Received request to update virtual sensor derivation {}", derivationInputs.getDerivationIRI());
        // Get latest time for which pollutant data has been stored, list of dispersion
        // matrix data IRIs and station location
        String derivation = derivationInputs.getDerivationIRI();
        Instant latestTime = queryClient.getLatestStationTime(derivation);
        Map<String, String> pollutantToDispRaster = queryClient.getDispersionRasterIris(derivation);
        Point stationLocation = queryClient.getSensorLocation(derivation);
        Map<String, String> pollutantToConcIri = queryClient.getStationDataIris(derivation);
        queryClient.updateStationUsingDispersionRaster(latestTime, pollutantToDispRaster, pollutantToConcIri,
                stationLocation);

    }

}