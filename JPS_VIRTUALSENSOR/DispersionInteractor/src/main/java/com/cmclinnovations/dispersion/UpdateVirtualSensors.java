package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
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
    private QueryClient queryClient;
    private DispersionPostGISClient dispersionPostGISClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {

        LOGGER.info("Received POST request to update virtual sensor derivations ");
        // Check that at least one AERMOD simulation has been run
        try (Connection conn = dispersionPostGISClient.getConnection()) {
            if (!dispersionPostGISClient.tableExists("dispersion_contours", conn)) {
                LOGGER.error(
                        "At least one AERMOD simulation should be run before updating virtual sensor concentraion timeseries");
                return;
            }

        } catch (SQLException e) {
            LOGGER.error("SQL state {}", e.getSQLState());
            LOGGER.error(e.getMessage());
            LOGGER.error("Probably failed to close SQL connection or failed to connect");
        }

        // IRI of the dispersion derivation to update
        String derivation = req.getParameter("derivation");
        List<String> derivationList = queryClient.getVirtualSensorDerivations(derivation);

        for (String vsDerivation : derivationList) {
            devClient.updatePureSyncDerivation(vsDerivation);
        }

    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClient = new QueryClient(storeClient, null, null);
        devClient = new DerivationClient(storeClient, QueryClient.PREFIX);
        dispersionPostGISClient = new DispersionPostGISClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
    }

}
