package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.postgis.Polygon;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * a separate mapping is required for each SRID, currently only supports 32630
 */
@WebServlet(urlPatterns = {"/InitialiseSimulation"})
public class InitialiseSimulation extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(InitialiseSimulation.class);

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        // EWKT literal for the scope to create
        String ewkt = req.getParameter("ewkt");

        Polygon polygon = null;
        try {
            polygon = new Polygon(ewkt);
        } catch (SQLException e) {
            LOGGER.error("Failed to parse given EWKT literal", e);
        }
        
        if (polygon != null) {
            EndpointConfig endpointConfig = new EndpointConfig();
            DispersionPostGISClient dispersionPostGISClient = new DispersionPostGISClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
            RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
            QueryClient queryClient = new QueryClient(storeClient);

            String scopeIri = null;
            try (Connection conn = dispersionPostGISClient.getConnection()) {
                if (!dispersionPostGISClient.tableExists(EnvConfig.SCOPE_TABLE_NAME, conn)) {
                    // first time initialisation
                    dispersionPostGISClient.createTable(EnvConfig.SCOPE_TABLE_NAME, conn);
    
                    // add ontop mapping
                    Path obdaFile = new ClassPathResource("ontop.obda").getFile().toPath();
                    new OntopClient().updateOBDA(obdaFile);

                    // adds OntoAgent instance
                    queryClient.initialiseAgent();
                }
    
                if (!dispersionPostGISClient.scopeExists(polygon, conn)) {
                    scopeIri = dispersionPostGISClient.addScope(polygon, conn);
                } else {
                    String responseString = "Given EWKT literal already exists in the database, or the scopeExists query failed, check logs";
                    resp.getWriter().write(String.format("Created scope <%s>", scopeIri));
                    LOGGER.warn(responseString);
                }
            } catch (SQLException e) {
                LOGGER.error("SQL state {}", e.getSQLState());
                LOGGER.error(e.getMessage());
                LOGGER.error("Probably failed to close SQL connection or failed to connect");
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
                LOGGER.error("Probably failed to add ontop mapping");
            }

            if (scopeIri != null) {
                queryClient.initialiseScopeDerivation(scopeIri);
                resp.getWriter().write(String.format("Created scope <%s>", scopeIri));
            }
        }
    }
}