package uk.ac.cam.cares.jps.agent.gfaagent;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

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
import org.springframework.core.io.ClassPathResource;

@WebServlet(urlPatterns = { GFAAgent.COST_PATH, GFAAgent.GFA_PATH })
public class GFAAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GFAAgent.class);
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private String ontopUrl;

    public static final String GFA_PATH = "/gfa";
    public static final String COST_PATH = "/cost";

    @Override
    public synchronized void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        ontopUrl = endpointConfig.getOntopUrl();
        remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(), endpointConfig.getDbUser(),
                endpointConfig.getDbPassword());
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        GFACalculation gfaCalculation = new GFACalculation(ontopUrl);

        if (req.getServletPath().contentEquals(GFA_PATH)) {
            // calculate GFA 1. query footpring 2. query height (if no height, estimate
            // 3.2m/floor) 3. calculate 4. store
            try (Connection conn = remoteRDBStoreClient.getConnection()) {
                GFAPostGISClient.createSchema(conn);
                GFAPostGISClient.createGFATable(conn);

                LOGGER.info("Starting to add GFA data");
                gfaCalculation.calculationGFA(conn);
                LOGGER.info("GFA calculation done");

            } catch (SQLException e) {
                throw new RuntimeException("Error during GFA calculation", e);
            }

            LOGGER.info("Uploading OBDA file for GFA data");
            // Upload Ontop mapping
            Path obdaPath;
            try {
                obdaPath = new ClassPathResource("gfa.obda").getFile().toPath();
            } catch (IOException e) {
                throw new RuntimeException("Error uploading gfa.obda", e);
            }
            OntopClient ontopClient = OntopClient.getInstance();
            ontopClient.updateOBDA(obdaPath);

        } else if (req.getServletPath().contentEquals(COST_PATH)) {
            LOGGER.info("Received POST request to calculate building cost");
            CostCalculation costCalculation = new CostCalculation(ontopUrl);

            LOGGER.info("Querying buildings info");
            costCalculation.setBuildings();

            LOGGER.info("Calculating costs");
            costCalculation.calculateCost();

            LOGGER.info("Uploading data");
            try (Connection conn = remoteRDBStoreClient.getConnection()) {
                costCalculation.uploadData(conn);
            } catch (SQLException e) {
                throw new RuntimeException("Error uploading cost data", e);
            }
            LOGGER.info("Uploading OBDA file for cost data");
            // Upload Ontop mapping
            Path obdaPath;
            try {
                obdaPath = new ClassPathResource("cost.obda").getFile().toPath();
            } catch (IOException e) {
                throw new RuntimeException("Error uploading cost.obda", e);
            }
            OntopClient ontopClient = OntopClient.getInstance();
            ontopClient.updateOBDA(obdaPath);
            LOGGER.info("Uploaded obda file");
        }
    }
}
