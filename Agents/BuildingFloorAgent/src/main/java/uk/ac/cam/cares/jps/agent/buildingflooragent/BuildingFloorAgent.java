package uk.ac.cam.cares.jps.agent.buildingflooragent;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.io.IOException;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

@WebServlet(urlPatterns = { "/" })
public class BuildingFloorAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(BuildingFloorAgent.class);
    private EndpointConfig endpointConfig;

    private RemoteRDBStoreClient postgisClient;

    @Override
    public synchronized void init() {
        endpointConfig = new EndpointConfig();
        postgisClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(), endpointConfig.getDbUser(),
                endpointConfig.getDbPassword());
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        // integrate floors data: 1. query osm address 2. match address from HDB csv 3.
        // store floors data
        IntegrateFloors integrateFloors = new IntegrateFloors(endpointConfig.getOntopUrl());

        try (Connection conn = postgisClient.getConnection()) {
            LOGGER.info("Initialising schema to store results");
            FloorPostGISClient.createSchema(conn);
            FloorPostGISClient.createTable(conn);

            LOGGER.info("Querying OSM building data");
            integrateFloors.setOSMBuildings();

            LOGGER.info("Updating floor data based on CSV input");
            integrateFloors.matchAddress(endpointConfig.getFilepath(), conn);

            LOGGER.info("Updating floor data based on OSM and rough estimate");
            integrateFloors.importFloorData(conn);

        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        // Upload Ontop mapping
        LOGGER.info("Uploading ontop mapping");
        Path obdaPath;
        try {
            obdaPath = new ClassPathResource("buildingfloor.obda").getFile().toPath();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        OntopClient ontopClient = OntopClient.getInstance();
        ontopClient.updateOBDA(obdaPath);

        LOGGER.info("Uploaded mapping");

        return requestParams;
    }
}
