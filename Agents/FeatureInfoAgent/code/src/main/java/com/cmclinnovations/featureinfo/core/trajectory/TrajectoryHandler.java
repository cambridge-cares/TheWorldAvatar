package com.cmclinnovations.featureinfo.core.trajectory;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.apache.logging.log4j.LogManager;

import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.config.StackInteractor;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class TrajectoryHandler {
    private static final Logger LOGGER = LogManager.getLogger(TrajectoryHandler.class);
    private final Optional<String> enforcedEndpoint;
    private final String iri;
    private final ConfigStore configStore;
    private TimeSeriesClient<Instant> tsClient;
    private RemoteStoreClient remoteStoreClient;

    public TrajectoryHandler(String iri, Optional<String> enforcedEndpoint, ConfigStore configStore) {
        this.iri = iri;
        this.enforcedEndpoint = enforcedEndpoint;
        this.configStore = configStore;
    }

    public void setClients(RemoteStoreClient remoteStoreClient, TimeSeriesClient<Instant> tsClient) {
        this.remoteStoreClient = remoteStoreClient;
        this.tsClient = tsClient;
    }

    public JSONArray getData(List<ConfigEntry> classMatches) {
        JSONArray data = new JSONArray();
        classMatches.forEach(classMatch -> {
            String database = classMatch.getTrajectoryDatabase();

            String featureIriQuery = classMatch.getFeatureIriQuery();

            List<String> featureIriList = new ArrayList<>();
            try (Connection conn = connectToDatabase(database); Statement statement = conn.createStatement()) {
                ResultSet result = statement.executeQuery(featureIriQuery);

                while (result.next()) {
                    featureIriList.add(result.getString("iri"));
                }
            } catch (SQLException e) {

            }

            String trajectoryMetaQueryTemplate = classMatch.getTrajectoryMetaQuery();

            ValuesPattern valuesPattern = new ValuesPattern(SparqlBuilder.var("building"),
                    featureIriList.stream().map(Rdf::iri).collect(Collectors.toList()));
            String trajectoryMetaQuery = trajectoryMetaQueryTemplate.replace("[FEATURE_IRI_VALUES]",
                    valuesPattern.getQueryString());

            JSONArray queryResult = remoteStoreClient.executeQuery(trajectoryMetaQuery);
            data.put(queryResult);

        });
        return data;
    }

    /**
     * Re-initialises the RDB client with a connection to the input database.
     * 
     * @param rdbEndpoint endpoint for postgres.
     * @param database    database name.
     * 
     * @throws SQLException if database cannot be connected to.
     */
    protected Connection connectToDatabase(String database) throws SQLException {
        StackEndpoint rdbEndpoint = this.configStore.getStackEndpoints(StackEndpointType.POSTGRES).get(0);
        LOGGER.info("Making new connection to database: {}", database);
        String postgresURL = StackInteractor.generatePostgresURL(database);
        // Create new connection
        RemoteRDBStoreClient dbClient = new RemoteRDBStoreClient(postgresURL, rdbEndpoint.username(),
                rdbEndpoint.password());
        return dbClient.getConnection();
    }
}
