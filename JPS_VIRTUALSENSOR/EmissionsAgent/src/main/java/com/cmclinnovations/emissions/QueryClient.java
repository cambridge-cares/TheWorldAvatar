package com.cmclinnovations.emissions;

import org.eclipse.rdf4j.model.vocabulary.TIME;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.List;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Instant> tsClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));

    // classes
    // strings to send to derivation outputs
    static final String EMISSION = PREFIX + "Emission";
    static final String NO_X = PREFIX + "NOx";
    static final String UHC = PREFIX + "uHC";
    static final String CO = PREFIX + "CO";
    static final String SO2 = PREFIX + "SO2";
    static final String PM10 = PREFIX + "PM10";
    static final String PM25 = PREFIX + "PM2.5";
    static final String DENSITY = OM_STRING + "Density";
    static final String MASS_FLOW = OM_STRING + "MassFlow";
    static final String TEMPERATURE = OM_STRING + "Temperature";
    static final String MEASURE_STRING = OM_STRING + "Measure";

    private static final String SPEED_STRING = PREFIX + "Speed";
    private static final Iri SPEED = iri(SPEED_STRING);
    private static final String SHIP_TYPE_STRING = PREFIX + "ShipType";
    private static final Iri SHIP_TYPE = iri(SHIP_TYPE_STRING);

    static final String SHIP = PREFIX + "Ship";
    static final String SIMULATION_TIME = PREFIX + "SimulationTime";

    // properties
    private static final Iri HAS_PROPERTY = P_DISP.iri("hasProperty");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    static final String EMITS = PREFIX + "emits";
    static final String HAS_POLLUTANT_ID = PREFIX + "hasPollutantID";

    public QueryClient(RemoteStoreClient storeClient, RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        TimeSeriesRDBClientWithReducedTables<Instant> timeSeriesRdbClient = new TimeSeriesRDBClientWithReducedTables<>(Instant.class);
        this.tsClient = new TimeSeriesClient<>(storeClient, timeSeriesRdbClient);
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }

    /**
     * used by Emissions agent to query a ship given an IRI
     * 
     * @param shipIri
     * @return
     */
    Ship getShip(String shipIri, Long simTime) {
        // step 1: query ship type
        SelectQuery query = Queries.SELECT();

        Variable shipType = query.var();
        Variable property = query.var();

        GraphPattern gp = GraphPatterns.and(iri(shipIri).has(HAS_PROPERTY, property),
                property.isA(SHIP_TYPE).andHas(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), shipType));

        query.prefix(P_DISP, P_OM).where(gp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        int shipTypeInt;
        if (queryResult.length() == 1) {
            shipTypeInt = queryResult.getJSONObject(0).getInt(shipType.getQueryString().substring(1));
        } else {
            throw new RuntimeException("Incorrect number of ships queried");
        }

        // step2: query ship speed measure iri
        SelectQuery query2 = Queries.SELECT();

        Variable speed = query2.var();

        GraphPattern gp2 = GraphPatterns.and(iri(shipIri).has(HAS_PROPERTY, property),
                property.isA(SPEED).andHas(HAS_VALUE, speed));

        query2.prefix(P_OM, P_DISP).where(gp2);

        JSONArray queryResult2 = storeClient.executeQuery(query2.getQueryString());

        String speedMeasure;
        if (queryResult.length() == 1) {
            speedMeasure = queryResult2.getJSONObject(0).getString(speed.getQueryString().substring(1));
        } else {
            throw new RuntimeException("Incorrect number of ships queried");
        }

        double shipSpeed;
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            // get value closest to simulation time
            if (simTime != null) {
                List<Double> speedList = tsClient
                        .getTimeSeriesWithinBounds(List.of(speedMeasure), Instant.ofEpochSecond(simTime), null, conn)
                        .getValuesAsDouble(speedMeasure);
                if (!speedList.isEmpty()) {
                    shipSpeed = speedList.get(0);
                } else {
                    shipSpeed = tsClient.getLatestData(speedMeasure, conn).getValuesAsDouble(speedMeasure).get(0);
                }
            } else {
                shipSpeed = tsClient.getLatestData(speedMeasure, conn).getValuesAsDouble(speedMeasure).get(0);
            }

            Ship ship = new Ship();
            ship.setSpeed(shipSpeed);
            ship.setShipType(shipTypeInt);
            return ship;
        } catch (SQLException e) {
            LOGGER.error("Failed at getting ship time series data");
            LOGGER.error(e.getMessage());
            return null;
        } catch (Exception e) {
            LOGGER.error("Probably the ship is instantiated but does not contain time series data yet");
            LOGGER.error(e.getMessage());
            return null;
        }
    }

    long getSimTimeValue(String simTimeIri) {
        SelectQuery query = Queries.SELECT().prefix(P_OM);
        Variable value = query.var();
        GraphPattern gp = iri(simTimeIri).has(
                PropertyPaths.path(iri(TIME.IN_TIME_POSITION), iri(TIME.NUMERIC_POSITION)), value);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return Long.parseLong(queryResult.getJSONObject(0).getString(value.getQueryString().substring(1)));
    }
}
