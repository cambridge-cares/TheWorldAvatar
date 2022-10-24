package com.cmclinnovations.emissions;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Long> tsClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "http://www.theworldavatar.com/dispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp",iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri(OM_STRING));

    // classes
    // strings to send to derivation outputs
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

    // properties
    private static final Iri HAS_SPEED = P_DISP.iri("hasSpeed");
    private static final Iri HAS_SHIPTYPE = P_DISP.iri("hasShipType");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");

    public QueryClient(RemoteStoreClient storeClient, RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        this.tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }

    /**
     * used by Emissions agent to query a ship given an IRI
     * @param shipIri
     * @return
     */
    Ship getShip(String shipIri) {
        // step 1: query measure IRIs for each ship and group them
        SelectQuery query = Queries.SELECT();

        Variable speed = query.var();
        Variable shipType = query.var();

        GraphPattern gp = iri(shipIri).has(PropertyPaths.path(HAS_SPEED,HAS_VALUE),speed)
        .andHas(PropertyPaths.path(HAS_SHIPTYPE,HAS_VALUE,HAS_NUMERICALVALUE), shipType);

        query.prefix(P_DISP,P_OM).where(gp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        String speedMeasure;
        int shipTypeInt;
        if (queryResult.length() == 1) {
            speedMeasure = queryResult.getJSONObject(0).getString(speed.getQueryString().substring(1));
            shipTypeInt = queryResult.getJSONObject(0).getInt(shipType.getQueryString().substring(1));
        } else {
            throw new RuntimeException("Incorrect number of ships queried");
        }

        int shipSpeed;
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            shipSpeed = tsClient.getLatestData(speedMeasure,conn).getValuesAsInteger(speedMeasure).get(0);
            Ship ship = new Ship();
            ship.setSpeed(shipSpeed);
            ship.setShipType(shipTypeInt);
            return ship;
        } catch (SQLException e) {
            LOGGER.error("Failed at getting ship time series data");
            LOGGER.error(e.getMessage());
            return null;
        }
    }
}
