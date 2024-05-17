package com.cmclinnovations.emissions;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.time.Instant;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Instant> tsClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";

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
    static final String FUEL_CONSUMPTION = PREFIX + "SpecificFuelConsumption";
    static final String ENERGY_EFFICIENCY = PREFIX + "EnergyEfficiency";
    static final String SPECIFIC_CO2_EMISSION = PREFIX + "SpecificCO2Emission";
    static final String CII = PREFIX + "CII";

    static final String SHIP = PREFIX + "Ship";
    static final String SIMULATION_TIME = PREFIX + "SimulationTime";

    // properties
    static final String HAS_PROPERTY_STRING = PREFIX + "hasProperty";
    static final String HAS_VALUE_STRING = OM_STRING + "hasValue";
    static final String HAS_NUMERICALVALUE_STRING = OM_STRING + "hasNumericalValue";
    static final String EMITS = PREFIX + "emits";
    static final String HAS_POLLUTANT_ID = PREFIX + "hasPollutantID";

    public QueryClient(RemoteStoreClient storeClient, RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        this.tsClient = new TimeSeriesClient<>(storeClient, Instant.class);
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }
}
