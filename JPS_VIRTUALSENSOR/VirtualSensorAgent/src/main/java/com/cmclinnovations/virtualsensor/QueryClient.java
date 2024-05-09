package com.cmclinnovations.virtualsensor;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.postgis.Point;

import com.cmclinnovations.virtualsensor.sparqlbuilder.ServiceEndpoint;

import it.unibz.inf.ontop.model.vocabulary.GEO;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Long> tsClientLong;
    private TimeSeriesClient<Instant> tsClientInstant;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));
    public static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final Prefix P_EMS = SparqlBuilder.prefix("ontoems", iri(ONTO_EMS));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));

    // classes
    // as Iri classes for sparql updates sent directly from here
    private static final Iri MEASURE = P_OM.iri("Measure");
    private static final Iri REPORTING_STATION = P_EMS.iri("ReportingStation");
    static final String DISPERSION_OUTPUT = PREFIX + "DispersionOutput";

    // Pollutants
    private static final String NO_X = PREFIX + "NOx";
    private static final String UHC = PREFIX + "uHC";
    private static final String CO = PREFIX + "CO";
    private static final String CO2 = PREFIX + "CO2";
    private static final String SO2 = PREFIX + "SO2";
    private static final String PM10 = PREFIX + "PM10";
    private static final String PM25 = PREFIX + "PM2.5";

    // Pollutant concentrations and units

    private static final String NO_X_CONC = ONTO_EMS + "NitrogenOxidesConcentration";
    private static final String UHC_CONC = ONTO_EMS + "UhcConcentration";// Currently not in ONTOEMS
    private static final String CO_CONC = ONTO_EMS + "CarbonMonoxideConcentration";
    private static final String CO2_CONC = ONTO_EMS + "CarbonDioxideConcentration";
    private static final String SO2_CONC = ONTO_EMS + "SulfurDioxideConcentration";
    private static final String PM10_CONC = ONTO_EMS + "PM10Concentration";
    private static final String PM25_CONC = ONTO_EMS + "PM2.5Concentration";

    // properties

    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
    private static final Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");
    private static final Iri HAS_DISPERSION_RASTER = P_DISP.iri("hasDispersionRaster");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");
    private static final Iri HAS_GEOMETRY = P_GEO.iri("hasGeometry");
    private static final Iri AS_WKT = iri("http://www.opengis.net/ont/geosparql#asWKT");

    public QueryClient(StoreClientInterface storeClient, TimeSeriesClient<Long> tsClientLong,
            TimeSeriesClient<Instant> tsClientInstant,
            RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        this.tsClientLong = tsClientLong;
        this.tsClientInstant = tsClientInstant;
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }

    public Instant getLatestStationTime(String derivation) {
        SelectQuery query = Queries.SELECT();
        Variable station = query.var();
        Variable pollutantConcentration = query.var();
        Variable measure = query.var();

        // Only need to consider one result because all pollutants are stored in a
        // single time series.
        // Number of rows in the time series table should the same for all pollutants.

        query.where(station.isA(REPORTING_STATION).andHas(belongsTo, iri(derivation)),
                station.has(REPORTS, pollutantConcentration),
                pollutantConcentration.has(HAS_VALUE, measure),
                measure.isA(MEASURE)).prefix(P_DISP, P_OM, P_EMS)
                .select(measure).limit(1);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        String pollutantConcIri = null;
        for (int i = 0; i < queryResult.length(); i++) {
            pollutantConcIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
        }

        Instant latestTime = null;

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            latestTime = tsClientInstant.getMaxTime(pollutantConcIri, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return latestTime;

    }

    public Map<String, String> getDispersionRasterIris(List<String> dispersionOutput) {
        // Get data IRIs of dispersion derivations
        SelectQuery query = Queries.SELECT();
        Variable pollutantIri = query.var();
        Variable pollutant = query.var();
        Variable dispRaster = query.var();
        Variable dispOutputVar = query.var();

        ValuesPattern values = new ValuesPattern(dispOutputVar,
                dispersionOutput.stream().map(Rdf::iri).collect(Collectors.toList()));

        // dispMatrix is the timeseries data IRI of the fileServer URL of the AERMOD
        // concentration output (averageConcentration.dat).
        // There is exactly one such data IRI for each pollutant ID.
        query.where(dispOutputVar.has(HAS_POLLUTANT_ID, pollutantIri)
                .andHas(HAS_DISPERSION_RASTER, dispRaster),
                pollutantIri.isA(pollutant), values).prefix(P_DISP, P_OM, P_EMS)
                .select(pollutant, dispRaster);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        Map<String, String> pollutantToDispRaster = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String dispMatrixIri = queryResult.getJSONObject(i).getString(dispRaster.getQueryString().substring(1));
            String pollutantId = queryResult.getJSONObject(i).getString(pollutant.getQueryString().substring(1));
            pollutantToDispRaster.put(pollutantId, dispMatrixIri);
        }
        return pollutantToDispRaster;
    }

    public Point getSensorLocation(String derivation) {
        // get the coordinates of this station
        // build coordinate query
        SelectQuery query2 = Queries.SELECT();
        Variable wkt = query2.var();
        Variable station = query2.var();

        ServiceEndpoint ontop = new ServiceEndpoint(new EndpointConfig().getOntopUrl());

        query2.select(wkt).where(station.has(belongsTo, iri(derivation)), ontop.service(
                station.has(PropertyPaths.path(HAS_GEOMETRY, AS_WKT), wkt))).prefix(P_GEO);

        // submit coordinate query to ontop via blazegraph
        String wktString = storeClient.executeQuery(query2.getQueryString()).getJSONObject(0)
                .getString(wkt.getQueryString().substring(1));

        // parse wkt literal
        Point sensorLocation = null;

        try {
            sensorLocation = new Point(wktString);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to parse result of virtual sensor ontop query.");
        }

        return sensorLocation;
    }

    public Map<String, String> getStationDataIris(String derivation) {
        SelectQuery query = Queries.SELECT();
        Variable station = query.var();
        Variable quantity = query.var();
        Variable measure = query.var();
        Variable pollutantConcentration = query.var();

        // Only need to consider one result because all pollutants are stored in a
        // single time series.
        // Number of rows in the time series table should the same for all pollutants.

        query.where(station.isA(REPORTING_STATION).andHas(belongsTo, iri(derivation)),
                station.has(REPORTS, quantity),
                quantity.isA(pollutantConcentration),
                quantity.has(HAS_VALUE, measure),
                measure.isA(MEASURE)).prefix(P_DISP, P_OM, P_EMS)
                .select(measure, pollutantConcentration);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        Map<String, String> pollutantToConcIriMap = new HashMap<>();

        for (int i = 0; i < queryResult.length(); i++) {
            String pollutantConcIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
            String pollutantConcId = queryResult.getJSONObject(i)
                    .getString(pollutantConcentration.getQueryString().substring(1));
            pollutantToConcIriMap.put(pollutantConcId, pollutantConcIri);
        }

        return pollutantToConcIriMap;
    }

    public void updateStationUsingDispersionRaster(Instant latestTime, Map<String, String> pollutantToDispRaster,
            Map<String, String> concToDataIriMap, Point stationLocation, Connection conn) {

        List<String> dispRasterIriList = new ArrayList<>(pollutantToDispRaster.values());

        // Coordinates in the dispersion matrices will be in the
        // simulation srid.
        // Station location is stored in EPSG:4326.
        // Simulation srid can be determined from station location.

        // Convert from Instant to Long
        Long latestTimeLong = null;

        if (latestTime != null)
            latestTimeLong = latestTime.getEpochSecond();

        List<String> tsDataList = new ArrayList<>();
        List<List<?>> tsValuesList = new ArrayList<>();

        Map<String, String> concToPollutantMap = new HashMap<>();
        concToPollutantMap.put(CO2_CONC, CO2);
        concToPollutantMap.put(CO_CONC, CO);
        concToPollutantMap.put(NO_X_CONC, NO_X);
        concToPollutantMap.put(PM25_CONC, PM25);
        concToPollutantMap.put(PM10_CONC, PM10);
        concToPollutantMap.put(SO2_CONC, SO2);
        concToPollutantMap.put(UHC_CONC, UHC);

        TimeSeries<Long> dispRasterTimeSeries = tsClientLong
                .getTimeSeriesWithinBounds(dispRasterIriList, latestTimeLong, null, conn);

        for (Map.Entry<String, String> concToDataIri : concToDataIriMap.entrySet()) {
            String pollutant = concToPollutantMap.get(concToDataIri.getKey());
            String dispRasterIri = pollutantToDispRaster.get(pollutant);
            List<String> dispersionRasterFileNames = dispRasterTimeSeries.getValuesAsString(dispRasterIri);
            List<Double> concentrations = new ArrayList<>();
            for (int j = 0; j < dispersionRasterFileNames.size(); j++) {
                String rasterFileName = dispersionRasterFileNames.get(j);
                String sql = String.format(
                        "SELECT ST_Value(rast, ST_Transform(ST_GeomFromText('%s',4326),ST_SRID(rast))) AS val "
                                + "FROM %s WHERE ST_Intersects(rast, ST_Transform(ST_GeomFromText('%s',4326),ST_SRID(rast))) "
                                + "AND filename='%s'",
                        stationLocation.toString(),
                        "dispersion_raster", stationLocation.toString(), rasterFileName);
                try (ResultSet result = conn.createStatement().executeQuery(sql)) {
                    if (result.next()) {
                        double concentration = result.getDouble("val");
                        concentrations.add(concentration);
                    } else {
                        String errmsg = "Could not obtain raster value";
                        LOGGER.error(errmsg);
                        throw new RuntimeException(errmsg);
                    }
                } catch (SQLException e) {
                    String errmsg = "Possible error at reading sql query result";
                    LOGGER.error(e.getMessage());
                    LOGGER.error(errmsg);
                    throw new RuntimeException(errmsg, e);
                }
            }
            tsValuesList.add(concentrations);
            tsDataList.add(concToDataIri.getValue());
        }

        // AermodAgent uses Unix timestamps. These must be converted to Instant
        // for the times
        // to be parsed correctly by FeatureInfoAgent.
        List<Long> timeStampsLong = dispRasterTimeSeries.getTimes();
        List<Instant> timeStamps = new ArrayList<>();

        timeStampsLong.stream().forEach(ts -> {
            Instant timeInstant = Instant.ofEpochSecond(ts);
            timeStamps.add(timeInstant);
        });

        TimeSeries<Instant> timeSeries = new TimeSeries<>(timeStamps,
                tsDataList, tsValuesList);

        tsClientInstant.addTimeSeriesData(timeSeries, conn);
    }
}
