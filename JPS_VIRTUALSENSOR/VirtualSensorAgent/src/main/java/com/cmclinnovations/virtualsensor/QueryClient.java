package com.cmclinnovations.virtualsensor;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Long> tsClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));

    public static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final Prefix P_EMS = SparqlBuilder.prefix("ontoems", iri(ONTO_EMS));

    // classes
    // as Iri classes for sparql updates sent directly from here
    private static final Iri MEASURE = P_OM.iri("Measure");
    private static final Iri REPORTING_STATION = P_EMS.iri("ReportingStation");
    private static final Iri DISPERSION_OUTPUT = P_DISP.iri("DispersionOutput");
    private static final Iri DISPERSION_MATRIX = P_DISP.iri("DispersionMatrix");
    private static final Iri GEOM = iri("http://www.opengis.net/ont/geosparql#Geometry");

    // Pollutants
    private static final Iri POLLUTANT_ID = P_DISP.iri("PollutantID");
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

    private static final Iri UNIT_POLLUTANT_CONC = P_OM.iri("microgramPerCubicmetre");

    // properties
    private static final Iri HAS_PROPERTY = P_DISP.iri("hasProperty");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
    private static final Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");
    private static final Iri HAS_DISPERSION_MATRIX = P_DISP.iri("hasDispersionMatrix");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");
    private static final Iri HAS_WKT = iri("http://www.opengis.net/ont/geosparql#asWKT");
    private static final Iri HAS_OBSERVATION_LOCATION = P_EMS.iri("hasObservationLocation");

    public QueryClient(StoreClientInterface storeClient, TimeSeriesClient<Long> tsClient,
            RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        this.tsClient = tsClient;
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }

    public Long getLatestStationTime(String derivation) {
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

        Long latestTime = 0L;

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            latestTime = tsClient.getMaxTime(pollutantConcIri, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return latestTime;

    }

    public Map<String, String> getDispersionMatrixIris(String derivation) {
        // Get data IRIs of dispersion derivations
        SelectQuery query = Queries.SELECT();
        Variable dispersionOutput = query.var();
        Variable pollutantIri = query.var();
        Variable pollutant = query.var();
        Variable dispMatrix = query.var();

        // dispMatrix is the timeseries data IRI of the fileServer URL of the AERMOD
        // concentration output (averageConcentration.dat).
        // There is exactly one such data IRI for each pollutant ID.

        query.where(iri(derivation).has(isDerivedFrom, dispersionOutput),
                dispersionOutput.isA(DISPERSION_OUTPUT).andHas(HAS_POLLUTANT_ID, pollutantIri)
                        .andHas(HAS_DISPERSION_MATRIX, dispMatrix),
                pollutantIri.isA(pollutant)).prefix(P_DISP, P_OM, P_EMS)
                .select(pollutant, dispMatrix);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        Map<String, String> pollutantToDispMatrix = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String dispMatrixIri = queryResult.getJSONObject(i).getString(dispMatrix.getQueryString().substring(1));
            String pollutantId = queryResult.getJSONObject(i).getString(pollutant.getQueryString().substring(1));
            pollutantToDispMatrix.put(pollutantId, dispMatrixIri);
        }
        return pollutantToDispMatrix;

    }

    public Point getStationLocation(String derivation) {
        SelectQuery query = Queries.SELECT();
        Variable locationWkt = query.var();
        Variable locationIri = query.var();
        Variable station = query.var();

        query.where(station.has(belongsTo, iri(derivation)).andHas(HAS_OBSERVATION_LOCATION, locationIri),
                locationIri.isA(GEOM).andHas(HAS_WKT, locationWkt)).prefix(P_DISP, P_OM, P_EMS)
                .select(locationWkt);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        String locationString = queryResult.getJSONObject(0).getString(locationWkt.getQueryString().substring(1));

        Point location = null;
        try {
            location = new Point(locationString);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return location;

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

    public void updateStation(Long latestTime, Map<String, String> pollutantToDispMatrix,
            Map<String, String> pollutantToConcIri, Point stationLocation) {

        List<String> dispMatrixIriList = new ArrayList<>(pollutantToDispMatrix.values());
        TimeSeries<Long> dispMatrixTimeSeries = null;

        // Coordinates in the dispersion matrices will be in the
        // simulation srid.
        // Station location is stored in EPSG:4326.
        // Simulation srid can be determined from station location.

        double xp = stationLocation.getX();
        double yp = stationLocation.getY();
        // compute simulation srid
        int centreZoneNumber = (int) Math.ceil((xp + 180) / 6);
        int simulationSrid;
        if (yp < 0) {
            simulationSrid = Integer.valueOf("327" + centreZoneNumber);

        } else {
            simulationSrid = Integer.valueOf("326" + centreZoneNumber);
        }

        double[] xyOriginal = { xp, yp };
        int originalSrid = 4326;
        double[] xyTransformed = CRSTransformer.transform("EPSG:" + originalSrid, "EPSG:" + simulationSrid, xyOriginal);

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            dispMatrixTimeSeries = tsClient
                    .getTimeSeriesWithinBounds(dispMatrixIriList, latestTime, null, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            return;
        }

        List<String> tsDataList = new ArrayList<>();
        List<List<?>> tsValuesList = new ArrayList<>();
        for (String pollutant : pollutantToDispMatrix.keySet()) {
            String dispMatrixIri = pollutantToDispMatrix.get(pollutant);
            List<String> dispersionMatrixUrls = dispMatrixTimeSeries.getValuesAsString(dispMatrixIri);
            List<Double> concentrations = new ArrayList<>();
            for (int j = 0; j < dispersionMatrixUrls.size(); j++) {
                String fileUrl = dispersionMatrixUrls.get(j);
                double conc = 0.0;
                if (fileUrl != null)
                    conc = getInterpolatedValue(EnvConfig.PYTHON_SERVICE_INTERPOLATION_URL, fileUrl, xyTransformed[0],
                            xyTransformed[1]);
                concentrations.add(conc);
            }
            tsValuesList.add(concentrations);
            switch (pollutant) {
                case CO2:
                    tsDataList.add(pollutantToConcIri.get(CO2_CONC));
                    break;
                case NO_X:
                    tsDataList.add(pollutantToConcIri.get(NO_X_CONC));
                    break;
                case SO2:
                    tsDataList.add(pollutantToConcIri.get(SO2_CONC));
                    break;
                case CO:
                    tsDataList.add(pollutantToConcIri.get(CO_CONC));
                    break;
                case UHC:
                    tsDataList.add(pollutantToConcIri.get(UHC_CONC));
                    break;
                case PM10:
                    tsDataList.add(pollutantToConcIri.get(PM10_CONC));
                    break;
                case PM25:
                    tsDataList.add(pollutantToConcIri.get(PM25_CONC));
                    break;
                default:
                    LOGGER.info(
                            "Unknown pollutant ID encountered in the updateStation method of VirtualSensorAgent/QueryClient class: {}",
                            pollutant);

            }

        }

        // AermodAgent uses Unix timestamps. These must be converted to LocalDateTime
        // for the times
        // to be parsed correctly by FeatureInfoAgent.

        List<Long> timeStampsLong = dispMatrixTimeSeries.getTimes();
        List<LocalDateTime> timeStamps = new ArrayList<>();

        timeStampsLong.stream().forEach(ts -> {
            Instant timeInstant = Instant.ofEpochMilli(millis(ts));
            LocalDateTime ldt = LocalDateTime.ofInstant(timeInstant, ZoneOffset.UTC);
            timeStamps.add(ldt);
        });

        TimeSeries<LocalDateTime> timeSeries = new TimeSeries<>(timeStamps,
                tsDataList, tsValuesList);

        TimeSeriesClient<LocalDateTime> tsClientLdt = new TimeSeriesClient<>((RemoteStoreClient) storeClient,
                LocalDateTime.class);

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            tsClientLdt.addTimeSeriesData(timeSeries, conn);
        } catch (SQLException e) {
            LOGGER.error("Failed at closing connection");
            LOGGER.error(e.getMessage());
        }

    }

    /**
     * executes get request from python-service to postprocess
     */
    public double getInterpolatedValue(String endPoint, String outputFileURL, double xp, double yp) {
        URI httpGet;
        try {
            URIBuilder builder = new URIBuilder(endPoint);
            builder.setParameter("dispersionMatrix", outputFileURL);
            builder.setParameter("xp", String.valueOf(xp));
            builder.setParameter("yp", String.valueOf(yp));

            httpGet = builder.build();
        } catch (URISyntaxException e) {
            LOGGER.error("Failed at building URL");
            throw new RuntimeException(e);
        }

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
                CloseableHttpResponse httpResponse = httpClient.execute(new HttpGet(httpGet))) {
            String result = EntityUtils.toString(httpResponse.getEntity());
            JSONObject js = new JSONObject(result);
            return js.getDouble("result");
        } catch (IOException e) {
            LOGGER.error("Failed at making connection with python service");
            throw new RuntimeException(e);
        } catch (JSONException e) {
            LOGGER.error("Failed to parse result from python service for aermod geojson");
            LOGGER.error(outputFileURL);
            throw new RuntimeException(e);
        }
    }

    // Convert any Unix timestamp to milliseconds.
    // See https://www.baeldung.com/java-date-unix-timestamp.
    private long millis(long timestamp) {
        if (timestamp >= 1E16 || timestamp <= -1E16) {
            return timestamp / 1_000_000;
        }

        if (timestamp >= 1E14 || timestamp <= -1E14) {
            return timestamp / 1_000;
        }
        if (timestamp >= 1E11 || timestamp <= -3E10) {
            return timestamp;
        }

        return timestamp * 1_000;

    }
}
