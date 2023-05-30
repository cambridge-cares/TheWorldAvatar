package com.cmclinnovations.aermod;

import org.eclipse.rdf4j.model.vocabulary.GEOF;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.constraint.SparqlFunction;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.CoordinateSequenceFilter;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.util.GeometryFixer;
import org.locationtech.jts.geom.Point;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;

import com.cmclinnovations.aermod.sparqlbuilder.GeoSPARQL;
import com.cmclinnovations.aermod.sparqlbuilder.ValuesPattern;

import java.util.Arrays;
import java.util.Collections;

import com.cmclinnovations.aermod.objects.Building;
import com.cmclinnovations.aermod.objects.PointSource;
import com.cmclinnovations.aermod.objects.Ship;
import com.cmclinnovations.aermod.objects.StaticPointSource;
import com.cmclinnovations.aermod.objects.WeatherData;

import it.unibz.inf.ontop.model.vocabulary.GEO;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementService;
import org.apache.jena.graph.Node;

public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontopStoreClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private TimeSeriesClient<Long> tsClientLong;
    private TimeSeriesClient<Instant> tsClientInstant;
    private String citiesNamespace;
    private String namespaceCRS;

    // prefixes
    private static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final String PREFIX_DISP = "https://www.theworldavatar.com/kg/ontodispersion/";
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String CHEM = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#";
    public static final String ONTO_CITYGML = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";

    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX_DISP));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));
    private static final Prefix P_GEOF = SparqlBuilder.prefix("geof", iri(GEOF.NAMESPACE));
    private static final Prefix P_EMS = SparqlBuilder.prefix("ems", iri(ONTO_EMS));
    private static final Prefix P_OCGML = SparqlBuilder.prefix("ocgml", iri(ONTO_CITYGML));
    // classes
    public static final String REPORTING_STATION = "https://www.theworldavatar.com/kg/ontoems/ReportingStation";
    public static final String NX = PREFIX_DISP + "nx";
    public static final String NY = PREFIX_DISP + "ny";
    public static final String SCOPE = PREFIX_DISP + "Scope";
    public static final String CITIES_NAMESPACE = PREFIX_DISP + "OntoCityGMLNamespace";
    public static final String SIMULATION_TIME = PREFIX_DISP + "SimulationTime";
    public static final String NO_X = PREFIX_DISP + "NOx";
    public static final String UHC = PREFIX_DISP + "uHC";
    public static final String CO = PREFIX_DISP + "CO";
    public static final String SO2 = PREFIX_DISP + "SO2";
    public static final String PM10 = PREFIX_DISP + "PM10";
    public static final String PM25 = PREFIX_DISP + "PM2.5";
    public static final String CO2 = PREFIX_DISP + "CO2";
    public static final String DENSITY = OM_STRING + "Density";
    public static final String TEMPERATURE = OM_STRING + "Temperature";
    public static final String MASS_FLOW = OM_STRING + "MassFlow";
    private static final Iri SHIP = P_DISP.iri("Ship");
    private static final Iri MEASURE = P_OM.iri("Measure");
    private static final Iri STATIC_POINT_SOURCE = P_DISP.iri("StaticPointSource");

    // weather types
    private static final String CLOUD_COVER = ONTO_EMS + "CloudCover";
    private static final String AIR_TEMPERATURE = ONTO_EMS + "AirTemperature";
    private static final String RELATIVE_HUMIDITY = ONTO_EMS + "RelativeHumidity";
    private static final String WIND_SPEED = ONTO_EMS + "WindSpeed";
    private static final String WIND_DIRECTION = ONTO_EMS + "WindDirection";
    private static final String NO2_CONC = ONTO_EMS + "NitrogenDioxideConcentration";

    // IRI of units used
    private static final Iri UNIT_DEGREE = P_OM.iri("degree");
    private static final Iri UNIT_CELCIUS = P_OM.iri("degreeCelsius");
    private static final Iri UNIT_MS = P_OM.iri("metrePerSecond-Time");
    private static final Iri UNIT_PERCENTAGE = P_OM.iri("PercentageUnit");
    private static final Iri UNIT_POLLUTANT_CONC = P_OM.iri("microgramPerCubicmetre");
    private static final String UNIT_KG_S = OM_STRING + "kilogramPerSecond-Time";
    private static final String UNIT_KELVIN = OM_STRING + "kelvin";
    private static final String UNIT_KG_M3 = OM_STRING + "kilogramPerCubicmetre";

    // Location type
    private static final Iri LOCATION = P_DISP.iri("Location");
    private static final Iri OBSERVATION_LOCATION = P_OM.iri("hasObservationLocation");

    // outputs (belongsTo)
    private static final String DISPERSION_MATRIX = PREFIX_DISP + "DispersionMatrix";
    private static final String DISPERSION_LAYER = PREFIX_DISP + "DispersionLayer";
    private static final String SHIPS_LAYER = PREFIX_DISP + "ShipsLayer";

    // properties
    private static final Iri HAS_PROPERTY = P_DISP.iri("hasProperty");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri HAS_QUANTITY = P_OM.iri("hasQuantity");
    private static final Iri HAS_UNIT = P_OM.iri("hasUnit");
    private static final Iri HAS_GEOMETRY = P_GEO.iri("hasGeometry");
    private static final Iri AS_WKT = P_GEO.iri("asWKT");
    private static final Iri IS_DERIVED_FROM = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
    private static final Iri BELONGS_TO = iri(DerivationSparql.derivednamespace + "belongsTo");
    private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri HAS_NAME = P_DISP.iri("hasName");
    private static final Iri HAS_OCGML_OBJECT = P_DISP.iri("hasOntoCityGMLCityObject");
    private static final Iri EMITS = P_DISP.iri("emits");
    private static final Iri HAS_SRS_NAME = P_OCGML.iri("srsname");
    private static final Iri HAS_QTY = P_DISP.iri("hasQuantity");
    private static final Iri OCGML_GEOM = P_OCGML.iri("GeometryType");
    private static final Iri OCGML_CITYOBJECT = P_OCGML.iri("cityObjectId");
    private static final Iri OCGML_OBJECTCLASSID = P_OCGML.iri("objectClassId");
    private static final Iri OCGML_LOD2MULTISURFACEID = P_OCGML.iri("lod2MultiSurfaceId");
    private static final Iri OCGML_BUILDINGID = P_OCGML.iri("buildingId");
    private static final Iri OCGML_ENVELOPETYPE = P_OCGML.iri("EnvelopeType");

    // fixed units for each measured property
    private static final Map<String, Iri> UNIT_MAP = new HashMap<>();
    static {
        UNIT_MAP.put(CLOUD_COVER, UNIT_PERCENTAGE);
        UNIT_MAP.put(AIR_TEMPERATURE, UNIT_CELCIUS);
        UNIT_MAP.put(RELATIVE_HUMIDITY, UNIT_PERCENTAGE);
        UNIT_MAP.put(WIND_SPEED, UNIT_MS);
        UNIT_MAP.put(WIND_DIRECTION, UNIT_DEGREE);
    }

    public QueryClient(RemoteStoreClient storeClient, RemoteStoreClient ontopStoreClient,
            RemoteRDBStoreClient rdbStoreClient) {
        this.storeClient = storeClient;
        this.ontopStoreClient = ontopStoreClient;
        this.tsClientLong = new TimeSeriesClient<>(storeClient, Long.class);
        this.tsClientInstant = new TimeSeriesClient<>(storeClient, Instant.class);
        this.rdbStoreClient = rdbStoreClient;
    }

    String getCitiesNamespace(String citiesNamespaceIri) {
        SelectQuery query = Queries.SELECT().prefix(P_DISP);
        Variable citiesNamespace = query.var();
        GraphPattern gp = iri(citiesNamespaceIri).has(HAS_NAME, citiesNamespace);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return queryResult.getJSONObject(0).getString(citiesNamespace.getQueryString().substring(1));
    }

    int getMeasureValueAsInt(String instance) {
        SelectQuery query = Queries.SELECT().prefix(P_OM);
        Variable value = query.var();
        GraphPattern gp = iri(instance).has(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), value);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return Integer.parseInt(queryResult.getJSONObject(0).getString(value.getQueryString().substring(1)));
    }

    long getMeasureValueAsLong(String instance) {
        SelectQuery query = Queries.SELECT().prefix(P_OM);
        Variable value = query.var();
        GraphPattern gp = iri(instance).has(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), value);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return Long.parseLong(queryResult.getJSONObject(0).getString(value.getQueryString().substring(1)));
    }

    private List<String> queryStaticPointSources() {
        SelectQuery query = Queries.SELECT().prefix(P_DISP);
        Variable sps = query.var();
        Variable emissionIRI = query.var();
        Variable ocgmlIRI = query.var();
        GraphPattern gp = GraphPatterns.and(sps.isA(STATIC_POINT_SOURCE).andHas(EMITS, emissionIRI),
                emissionIRI.has(HAS_OCGML_OBJECT, ocgmlIRI));
        query.select(ocgmlIRI).where(gp);
        JSONArray pointSourceIRI = storeClient
                .executeQuery(query.getQueryString().replace("SELECT", "SELECT DISTINCT"));
        List<String> pointSourceIRIList = new ArrayList<>();
        for (int i = 0; i < pointSourceIRI.length(); i++) {
            String ontoCityGMLIRI = pointSourceIRI.getJSONObject(i).getString(ocgmlIRI.getQueryString().substring(1));
            pointSourceIRIList.add(ontoCityGMLIRI);
        }

        return pointSourceIRIList;

    }

    String getNamespaceCRS(String namespace) {
        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable crs = query.var();
        Variable test = query.var();
        GraphPattern gp = test.has(HAS_SRS_NAME, crs);
        query.select(crs).where(gp);
        JSONArray queryResult = AccessAgentCaller.queryStore(namespace, query.getQueryString());
        return queryResult.getJSONObject(0).getString(crs.getQueryString().substring(1));
    }

    public void setcitiesNamespaceCRS(String citiesNamespace, String namespaceCRS) {
        this.citiesNamespace = citiesNamespace;
        this.namespaceCRS = namespaceCRS;
    }

    private List<String> getIRIofStaticPointSourcesWithinScope(Polygon scope)
            throws org.apache.jena.sparql.lang.sparql_11.ParseException {

        List<String> pointSourceIRIAll = queryStaticPointSources();
        List<String> pointSourceIRIList = new ArrayList<>();
        Map<String, String> pointSourceIRIMap = new HashMap<>();

        for (int i = 0; i < pointSourceIRIAll.size(); i++) {
            String psIRI = pointSourceIRIAll.get(i);
            if (psIRI.contains("cityfurniture")) {
                String coIRI = psIRI.replace("cityfurniture", "cityobject");
                pointSourceIRIList.add(coIRI);
                pointSourceIRIMap.put(coIRI, psIRI);
            } else if (psIRI.contains("building")) {
                String coIRI = psIRI.replace("building", "cityobject");
                pointSourceIRIList.add(coIRI);
                pointSourceIRIMap.put(coIRI, psIRI);
            } else {
                LOGGER.warn(
                        "The following OCGML IRI of a static point source does not reference a cityfurniture or building object: "
                                +
                                psIRI);
            }

        }

        Coordinate[] scopeCoordinates = scope.getCoordinates();
        double xMin = scopeCoordinates[0].x;
        double yMin = scopeCoordinates[0].y;
        double xMax = xMin;
        double yMax = yMin;
        for (int i = 0; i < scopeCoordinates.length; i++) {
            double xc = scopeCoordinates[i].x;
            double yc = scopeCoordinates[i].y;
            xMin = Math.min(xMin, xc);
            yMin = Math.min(yMin, yc);
            xMax = Math.max(xMax, xc);
            yMax = Math.max(yMax, yc);
        }

        // Convert coordinates to the CRS used in the namespace

        String scopeSrid = "EPSG:" + scope.getSRID();
        double[] xyMin = { xMin, yMin };
        double[] xyMax = { xMax, yMax };
        double[] xyMinCRS = CRSTransformer.transform(scopeSrid, namespaceCRS, xyMin);
        double[] xyMaxCRS = CRSTransformer.transform(scopeSrid, namespaceCRS, xyMax);

        // Perform geospatial SPARQL query to get IRIs of emitting points within scope.
        // This query only works correctly in the Pirmasens namespace where emitting
        // points are
        // instantiated in the cityobject graph.
        // The values of zMin and zMax are arbitrary.
        double zMin = 0.0;
        double zMax = 800.0;
        String lowerPoints = xyMinCRS[0] + "#" + xyMinCRS[1] + "#" + zMin + "#";
        String lowerBounds = lowerPoints + lowerPoints + lowerPoints + lowerPoints + lowerPoints;
        lowerBounds = lowerBounds.substring(0, lowerBounds.length() - 1);

        String upperPoints = xyMaxCRS[0] + "#" + xyMaxCRS[1] + "#" + zMax + "#";
        String upperBounds = upperPoints + upperPoints + upperPoints + upperPoints + upperPoints;
        upperBounds = upperBounds.substring(0, upperBounds.length() - 1);

        String geoUri = "http://www.bigdata.com/rdf/geospatial#";
        // where clause for geospatial search
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ocgml", ONTO_CITYGML)
                .addPrefix("geo", geoUri)
                .addWhere("?cityObject", "geo:predicate", "ocgml:EnvelopeType")
                .addWhere("?cityObject", "geo:searchDatatype", "<http://localhost/blazegraph/literals/POLYGON-3-15>")
                .addWhere("?cityObject", "geo:customFields", "X0#Y0#Z0#X1#Y1#Z1#X2#Y2#Z2#X3#Y3#Z3#X4#Y4#Z4")
                // PLACEHOLDER because lowerBounds and upperBounds would be otherwise added as
                // doubles, not strings
                .addWhere("?cityObject", "geo:customFieldsLowerBounds", "PLACEHOLDER" + lowerBounds)
                .addWhere("?cityObject", "geo:customFieldsUpperBounds", "PLACEHOLDER" + upperBounds);

        WhereBuilder wb3 = new WhereBuilder().addWhereValueVar("?cityobject",
                pointSourceIRIList.toArray(new String[0]));

        WhereBuilder wb4 = new WhereBuilder().addBind("?cityobject", "?cityObject");

        WhereBuilder wb5 = new WhereBuilder()
                .addPrefix("ocgml", ONTO_CITYGML)
                .addWhere("?cityObject", "ocgml:objectClassId", "?id")
                .addFilter("?id IN (21,26)");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?cityObject");

        Query query = sb.build();
        // add geospatial service
        ElementGroup body = new ElementGroup();
        // Filtering of IRIs in scope but not in pointSourceIRIList will be done within
        // the code.
        // It is very slow to do this step in SPARQL. Including a values valuse with
        // hundreds of elements
        // results in the query taking several minutes to complete.
        // body.addElement(wb3.build().getQueryPattern());
        // body.addElement(wb4.build().getQueryPattern());
        body.addElement(wb5.build().getQueryPattern());
        body.addElement(new ElementService(geoUri + "search", wb.build().getQueryPattern()));
        query.setQueryPattern(body);

        String queryString = query.toString().replace("PLACEHOLDER", "");
        queryString = queryString.replace("\"http", "<http");
        queryString = queryString.replace("/\"", "/>");
        JSONArray buildingIRIQueryResult = AccessAgentCaller.queryStore(citiesNamespace, queryString);

        List<String> pointSourceIRIWithinScope = new ArrayList<>();

        for (int i = 0; i < buildingIRIQueryResult.length(); i++) {
            String pointSourceIRI = buildingIRIQueryResult.getJSONObject(i).getString("cityObject");
            if (pointSourceIRIList.contains(pointSourceIRI))
                pointSourceIRIWithinScope.add(pointSourceIRIMap.get(pointSourceIRI));
        }

        return pointSourceIRIWithinScope;

    }

    public List<StaticPointSource> getStaticPointSourcesWithinScope(Polygon scope)
            throws org.apache.jena.sparql.lang.sparql_11.ParseException {

        List<String> pointSourceOCGMLIRIWithinScope = getIRIofStaticPointSourcesWithinScope(scope);
        SelectQuery query = Queries.SELECT().prefix(P_DISP, P_OM);
        Variable ocgmlIRI = query.var();
        Variable sps = query.var();
        Variable emissionIRI = query.var();
        Variable pollutant = query.var();
        Variable massFlowIRI = query.var();
        Variable densityIRI = query.var();
        Variable temperatureIRI = query.var();
        Variable emissionValue = query.var();
        Variable densityValue = query.var();
        Variable temperatureValue = query.var();
        Variable emissionUnit = query.var();
        Variable densityUnit = query.var();
        Variable temperatureUnit = query.var();

        GraphPattern gp = GraphPatterns.and(sps.isA(STATIC_POINT_SOURCE).andHas(EMITS, emissionIRI),
                emissionIRI.isA(pollutant).andHas(HAS_OCGML_OBJECT, ocgmlIRI).andHas(HAS_QTY, massFlowIRI)
                        .andHas(HAS_QTY, densityIRI).andHas(HAS_QTY, temperatureIRI),
                massFlowIRI.isA(iri(MASS_FLOW)).andHas(HAS_NUMERICALVALUE, emissionValue).andHas(HAS_UNIT,
                        emissionUnit),
                densityIRI.isA(iri(DENSITY)).andHas(HAS_NUMERICALVALUE, densityValue).andHas(HAS_UNIT, densityUnit),
                temperatureIRI.isA(iri(TEMPERATURE)).andHas(HAS_NUMERICALVALUE, temperatureValue).andHas(HAS_UNIT,
                        temperatureUnit));
        ValuesPattern<Iri> vp = new ValuesPattern<>(ocgmlIRI,
                pointSourceOCGMLIRIWithinScope.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        query.select(sps, ocgmlIRI, pollutant, emissionValue, emissionUnit, densityValue, densityUnit, temperatureValue,
                temperatureUnit).where(gp, vp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        Map<String, StaticPointSource> iriToPointSourceMap = new HashMap<>();

        for (int i = 0; i < queryResult.length(); i++) {
            String spsIRI = queryResult.getJSONObject(i).getString(sps.getQueryString().substring(1));
            String pollutantID = queryResult.getJSONObject(i).getString(pollutant.getQueryString().substring(1));
            String ontoCityGMLIRI = queryResult.getJSONObject(i).getString(ocgmlIRI.getQueryString().substring(1));
            double emission = queryResult.getJSONObject(i).getDouble(emissionValue.getQueryString().substring(1));
            double density = queryResult.getJSONObject(i).getDouble(densityValue.getQueryString().substring(1));
            double temperature = queryResult.getJSONObject(i).getDouble(temperatureValue.getQueryString().substring(1));
            String emissionUnitString = queryResult.getJSONObject(i)
                    .getString(emissionUnit.getQueryString().substring(1));
            String densityUnitString = queryResult.getJSONObject(i)
                    .getString(densityUnit.getQueryString().substring(1));
            String temperatureUnitString = queryResult.getJSONObject(i)
                    .getString(temperatureUnit.getQueryString().substring(1));

            StaticPointSource pointSource;

            // Check the units when setting the flow rates, density and temperature.
            boolean correctUnits = true;

            if (!emissionUnitString.equals(UNIT_KG_S)) {
                String msg1 = "Unexpected emission units for static point source with IRI " + spsIRI;
                String msg2 = "Pollutant type is " + pollutantID;
                String msg = msg1 + msg2;
                LOGGER.warn(msg);
                correctUnits = false;
            }

            if (!densityUnitString.equals(UNIT_KG_M3)) {
                String msg1 = "Unexpected density units for static point source with IRI " + spsIRI;
                String msg2 = "Pollutant type is " + pollutantID;
                String msg = msg1 + msg2;
                LOGGER.warn(msg);
                correctUnits = false;
            }

            if (!temperatureUnitString.equals(UNIT_KELVIN)) {
                String msg1 = "Unexpected temperature units for static point source with IRI " + spsIRI;
                String msg2 = "Pollutant type is " + pollutantID;
                String msg = msg1 + msg2;
                LOGGER.warn(msg);
                correctUnits = false;
            }

            if (!correctUnits)
                continue;

            if (iriToPointSourceMap.containsKey(spsIRI)) {
                pointSource = iriToPointSourceMap.get(spsIRI);
            } else {
                pointSource = new StaticPointSource(spsIRI);
                pointSource.setOcgmlIri(ontoCityGMLIRI);
                pointSource.setMixtureDensityInKgm3(density);
                pointSource.setMixtureTemperatureInKelvin(temperature);
                iriToPointSourceMap.put(spsIRI, pointSource);
            }

            switch (pollutantID) {
                case CO2:
                    pointSource.setFlowRateCO2InKgPerS(emission);
                    break;
                case NO_X:
                    pointSource.setFlowRateNOxInKgPerS(emission);
                    break;
                case SO2:
                    pointSource.setFlowRateSO2InKgPerS(emission);
                    break;
                case CO:
                    pointSource.setFlowRateCOInKgPerS(emission);
                    break;
                case UHC:
                    pointSource.setFlowRateHCInKgPerS(emission);
                    break;
                case PM10:
                    pointSource.setFlowRatePM10InKgPerS(emission);
                    break;
                case PM25:
                    pointSource.setFlowRatePM25InKgPerS(emission);
                    break;
                default:
                    LOGGER.info("Unknown pollutant ID encountered in AermodAgent/QueryClient class: " + pollutantID);

            }

        }

        return iriToPointSourceMap.values().stream().collect(Collectors.toList());

    }

    List<Ship> getShipsWithinTimeAndScopeViaTsClient(long simulationTime, Geometry scope) {
        long simTimeUpperBound = simulationTime + 1800; // +30 minutes
        long simTimeLowerBound = simulationTime - 1800; // -30 minutes

        Map<String, String> measureToShipMap = getMeasureToShipMap();
        List<String> measures = new ArrayList<>(measureToShipMap.keySet());

        List<Ship> ships = new ArrayList<>();
        try (Connection conn = rdbStoreClient.getConnection()) {
            measures.stream().forEach(measure -> {
                TimeSeries<Long> ts = tsClientLong.getTimeSeriesWithinBounds(List.of(measure), simTimeLowerBound,
                        simTimeUpperBound, conn);
                if (ts.getValuesAsPoint(measure).size() > 1) {
                    LOGGER.warn("More than 1 point within this time inverval");
                } else if (ts.getValuesAsPoint(measure).isEmpty()) {
                    return;
                }

                try {
                    // this is to convert from org.postgis.Point to the Geometry class
                    org.postgis.Point postgisPoint = ts.getValuesAsPoint(measure).get(0);
                    String wktLiteral = postgisPoint.getTypeString() + postgisPoint.getValue();

                    Point point = (Point) new org.locationtech.jts.io.WKTReader().read(wktLiteral);

                    if (scope.covers(point)) {
                        // measureToShipMap.get(measure) gives the iri
                        Ship ship = new Ship(measureToShipMap.get(measure));
                        ship.setLocation(point);
                        ships.add(ship);
                    }
                } catch (ParseException e) {
                    LOGGER.error("Failed to parse WKT literal of point");
                    LOGGER.error(e.getMessage());
                    return;
                }

            });
        } catch (SQLException e) {
            LOGGER.error("Probably failed at closing connection");
            LOGGER.error(e.getMessage());
        }

        return ships;
    }

    /**
     * the result is the geo:wktLiteral type with IRI of SRID in front
     * 
     * @param scopeIri
     * @return
     */
    Polygon getScopeFromOntop(String scopeIri) {
        SelectQuery query = Queries.SELECT();
        Variable scope = query.var();

        query.prefix(P_GEO).where(iri(scopeIri).has(PropertyPaths.path(HAS_GEOMETRY, AS_WKT), scope));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        String wktLiteral = queryResult.getJSONObject(0).getString(scope.getQueryString().substring(1));
        Geometry scopePolygon = WKTReader.extract(wktLiteral).getGeometry();
        scopePolygon.setSRID(4326);
        return (Polygon) scopePolygon;
    }

    Map<String, String> getMeasureToShipMap() {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();
        Variable locationMeasure = query.var();
        Variable property = query.var();

        GraphPattern gp = GraphPatterns.and(ship.isA(SHIP).andHas(HAS_PROPERTY, property),
                property.isA(LOCATION).andHas(HAS_VALUE, locationMeasure));

        query.where(gp).prefix(P_OM, P_DISP);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        Map<String, String> locationMeasureToShipMap = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String locationMeasureIri = queryResult.getJSONObject(i)
                    .getString(locationMeasure.getQueryString().substring(1));
            String shipIri = queryResult.getJSONObject(i).getString(ship.getQueryString().substring(1));
            locationMeasureToShipMap.put(locationMeasureIri, shipIri);
        }

        return locationMeasureToShipMap;
    }

    void setEmissions(List<PointSource> allSources) {
        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable pointSource = query.var();
        Variable entity = query.var();
        Variable entityType = query.var();
        Variable quantity = query.var();
        Variable quantityType = query.var();
        Variable numericalValue = query.var();

        ValuesPattern<Iri> sourceValues = new ValuesPattern<>(pointSource,
                allSources.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);

        GraphPattern gp = GraphPatterns.and(derivation.has(IS_DERIVED_FROM, pointSource),
                entity.has(BELONGS_TO, derivation).andIsA(entityType)
                        .andHas(HAS_QUANTITY, quantity),
                quantity.isA(quantityType).andHas(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), numericalValue));

        query.where(gp, sourceValues).prefix(P_OM);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        // create a look up map to get point source object based on IRI
        Map<String, PointSource> iriToSourceMap = new HashMap<>();
        allSources.stream().forEach(s -> iriToSourceMap.put(s.getIri(), s));

        for (int i = 0; i < queryResult.length(); i++) {
            String sourceIri = queryResult.getJSONObject(i).getString(pointSource.getQueryString().substring(1));
            double literalValue = queryResult.getJSONObject(i).getDouble(numericalValue.getQueryString().substring(1));
            PointSource sourceObject = iriToSourceMap.get(sourceIri);

            String entityTypeIri = queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1));
            String quantityTypeIri = queryResult.getJSONObject(i).getString(quantityType.getQueryString().substring(1));

            if (entityTypeIri.contentEquals(PM10) && quantityTypeIri.contentEquals(MASS_FLOW)) {
                // PM10 flowrate
                sourceObject.setFlowRatePM10InKgPerS(literalValue);
            } else if (entityTypeIri.contentEquals(PM25) && quantityTypeIri.contentEquals(MASS_FLOW)) {
                // PM2.5 flowrate
                sourceObject.setFlowRatePM25InKgPerS(literalValue);
            } else if (entityTypeIri.contentEquals(PM25) && quantityTypeIri.contentEquals(DENSITY)) {
                // particle density
                sourceObject.setParticleDensity(literalValue);
            } else if (entityTypeIri.contentEquals(SO2) && quantityTypeIri.contentEquals(TEMPERATURE)) {
                // all gas mixtures share the same temperature
                sourceObject.setMixtureTemperatureInKelvin(literalValue);
            } else if (entityTypeIri.contentEquals(SO2) && quantityTypeIri.contentEquals(DENSITY)) {
                // all gas mixtures share the same density
                sourceObject.setMixtureDensityInKgm3(literalValue);
            } else if (entityTypeIri.contentEquals(SO2) && quantityTypeIri.contentEquals(MASS_FLOW)) {
                sourceObject.setFlowRateSO2InKgPerS(literalValue);
            } else if (entityTypeIri.contentEquals(NO_X) && quantityTypeIri.contentEquals(MASS_FLOW)) {
                sourceObject.setFlowRateNOxInKgPerS(literalValue);
            }

        }
    }

    /**
     * returns derivation IRIs for each ship by querying
     * <derivation> isDerivedFrom <ship>
     * 
     * @param ships
     * @return
     */
    List<String> getDerivationsOfPointSources(List<PointSource> allSources) {
        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable pointSource = query.var();
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        ValuesPattern<Iri> vp = new ValuesPattern<>(pointSource,
                allSources.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = derivation.has(isDerivedFrom, pointSource);

        query.where(gp, vp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        List<String> derivations = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            derivations.add(queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1)));
        }

        return derivations;
    }

    // SPARQL query for weather data instantiated by weather agent as a timeseries.
    // Each parameter has a single value.
    WeatherData getWeatherData(String station, long timestamp) {
        SelectQuery query = Queries.SELECT();

        Variable weatherType = query.var();
        Variable quantity = query.var();
        Variable measure = query.var();
        Variable weatherUnit = query.var();

        // RDF types for weather data
        List<String> weatherTypeList = List.of(CLOUD_COVER, AIR_TEMPERATURE, RELATIVE_HUMIDITY, WIND_SPEED,
                WIND_DIRECTION);

        ValuesPattern<Iri> vp = new ValuesPattern<>(weatherType,
                weatherTypeList.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);

        // ValuesPattern vp = new ValuesPattern(weatherType, weatherUnit);
        // weatherTypeList.stream().forEach(type ->
        // vp.addValuePairForMultipleVariables(iri(type), UNIT_MAP.get(type)));

        GraphPattern gp = GraphPatterns.and(iri(station).has(REPORTS, quantity),
                quantity.isA(weatherType).andHas(HAS_VALUE, measure), measure.has(HAS_UNIT, weatherUnit));

        query.prefix(P_OM, P_EMS).where(gp, vp);

        Map<String, String> typeToMeasureMap = new HashMap<>();
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        for (int i = 0; i < queryResult.length(); i++) {
            String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
            String weatherTypeIri = queryResult.getJSONObject(i).getString(weatherType.getQueryString().substring(1));

            typeToMeasureMap.put(weatherTypeIri, measureIri);
        }

        TimeSeries<Instant> ts;
        try (Connection conn = rdbStoreClient.getConnection()) {
            Instant timestampAsInstant = Instant.ofEpochSecond(timestamp);
            ts = tsClientInstant.getTimeSeriesWithinBounds(
                    typeToMeasureMap.values().stream().collect(Collectors.toList()), timestampAsInstant,
                    timestampAsInstant, conn);
        } catch (SQLException e) {
            String errmsg = "Failed to obtain time series from weather station";
            LOGGER.fatal(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        WeatherData weatherData = new WeatherData();
        weatherTypeList.stream().forEach(type -> {
            double value = ts.getValuesAsDouble(typeToMeasureMap.get(type)).get(0);
            switch (type) {
                case CLOUD_COVER:
                    weatherData.setCloudCoverInPercentage(value);
                    break;
                case AIR_TEMPERATURE:
                    weatherData.setTemperatureInCelcius(value);
                    break;
                case RELATIVE_HUMIDITY:
                    weatherData.setHumidityInPercentage(value);
                    break;
                case WIND_SPEED:
                    weatherData.setWindSpeedInMetreSecond(value);
                    break;
                case WIND_DIRECTION:
                    weatherData.setWindDirectionInDegrees(value);
                    break;
                default:
                    LOGGER.error("Unknown weather RDF type: <{}>", type);
            }
        });
        return weatherData;
    }

    /**
     * Create a polygon with the given points
     * 
     * @param points points of the polygon as a string
     * @return a polygon
     */
    private Polygon toPolygon(String points) {
        int ind = 0;
        GeometryFactory gF = new GeometryFactory();

        String[] arr = points.split("#");

        Coordinate[] coordinates = new Coordinate[(arr.length) / 3];

        for (int i = 0; i < arr.length; i += 3) {
            coordinates[ind] = new Coordinate(Double.valueOf(arr[i]), Double.valueOf(arr[i + 1]),
                    Double.valueOf(arr[i + 2]));
            ind++;
        }

        return gF.createPolygon(coordinates);
    }

    public Map<String, List<Polygon>> cityFurnitureQuery(List<String> ocgmlIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable geometricIRI = SparqlBuilder.var("geometricIRI");
        Variable polygonData = SparqlBuilder.var("polygonData");
        Variable objectIRI = SparqlBuilder.var("objectIRI");

        GraphPattern gp = GraphPatterns
                .and(geometricIRI.has(OCGML_GEOM, polygonData).andHas(OCGML_CITYOBJECT, objectIRI))
                .filter(Expressions.not(Expressions.function(SparqlFunction.IS_BLANK, polygonData)));
        ValuesPattern<Iri> vp = new ValuesPattern<>(objectIRI,
                ocgmlIRI.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        query.select(polygonData, objectIRI).where(gp, vp).orderBy(objectIRI);
        JSONArray queryResult = AccessAgentCaller.queryStore(citiesNamespace, query.getQueryString());

        Map<String, List<Polygon>> iriToPolygonMap = new HashMap<>();

        for (int i = 0; i < queryResult.length(); i++) {
            String ontoCityGMLIRI = queryResult.getJSONObject(i).getString(objectIRI.getQueryString().substring(1));
            String polygonString = queryResult.getJSONObject(i).getString(polygonData.getQueryString().substring(1));
            if (iriToPolygonMap.containsKey(ontoCityGMLIRI)) {
                List<Polygon> polyList = iriToPolygonMap.get(ontoCityGMLIRI);
                polyList.add(toPolygon(polygonString));
                iriToPolygonMap.put(ontoCityGMLIRI, polyList);
            } else {
                List<Polygon> polyList = new ArrayList<>();
                polyList.add(toPolygon(polygonString));
                iriToPolygonMap.put(ontoCityGMLIRI, polyList);
            }
        }

        return iriToPolygonMap;
    }

    /**
     * Returns the ground geometry's exterior ring
     * 
     * @param geometry    ground geometry
     * @param polygonType polygon datatype, such as "<...\POLYGON-3-45-15>"
     * @return ground geometry with no holes
     */
    private String ignoreHole(String geometry, String polygonType) {
        int num;
        int ind;
        int count = 1;

        String[] split = polygonType.split("-");

        if (split.length < 4) {
            return geometry;
        }

        num = Integer.parseInt(split[2]);

        ind = geometry.indexOf("#");

        while (count != num) {
            ind = geometry.indexOf("#", ind + 1);
            count++;
        }
        return geometry.substring(0, ind);
    }

    public Map<String, List<List<Polygon>>> buildingsQuery(List<String> ocgmlIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable geometricIRI = SparqlBuilder.var("geometricIRI");
        Variable polygonData = SparqlBuilder.var("polygonData");
        Variable objectIRI = SparqlBuilder.var("objectIRI");
        Variable surfaceIRI = SparqlBuilder.var("surfaceIRI");
        Variable datatype = SparqlBuilder.var("datatype");
        Variable objectClassId = SparqlBuilder.var("objectClassId");

        // RdfLiteral.NumericLiteral objectClassId = Rdf.literalOf(35);
        List<Integer> objectClassIdValues = new ArrayList<>(Arrays.asList(33, 35));

        Expression dataType = Expressions.function(SparqlFunction.DATATYPE, polygonData);

        GraphPattern gp = GraphPatterns
                .and(surfaceIRI.has(OCGML_GEOM, polygonData).andHas(OCGML_CITYOBJECT, geometricIRI),
                        geometricIRI.has(OCGML_OBJECTCLASSID, objectClassId).andHas(OCGML_BUILDINGID, objectIRI))
                .filter(Expressions.not(Expressions.function(SparqlFunction.IS_BLANK, polygonData)));
        ValuesPattern<Iri> vp = new ValuesPattern<>(objectIRI,
                ocgmlIRI.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        ValuesPattern<Integer> vp2 = new ValuesPattern<>(objectClassId, objectClassIdValues, Integer.class);

        query.select(polygonData, objectIRI, dataType.as(datatype), objectClassId).where(gp, vp, vp2).orderBy(objectIRI,
                objectClassId);
        JSONArray queryResult = AccessAgentCaller.queryStore(citiesNamespace, query.getQueryString());

        Map<String, List<List<Polygon>>> iriToPolygonMap = new HashMap<>();

        for (int i = 0; i < queryResult.length(); i++) {
            String ontoCityGMLIRI = queryResult.getJSONObject(i).getString(objectIRI.getQueryString().substring(1));
            String polygonString = queryResult.getJSONObject(i).getString(polygonData.getQueryString().substring(1));
            String objectClassString = queryResult.getJSONObject(i)
                    .getString(objectClassId.getQueryString().substring(1));
            int objectClass = Integer.parseInt(objectClassString);
            String dataString = queryResult.getJSONObject(i).getString(datatype.getQueryString().substring(1));
            if (iriToPolygonMap.containsKey(ontoCityGMLIRI)) {
                List<List<Polygon>> polyList = iriToPolygonMap.get(ontoCityGMLIRI);
                if (objectClass == 35)
                    polyList.get(0).add(toPolygon(ignoreHole(polygonString, dataString)));
                else if (objectClass == 33)
                    polyList.get(1).add(toPolygon(polygonString));
                iriToPolygonMap.put(ontoCityGMLIRI, polyList);
            } else {
                List<List<Polygon>> polyList = new ArrayList<>();
                polyList.add(new ArrayList<>());
                polyList.add(new ArrayList<>());
                if (objectClass == 35)
                    polyList.get(0).add(toPolygon(ignoreHole(polygonString, dataString)));
                else if (objectClass == 33)
                    polyList.get(1).add(toPolygon(polygonString));
                iriToPolygonMap.put(ontoCityGMLIRI, polyList);
            }
        }

        return iriToPolygonMap;
    }

    public Map<String, List<List<Polygon>>> getBuildingsNearPollutantSources(List<StaticPointSource> allSources)
            throws org.apache.jena.sparql.lang.sparql_11.ParseException {

        List<String> cityObjectIRIList = allSources.stream()
                .map(i -> i.getOcgmlIri().replace("building", "cityobject").replace("cityfurniture", "cityobject"))
                .collect(Collectors.toList());

        List<String> boxBounds = getBoundingBoxofPointSources(allSources);

        String lowerBounds = boxBounds.get(0);
        String upperBounds = boxBounds.get(1);

        String geoUri = "http://www.bigdata.com/rdf/geospatial#";
        // where clause for geospatial search
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ocgml", ONTO_CITYGML)
                .addPrefix("geo", geoUri)
                .addWhere("?cityObject", "geo:predicate", "ocgml:EnvelopeType")
                .addWhere("?cityObject", "geo:searchDatatype", "<http://localhost/blazegraph/literals/POLYGON-3-15>")
                .addWhere("?cityObject", "geo:customFields", "X0#Y0#Z0#X1#Y1#Z1#X2#Y2#Z2#X3#Y3#Z3#X4#Y4#Z4")
                // PLACEHOLDER because lowerBounds and upperBounds would be otherwise added as
                // doubles, not strings
                .addWhere("?cityObject", "geo:customFieldsLowerBounds", "PLACEHOLDER" + lowerBounds)
                .addWhere("?cityObject", "geo:customFieldsUpperBounds", "PLACEHOLDER" + upperBounds);

        // where clause to check that the city object is a building
        WhereBuilder wb2 = new WhereBuilder()
                .addPrefix("ocgml", ONTO_CITYGML)
                .addWhere("?cityObject", "ocgml:objectClassId", "?id")
                .addFilter("?id=26");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?cityObject");

        Query query = sb.build();
        // add geospatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(geoUri + "search", wb.build().getQueryPattern()));
        body.addElement(wb2.build().getQueryPattern());
        query.setQueryPattern(body);

        String queryString = query.toString().replace("PLACEHOLDER", "");
        JSONArray buildingIRIQueryResult = AccessAgentCaller.queryStore(citiesNamespace, queryString);

        List<String> buildingOCGMLIRIList = new ArrayList<>();

        for (int i = 0; i < buildingIRIQueryResult.length(); i++) {
            String cityObjectIRI = buildingIRIQueryResult.getJSONObject(i).getString("cityObject");
            if (!cityObjectIRIList.contains(cityObjectIRI))
                buildingOCGMLIRIList.add(cityObjectIRI.replace("cityobject", "building"));
        }

        Map<String, List<List<Polygon>>> iriToPolygonMap = buildingsQuery(buildingOCGMLIRIList);

        return iriToPolygonMap;

    }

    private List<String> getBoundingBoxofPointSources(List<StaticPointSource> allSources) {
        // Determine bounding box for geospatial query by finding the minimum and
        // maximum x and y coordinates of pollutant sources
        // in the original coordinate system.
        double xMin = 0.0;
        double xMax = 0.0;
        double yMin = 0.0;
        double yMax = 0.0;

        for (int i = 0; i < allSources.size(); i++) {
            PointSource ps = allSources.get(i);
            Point location = ps.getLocation();
            double[] xyOriginal = { location.getX(), location.getY() };
            double[] xyTransformed = CRSTransformer.transform("EPSG:" + location.getSRID(), namespaceCRS, xyOriginal);

            if (i == 0) {
                xMin = xyTransformed[0];
                xMax = xMin;
                yMin = xyTransformed[1];
                yMax = yMin;
            } else {
                xMin = Math.min(xMin, xyTransformed[0]);
                xMax = Math.max(xMax, xyTransformed[0]);
                yMin = Math.min(yMin, xyTransformed[1]);
                yMax = Math.max(yMax, xyTransformed[1]);
            }
        }

        if (allSources.size() == 1) {
            double expandRange = 10.0;
            xMin -= expandRange;
            xMax += expandRange;
            yMin -= expandRange;
            yMax += expandRange;
        }

        double zMax = 800.0;

        String polygonPoints = String.valueOf(xMin) + "#" + String.valueOf(yMin) + "#" + String.valueOf(zMax) +
                "#" + String.valueOf(xMax) + "#" + String.valueOf(yMin) + "#" + String.valueOf(zMax) + "#"
                + String.valueOf(xMax) +
                "#" + String.valueOf(yMax) + String.valueOf("#") + String.valueOf(zMax) + "#" + String.valueOf(xMin) +
                "#" + String.valueOf(yMax) + "#" + String.valueOf(zMax) + "#" + String.valueOf(xMin) + "#"
                + String.valueOf(yMin) + "#" +
                String.valueOf(zMax);

        double buffer = 200.0;

        Polygon envelopePolygon = (Polygon) toPolygon(polygonPoints);

        Geometry surroundingRing = ((Polygon) inflatePolygon(envelopePolygon, buffer)).getExteriorRing();

        Coordinate[] surroundingCoordinates = surroundingRing.getCoordinates();

        String boundingBox = coordinatesToString(surroundingCoordinates);

        String[] points = boundingBox.split("#");

        String lowerPoints = points[0] + "#" + points[1] + "#" + 0 + "#";

        String lowerBounds = lowerPoints + lowerPoints + lowerPoints + lowerPoints + lowerPoints;
        lowerBounds = lowerBounds.substring(0, lowerBounds.length() - 1);

        String upperPoints = points[6] + "#" + points[7] + "#" + String.valueOf(Double.parseDouble(points[8]) + 100)
                + "#";

        String upperBounds = upperPoints + upperPoints + upperPoints + upperPoints + upperPoints;
        upperBounds = upperBounds.substring(0, upperBounds.length() - 1);

        return Arrays.asList(lowerBounds, upperBounds);

    }

    /**
     * Inflates a polygon
     * 
     * @param geom     polygon geometry
     * @param distance buffer distance
     * @return inflated polygon
     */
    private Geometry inflatePolygon(Geometry geom, Double distance) {
        ArrayList<Double> zCoordinate = getPolygonZ(geom);
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance, bufferParameters);
        buffered.setUserData(geom.getUserData());
        setPolygonZ(buffered, zCoordinate);
        return buffered;
    }

    /**
     * Converts an array of coordinates into a string
     * 
     * @param coordinates array of footprint coordinates
     * @return coordinates as a string
     */
    private String coordinatesToString(Coordinate[] coordinates) {
        String output = "";

        for (int i = 0; i < coordinates.length; i++) {
            output = output + "#" + Double.toString(coordinates[i].getX()) + "#"
                    + Double.toString(coordinates[i].getY()) + "#" + Double.toString(coordinates[i].getZ());
        }

        return output.substring(1, output.length());
    }

    /**
     * Extract the z coordinates of the polygon vertices
     * 
     * @param geom polygon geometry
     * @return the z coordinates of the polygon vertices
     */
    private static ArrayList<Double> getPolygonZ(Geometry geom) {
        Coordinate[] coordinates = geom.getCoordinates();
        ArrayList<Double> output = new ArrayList<>();

        for (int i = 0; i < coordinates.length; i++) {
            output.add(coordinates[i].getZ());
        }

        return output;
    }

    /**
     * Sets a polygon's z coordinates to the values from zInput
     * 
     * @param geom   polygon geometry
     * @param zInput ArrayList of values representing z coordinates
     */
    private void setPolygonZ(Geometry geom, ArrayList<Double> zInput) {
        Double newZ = Double.NaN;

        for (int i = 0; i < zInput.size(); i++) {
            if (!zInput.get(i).isNaN()) {
                newZ = zInput.get(i);
                break;
            }
        }

        if (newZ.isNaN()) {
            newZ = 10.0;
        }

        if (geom.getNumPoints() < zInput.size()) {
            while (geom.getNumPoints() != zInput.size()) {
                zInput.remove(zInput.size() - 1);
            }
        } else {
            while (geom.getNumPoints() != zInput.size()) {
                zInput.add(1, newZ);
            }
        }

        Collections.replaceAll(zInput, Double.NaN, newZ);
        geom.apply(new CoordinateSequenceFilter() {
            @Override
            public void filter(CoordinateSequence cSeq, int i) {
                cSeq.getCoordinate(i).setZ(zInput.get(i));
            }

            @Override
            public boolean isDone() {
                return false;
            }

            @Override
            public boolean isGeometryChanged() {
                return false;
            }
        });
    }

    public void setElevation(List<StaticPointSource> pointSources, List<Building> buildings, int simulationSrid) {

        for (int i = 0; i < pointSources.size(); i++) {
            StaticPointSource ps = pointSources.get(i);
            String originalSrid = "EPSG:" + ps.getLocation().getSRID();
            double[] xyOriginal = { ps.getLocation().getX(), ps.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);
            String sqlString = String.format("SELECT ST_Value(rast, ST_SetSRID(ST_MakePoint(%f,%f),%d)) AS val " +
                    "FROM elevation " + "WHERE ST_Intersects(rast, ST_SetSRID(ST_MakePoint(%f,%f),%d));",
                    xyTransformed[0], xyTransformed[1], simulationSrid, xyTransformed[0], xyTransformed[1],
                    simulationSrid);

            try (Statement stmt = rdbStoreClient.getConnection().createStatement()) {
                ResultSet result = stmt.executeQuery(sqlString);
                double elevation = result.getDouble("val");
                ps.setElevation(elevation);
            } catch (SQLException e) {
                LOGGER.error(e.getMessage());
            }

        }

        for (int i = 0; i < buildings.size(); i++) {
            Building building = buildings.get(i);
            String originalSrid = building.getSrid();
            double[] xyOriginal = { building.getLocation().getX(), building.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);
            String sqlString = String.format("SELECT ST_Value(rast, ST_SetSRID(ST_MakePoint(%f,%f),%d)) AS val" +
                    "FROM elevation" + "WHERE ST_Intersects(rast, ST_SetSRID(ST_MakePoint(%f,%f),%d));",
                    xyTransformed[0], xyTransformed[1], simulationSrid, xyTransformed[0], xyTransformed[1],
                    simulationSrid);

            try (Statement stmt = rdbStoreClient.getConnection().createStatement()) {
                ResultSet result = stmt.executeQuery(sqlString);
                double elevation = result.getDouble("val");
                building.setElevation(elevation);
            } catch (SQLException e) {
                LOGGER.error(e.getMessage());
            }

        }

    }

    void updateOutputs(String derivation, String dispersionMatrix, String dispersionLayer, String shipLayer,
            long timeStamp) {
        // first query the IRIs
        SelectQuery query = Queries.SELECT();

        Variable entity = query.var();
        Variable entityType = query.var();

        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        query.where(entity.has(belongsTo, iri(derivation)).andIsA(entityType)).prefix(P_DISP).select(entity, entityType)
                .distinct();

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        String dispersionMatrixIri = null;
        String dispersionLayerIri = null;
        String shipLayerIri = null;
        for (int i = 0; i < queryResult.length(); i++) {
            String entityTypeIri = queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1));

            switch (entityTypeIri) {
                case DISPERSION_MATRIX:
                    dispersionMatrixIri = queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1));
                    break;
                case DISPERSION_LAYER:
                    dispersionLayerIri = queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1));
                    break;
                case SHIPS_LAYER:
                    shipLayerIri = queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1));
                    break;
                default:
                    LOGGER.error("Unknown entity type: <{}>", entityType);
                    return;
            }
        }

        if (dispersionMatrixIri == null || dispersionLayerIri == null || shipLayerIri == null) {
            LOGGER.error("One of dispersion matrix, dispersion layer, ship layer IRI is null");
            return;
        }

        List<List<?>> values = new ArrayList<>();
        values.add(List.of(dispersionMatrix));
        values.add(List.of(dispersionLayer));
        values.add(List.of(shipLayer));

        TimeSeries<Long> timeSeries = new TimeSeries<>(List.of(timeStamp),
                List.of(dispersionMatrixIri, dispersionLayerIri, shipLayerIri), values);

        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClientLong.addTimeSeriesData(timeSeries, conn);
        } catch (SQLException e) {
            LOGGER.error("Failed at closing connection");
            LOGGER.error(e.getMessage());
            return;
        }
    }

    @Deprecated
    List<String> getShipsWithinTimeAndScopeViaKG(long simulationTime, String scopeIri) {
        long simTimeUpperBound = simulationTime + 1800; // +30 minutes
        long simTimeLowerBound = simulationTime - 1800; // -30 minutes

        Map<String, String> measureToShipMap = getMeasureToShipMap();
        List<String> locationMeasures = new ArrayList<>(measureToShipMap.keySet());

        Map<String, String> geometryToMeasureMap = getGeometryToMeasureMap(locationMeasures);
        List<String> geometryIrisWithinTimeBounds = getGeometriesWithinTimeBounds(
                new ArrayList<>(geometryToMeasureMap.keySet()), simTimeLowerBound, simTimeUpperBound);
        String scopeGeometry = getScopeGeometry(scopeIri);
        List<String> geometriesWithinScopeAndTime = getGeometriesWithinScope(scopeGeometry,
                geometryIrisWithinTimeBounds);

        List<String> shipsWithinTimeAndScope = new ArrayList<>();
        geometriesWithinScopeAndTime
                .forEach(g -> shipsWithinTimeAndScope.add(measureToShipMap.get(geometryToMeasureMap.get(g))));

        return shipsWithinTimeAndScope;
    }

    /**
     * to ontop
     * 
     * @param scopeIri
     */
    @Deprecated
    String getScopeGeometry(String scopeIri) {
        SelectQuery query = Queries.SELECT();
        Variable scopeGeometry = query.var();
        query.prefix(P_GEO).where(iri(scopeIri).has(HAS_GEOMETRY, scopeGeometry));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        return queryResult.getJSONObject(0).getString(scopeGeometry.getQueryString().substring(1));
    }

    /**
     * from ontop
     * 
     * @param locationMeasures
     * @return
     */
    @Deprecated
    Map<String, String> getGeometryToMeasureMap(List<String> locationMeasures) {
        SelectQuery query = Queries.SELECT();
        Variable locationMeasure = query.var();
        Variable geometry = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(locationMeasure,
                locationMeasures.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = locationMeasure.has(HAS_GEOMETRY, geometry);

        query.prefix(P_GEO).where(vp, gp);

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());

        Map<String, String> geometryToMeasureMap = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String geometryIri = queryResult.getJSONObject(i).getString(geometry.getQueryString().substring(1));
            String measureIri = queryResult.getJSONObject(i).getString(locationMeasure.getQueryString().substring(1));
            geometryToMeasureMap.put(geometryIri, measureIri);
        }

        return geometryToMeasureMap;
    }

    /**
     * from ontop
     * 
     * @param geometryIris
     * @param simTimeLowerBound
     * @param simTimeUpperBound
     */
    @Deprecated
    List<String> getGeometriesWithinTimeBounds(List<String> geometryIris, long simTimeLowerBound,
            long simTimeUpperBound) {
        SelectQuery query = Queries.SELECT();

        Variable shipTime = query.var();
        Variable geometry = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(geometry,
                geometryIris.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = GraphPatterns.and(vp, geometry.has(P_DISP.iri("hasTime"), shipTime));

        query.prefix(P_GEO, P_GEOF, P_OM, P_DISP).where(gp.filter(Expressions
                .and(Expressions.gt(shipTime, simTimeLowerBound), Expressions.lt(shipTime, simTimeUpperBound))));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        List<String> geometriesWithinTimeBounds = new ArrayList<>();

        for (int i = 0; i < queryResult.length(); i++) {
            geometriesWithinTimeBounds
                    .add(queryResult.getJSONObject(i).getString(geometry.getQueryString().substring(1)));
        }

        return geometriesWithinTimeBounds;
    }

    @Deprecated
    List<String> getGeometriesWithinScope(String scopeGeometry, List<String> shipGeometriesWithinTimeBounds) {
        SelectQuery query = Queries.SELECT();
        Variable shipGeometry = query.var();
        Variable scopeWkt = query.var();
        Variable shipWkt = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(shipGeometry,
                shipGeometriesWithinTimeBounds.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);

        GraphPattern gp = GraphPatterns.and(vp, shipGeometry.has(AS_WKT, shipWkt),
                iri(scopeGeometry).has(AS_WKT, scopeWkt));

        query.where(gp.filter(Expressions.and(GeoSPARQL.sfIntersects(shipWkt, scopeWkt)))).prefix(P_GEO, P_GEOF);

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        List<String> geometriesWithinScope = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            geometriesWithinScope
                    .add(queryResult.getJSONObject(i).getString(shipGeometry.getQueryString().substring(1)));
        }
        return geometriesWithinScope;
    }
}
