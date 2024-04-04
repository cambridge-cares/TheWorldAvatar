package com.cmclinnovations.aermod;

import org.eclipse.rdf4j.model.vocabulary.GEOF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.TIME;
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
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.Point;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;

import com.cmclinnovations.aermod.sparqlbuilder.GeoSPARQL;
import com.cmclinnovations.aermod.sparqlbuilder.ServiceEndpoint;
import com.cmclinnovations.aermod.sparqlbuilder.ValuesPattern;
import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;

import java.util.Arrays;
import java.util.Collections;

import com.cmclinnovations.aermod.objects.Building;
import com.cmclinnovations.aermod.objects.DispersionOutput;
import com.cmclinnovations.aermod.objects.PointSource;
import com.cmclinnovations.aermod.objects.Pollutant;
import com.cmclinnovations.aermod.objects.Ship;
import com.cmclinnovations.aermod.objects.StaticPointSource;
import com.cmclinnovations.aermod.objects.WeatherData;
import com.cmclinnovations.aermod.objects.Pollutant.PollutantType;

import it.unibz.inf.ontop.model.vocabulary.GEO;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
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
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementService;

public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private TimeSeriesClient<Long> tsClientLong;
    private TimeSeriesClient<Instant> tsClientInstant;
    private String citiesNamespace;
    private String namespaceCRS;
    private ServiceEndpoint ontopService;

    // prefixes
    private static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final String PREFIX_DISP = "https://www.theworldavatar.com/kg/ontodispersion/";
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String CHEM = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#";
    public static final String ONTO_CITYGML = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";

    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX_DISP));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));

    private static final Prefix P_EMS = SparqlBuilder.prefix("ems", iri(ONTO_EMS));
    private static final Prefix P_OCGML = SparqlBuilder.prefix("ocgml", iri(ONTO_CITYGML));
    private static final Prefix P_GRP = SparqlBuilder.prefix("grp",
            iri("http://www.opengis.net/citygml/cityobjectgroup/2.0/"));
    private static final Prefix P_BLDG = SparqlBuilder.prefix("bldg",
            iri("http://www.opengis.net/citygml/building/2.0/"));
    private static final Prefix P_GEOF = SparqlBuilder.prefix("geof", iri(GEOF.NAMESPACE));

    // classes
    public static final String REPORTING_STATION = "https://www.theworldavatar.com/kg/ontoems/ReportingStation";
    public static final String NX = PREFIX_DISP + "nx";
    public static final String NY = PREFIX_DISP + "ny";
    public static final String Z = PREFIX_DISP + "z";
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

    private static final Iri STATIC_POINT_SOURCE = P_DISP.iri("StaticPointSource");

    // weather types
    private static final String CLOUD_COVER = ONTO_EMS + "CloudCover";
    private static final String AIR_TEMPERATURE = ONTO_EMS + "AirTemperature";
    private static final String RELATIVE_HUMIDITY = ONTO_EMS + "RelativeHumidity";
    private static final String WIND_SPEED = ONTO_EMS + "WindSpeed";
    private static final String WIND_DIRECTION = ONTO_EMS + "WindDirection";

    // IRI of units used
    private static final Iri UNIT_DEGREE = P_OM.iri("degree");
    private static final Iri UNIT_CELCIUS = P_OM.iri("degreeCelsius");
    private static final Iri UNIT_MS = P_OM.iri("metrePerSecond-Time");
    private static final Iri UNIT_PERCENTAGE = P_OM.iri("PercentageUnit");
    private static final String UNIT_KG_S = OM_STRING + "kilogramPerSecond-Time";
    private static final String UNIT_KELVIN = OM_STRING + "kelvin";
    private static final String UNIT_KG_M3 = OM_STRING + "kilogramPerCubicmetre";

    // Location type
    private static final Iri LOCATION = P_DISP.iri("Location");

    // outputs (belongsTo)
    private static final String SHIPS_LAYER = PREFIX_DISP + "ShipsLayer";
    private static final String STATIC_POINT_SOURCE_LAYER = PREFIX_DISP + "StaticPointSourceLayer";
    private static final String BUILDINGS_LAYER = PREFIX_DISP + "BuildingsLayer";
    private static final String ELEVATION_LAYER = PREFIX_DISP + "Elevation";

    // properties
    private static final Iri HAS_PROPERTY = P_DISP.iri("hasProperty");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri HAS_UNIT = P_OM.iri("hasUnit");
    private static final Iri HAS_GEOMETRY = P_GEO.iri("hasGeometry");
    private static final Iri AS_WKT = P_GEO.iri("asWKT");
    private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri HAS_NAME = P_DISP.iri("hasName");
    private static final Iri HAS_OCGML_OBJECT = P_DISP.iri("hasOntoCityGMLCityObject");
    private static final Iri EMITS = P_DISP.iri("emits");
    private static final Iri HAS_SRS_NAME = P_OCGML.iri("srsname");
    private static final Iri HAS_QTY = P_OM.iri("hasQuantity");
    private static final Iri OCGML_GEOM = P_OCGML.iri("GeometryType");
    private static final Iri OCGML_CITYOBJECT = P_OCGML.iri("cityObjectId");
    private static final Iri OCGML_OBJECTCLASSID = P_OCGML.iri("objectClassId");
    private static final Iri OCGML_BUILDINGID = P_OCGML.iri("buildingId");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");
    private static final Iri HAS_DISPERSION_MATRIX = P_DISP.iri("hasDispersionMatrix");
    private static final Iri HAS_DISPERSION_RASTER = P_DISP.iri("hasDispersionRaster");
    private static final Iri HAS_DISPERSION_COLOUR_BAR = P_DISP.iri("hasDispersionColourBar");
    private static final Iri HAS_HEIGHT = P_DISP.iri("hasHeight");
    private static final Iri LOD0_FOOTPRINT = P_BLDG.iri("lod0FootPrint");
    private static final Iri MEASURED_HEIGHT = P_BLDG.iri("measuredHeight");
    private static final Iri PARENT = P_GRP.iri("parent");

    // fixed units for each measured property
    private static final Map<String, Iri> UNIT_MAP = new HashMap<>();
    static {
        UNIT_MAP.put(CLOUD_COVER, UNIT_PERCENTAGE);
        UNIT_MAP.put(AIR_TEMPERATURE, UNIT_CELCIUS);
        UNIT_MAP.put(RELATIVE_HUMIDITY, UNIT_PERCENTAGE);
        UNIT_MAP.put(WIND_SPEED, UNIT_MS);
        UNIT_MAP.put(WIND_DIRECTION, UNIT_DEGREE);
    }

    public QueryClient(RemoteStoreClient storeClient, String ontopEndpoint, RemoteRDBStoreClient rdbStoreClient) {
        this.storeClient = storeClient;
        this.tsClientLong = new TimeSeriesClient<>(storeClient, Long.class);
        this.tsClientInstant = new TimeSeriesClient<>(storeClient, Instant.class);
        this.rdbStoreClient = rdbStoreClient;
        ontopService = new ServiceEndpoint(ontopEndpoint);
    }

    String getCitiesNamespace(String citiesNamespaceIri) {
        SelectQuery query = Queries.SELECT().prefix(P_DISP);
        Variable citiesKgNamespace = query.var();
        GraphPattern gp = iri(citiesNamespaceIri).has(HAS_NAME, citiesKgNamespace);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return queryResult.getJSONObject(0).getString(citiesKgNamespace.getQueryString().substring(1));
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
        GraphPattern gp = iri(instance).has(PropertyPaths.path(iri(TIME.IN_TIME_POSITION), iri(TIME.NUMERIC_POSITION)),
                value);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return Long.parseLong(queryResult.getJSONObject(0).getString(value.getQueryString().substring(1)));
    }

    Map<String, Integer> getZMap(List<String> zIriList) {
        SelectQuery query = Queries.SELECT().prefix(P_OM);
        Variable valueVar = query.var();
        Variable zVar = query.var();
        ValuesPattern<Iri> valuesPattern = new ValuesPattern<>(zVar,
                zIriList.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        query.where(zVar.has(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), valueVar), valuesPattern);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        Map<String, Integer> zMap = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String zIri = queryResult.getJSONObject(i).getString(zVar.getQueryString().substring(1));
            int value = queryResult.getJSONObject(i).getInt(valueVar.getQueryString().substring(1));
            zMap.put(zIri, value);
        }

        return zMap;
    }

    private Map<String, String> getBuildingToPsMap() {
        SelectQuery query = Queries.SELECT().prefix(P_DISP);
        Variable sps = query.var();
        Variable ocgmlIRI = query.var();
        GraphPattern gp = GraphPatterns.and(sps.isA(STATIC_POINT_SOURCE).andHas(HAS_OCGML_OBJECT, ocgmlIRI));
        query.select(ocgmlIRI, sps).where(gp).distinct();
        JSONArray pointSourceIRI = storeClient
                .executeQuery(query.getQueryString());

        Map<String, String> buildingToPsMap = new HashMap<>();
        for (int i = 0; i < pointSourceIRI.length(); i++) {
            String ontoCityGMLIRI = pointSourceIRI.getJSONObject(i).getString(ocgmlIRI.getQueryString().substring(1));
            String psIri = pointSourceIRI.getJSONObject(i).getString(sps.getQueryString().substring(1));
            buildingToPsMap.put(ontoCityGMLIRI, psIri);
        }

        return buildingToPsMap;
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

    private List<String> getIRIofStaticPointSourcesWithinScope(Polygon scope, List<String> pointSourceIRIAll)
            throws org.apache.jena.sparql.lang.sparql_11.ParseException {

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
                        "The following OCGML IRI of a static point source does not reference a cityfurniture or building object: {}",
                        psIRI);
            }

        }

        List<Coordinate> coordinateList = Arrays.asList(scope.getCoordinates());
        List<Double> xCoordinates = coordinateList.stream().map(c -> c.getX()).collect(Collectors.toList());
        List<Double> yCoordinates = coordinateList.stream().map(c -> c.getY()).collect(Collectors.toList());

        double xMin = Collections.min(xCoordinates);
        double yMin = Collections.min(yCoordinates);
        double xMax = Collections.max(xCoordinates);
        double yMax = Collections.max(yCoordinates);

        // Convert coordinates to the CRS used in the namespace

        String scopeSrid = "EPSG:" + scope.getSRID();
        double[] xyMin = { xMin, yMin };
        double[] xyMax = { xMax, yMax };
        double[] xyMinCRS = CRSTransformer.transform(scopeSrid, namespaceCRS, xyMin);
        double[] xyMaxCRS = CRSTransformer.transform(scopeSrid, namespaceCRS, xyMax);

        // The variable zMax is set to an arbitrarily large value.
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
        body.addElement(wb5.build().getQueryPattern());
        body.addElement(new ElementService(geoUri + "search", wb.build().getQueryPattern()));
        query.setQueryPattern(body);

        String queryString = query.toString().replace("PLACEHOLDER", "");
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

        List<String> pointSourceIRIAll = new ArrayList<>(getBuildingToPsMap().keySet());
        List<String> pointSourceOCGMLIRIWithinScope = getIRIofStaticPointSourcesWithinScope(scope, pointSourceIRIAll);

        SelectQuery query = Queries.SELECT().prefix(P_DISP, P_OM);
        Variable ocgmlIRI = query.var();
        Variable sps = query.var();

        GraphPattern gp = sps.isA(STATIC_POINT_SOURCE).andHas(HAS_OCGML_OBJECT, ocgmlIRI);

        ValuesPattern<Iri> vp = new ValuesPattern<>(ocgmlIRI,
                pointSourceOCGMLIRIWithinScope.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        query.select(sps, ocgmlIRI).where(gp, vp).distinct();
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        List<StaticPointSource> pointSourceWithinScope = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String spsIRI = queryResult.getJSONObject(i).getString(sps.getQueryString().substring(1));
            String ontoCityGMLIRI = queryResult.getJSONObject(i).getString(ocgmlIRI.getQueryString().substring(1));

            StaticPointSource pointSource;

            if (ontoCityGMLIRI.contains("cityfurniture"))
                pointSource = new StaticPointSource(spsIRI, StaticPointSource.CityObjectType.CITY_FURNITURE);
            else if (ontoCityGMLIRI.contains("building"))
                pointSource = new StaticPointSource(spsIRI, StaticPointSource.CityObjectType.BUILDING);
            else {
                LOGGER.error("Static point source with IRI {} has an unknown OntoCityGML object type. ", spsIRI);
                throw new JPSRuntimeException("Static point source has unknown OntoCityGML object type.");
            }

            pointSource.setOcgmlIri(ontoCityGMLIRI);
            pointSourceWithinScope.add(pointSource);

        }

        return pointSourceWithinScope;

    }

    List<StaticPointSource> getStaticPointSourcesWithinScope(Map<String, Building> iriToBuildingMap) {
        Map<String, String> buildingToPsMap = getBuildingToPsMap();
        List<StaticPointSource> pointSourceList = new ArrayList<>();

        buildingToPsMap.keySet().stream().filter(iriToBuildingMap::containsKey).forEach(building -> {
            StaticPointSource pointSource = new StaticPointSource(buildingToPsMap.get(building));
            pointSource.setLocation(iriToBuildingMap.get(building).getLocation());
            pointSource.setHeight(iriToBuildingMap.get(building).getHeight());
            pointSource.setDiameter(2.0 * iriToBuildingMap.get(building).getRadius());

            pointSourceList.add(pointSource);
        });

        return pointSourceList;
    }

    List<Ship> getShipsWithinTimeAndScopeViaTsClient(long simulationTime, Geometry scope, long timeBuffer) {
        long simTimeUpperBound = simulationTime + timeBuffer; // +30 minutes
        long simTimeLowerBound = simulationTime - timeBuffer; // -30 minutes

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
                    point.setSRID(4326);

                    if (scope.covers(point)) {
                        // measureToShipMap.get(measure) gives the iri
                        Ship ship = new Ship(measureToShipMap.get(measure));
                        ship.setLocation(point);
                        ship.setLocationMeasureIri(measure);
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

        GraphPattern queryPattern = iri(scopeIri).has(PropertyPaths.path(HAS_GEOMETRY, AS_WKT), scope);
        query.prefix(P_GEO).where(ontopService.service(queryPattern));

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
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
        // create a look up map to get point source object based on IRI
        Map<String, PointSource> iriToSourceMap = new HashMap<>();
        allSources.stream().forEach(s -> iriToSourceMap.put(s.getIri(), s));
        List<String> pointSourceIRI = allSources.stream().map(s -> s.getIri()).collect(Collectors.toList());

        SelectQuery query = Queries.SELECT().prefix(P_DISP, P_OM);
        Variable ps = query.var();
        Variable emissionIRI = query.var();
        Variable pollutant = query.var();
        Variable pollutantIRI = query.var();
        Variable massFlowIRI = query.var();
        Variable densityIRI = query.var();
        Variable temperatureIRI = query.var();
        Variable emissionValue = query.var();
        Variable densityValue = query.var();
        Variable temperatureValue = query.var();
        Variable emissionUnit = query.var();
        Variable densityUnit = query.var();
        Variable temperatureUnit = query.var();

        GraphPattern gp = GraphPatterns.and(ps.has(EMITS, emissionIRI),
                emissionIRI.has(HAS_POLLUTANT_ID, pollutantIRI).andHas(HAS_QTY, massFlowIRI)
                        .andHas(HAS_QTY, densityIRI),
                pollutantIRI.isA(pollutant),
                massFlowIRI.isA(iri(MASS_FLOW)).andHas(PropertyPaths.path(HAS_VALUE,
                        HAS_NUMERICALVALUE), emissionValue)
                        .andHas(PropertyPaths.path(HAS_VALUE, HAS_UNIT), emissionUnit),
                densityIRI.isA(iri(DENSITY)).andHas(PropertyPaths.path(HAS_VALUE,
                        HAS_NUMERICALVALUE), densityValue)
                        .andHas(PropertyPaths.path(HAS_VALUE, HAS_UNIT), densityUnit),
                GraphPatterns.and(emissionIRI.has(HAS_QTY, temperatureIRI), temperatureIRI.isA(iri(TEMPERATURE))
                        .andHas(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), temperatureValue)
                        .andHas(PropertyPaths.path(HAS_VALUE, HAS_UNIT), temperatureUnit)).optional());
        // particle phase does not have temperature

        ValuesPattern<Iri> vp = new ValuesPattern<>(ps,
                pointSourceIRI.stream().map(Rdf::iri).collect(Collectors.toList()),
                Iri.class);
        query.select(ps, pollutant, emissionValue, emissionUnit, densityValue,
                densityUnit, temperatureValue,
                temperatureUnit).where(gp, vp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        String[] gasPhaseEmissions = { CO2, NO_X, SO2, CO, UHC };

        for (int i = 0; i < queryResult.length(); i++) {
            String psIRI = queryResult.getJSONObject(i).getString(ps.getQueryString().substring(1));
            String pollutantID = queryResult.getJSONObject(i).getString(pollutant.getQueryString().substring(1));
            // warning: Double.parseDouble does not parse values like 3E-7 correctly
            double emission = Double
                    .valueOf(queryResult.getJSONObject(i).getString(emissionValue.getQueryString().substring(1)));
            double density = queryResult.getJSONObject(i).getDouble(densityValue.getQueryString().substring(1));
            String emissionUnitString = queryResult.getJSONObject(i)
                    .getString(emissionUnit.getQueryString().substring(1));
            String densityUnitString = queryResult.getJSONObject(i)
                    .getString(densityUnit.getQueryString().substring(1));

            PointSource pointSource = iriToSourceMap.get(psIRI);

            boolean isGasPhase = StringUtils.equalsAny(pollutantID, gasPhaseEmissions);

            String temperatureUnitString = null;
            if (isGasPhase) {
                temperatureUnitString = queryResult.getJSONObject(i)
                        .getString(temperatureUnit.getQueryString().substring(1));
            }

            if (checkUnits(temperatureUnitString, densityUnitString, emissionUnitString)) {
                if (isGasPhase) {
                    pointSource.setMixtureDensityInKgm3(density);
                    double value = queryResult.getJSONObject(i)
                            .getDouble(temperatureValue.getQueryString().substring(1));
                    pointSource.setMixtureTemperatureInKelvin(value);
                } else {
                    pointSource.setParticleDensity(density);
                }

                pointSource.setFlowrateInKgPerS(Pollutant.getPollutantType(pollutantID), emission);
            }
        }
    }

    private boolean checkUnits(String temperatureUnitString, String densityUnitString, String massFlow) {
        boolean correctUnits = true;
        if (temperatureUnitString != null && !temperatureUnitString.equals(UNIT_KELVIN)) {
            LOGGER.warn("Unexpected temperature units");
            correctUnits = false;
        } else if (!densityUnitString.equals(UNIT_KG_M3)) {
            LOGGER.warn("Unexpected density units");
            correctUnits = false;
        } else if (!massFlow.equals(UNIT_KG_S)) {
            LOGGER.warn("Unexpected mass flowrate units");
            correctUnits = false;
        }
        return correctUnits;
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
        query.select(polygonData, objectIRI).where(gp, vp);
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

    // Object Class ID is 35 for ground surfaces and 33 for roof surfaces.
    // More details can be found in the OntoCityGML documentation at
    // https://3dcitydb-docs.readthedocs.io/en/latest/3dcitydb/schema/building.html

    public Map<String, List<List<Polygon>>> buildingsQuery(List<String> ocgmlIRI) {

        SelectQuery query = Queries.SELECT().prefix(P_OCGML);
        Variable geometricIRI = SparqlBuilder.var("geometricIRI");
        Variable polygonData = SparqlBuilder.var("polygonData");
        Variable objectIRI = SparqlBuilder.var("objectIRI");
        Variable surfaceIRI = SparqlBuilder.var("surfaceIRI");
        Variable datatype = SparqlBuilder.var("datatype");
        Variable objectClassId = SparqlBuilder.var("objectClassId");

        List<Integer> objectClassIdValues = new ArrayList<>(Arrays.asList(33, 35));

        Expression<?> dataType = Expressions.function(SparqlFunction.DATATYPE, polygonData);

        GraphPattern gp = GraphPatterns
                .and(surfaceIRI.has(OCGML_GEOM, polygonData).andHas(OCGML_CITYOBJECT, geometricIRI),
                        geometricIRI.has(OCGML_OBJECTCLASSID, objectClassId).andHas(OCGML_BUILDINGID, objectIRI))
                .filter(Expressions.not(Expressions.function(SparqlFunction.IS_BLANK, polygonData)));
        ValuesPattern<Iri> vp = new ValuesPattern<>(objectIRI,
                ocgmlIRI.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        ValuesPattern<Integer> vp2 = new ValuesPattern<>(objectClassId, objectClassIdValues, Integer.class);

        query.select(polygonData, objectIRI, dataType.as(datatype), objectClassId).where(gp, vp, vp2);
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

    Map<String, List<List<Polygon>>> estimatePolygonsFromEnvelope(Map<String, String> buildingToEnvelopeMap) {
        Map<String, List<List<Polygon>>> iriToPolygonMap = new HashMap<>();
        buildingToEnvelopeMap.entrySet().forEach(mapEntry -> {
            String[] envelopeString = mapEntry.getValue().split("#");

            List<Double> xList = new ArrayList<>();
            List<Double> yList = new ArrayList<>();
            List<Double> zList = new ArrayList<>();
            for (int i = 0; i < envelopeString.length; i += 3) {
                xList.add(Double.valueOf(envelopeString[i]));
                yList.add(Double.valueOf(envelopeString[i + 1]));
                zList.add(Double.valueOf(envelopeString[i + 2]));
            }

            double xMin = Collections.min(xList);
            double yMin = Collections.min(yList);
            double xMax = Collections.max(xList);
            double yMax = Collections.max(yList);
            double zMin = Collections.min(zList);
            double zMax = Collections.max(zList);

            // create polygon
            Coordinate[] coordinatesGround = { new Coordinate(xMin, yMin, zMin), new Coordinate(xMax, yMin, zMin),
                    new Coordinate(xMax, yMax, zMin), new Coordinate(xMin, yMax, zMin),
                    new Coordinate(xMin, yMin, zMin) };
            Coordinate[] coordinatesRoof = { new Coordinate(xMin, yMin, zMax), new Coordinate(xMax, yMin, zMax),
                    new Coordinate(xMax, yMax, zMax), new Coordinate(xMin, yMax, zMax),
                    new Coordinate(xMin, yMin, zMax) };

            Polygon polygonGround = new GeometryFactory().createPolygon(coordinatesGround);
            Polygon polygonRoof = new GeometryFactory().createPolygon(coordinatesRoof);
            List<List<Polygon>> polyList = new ArrayList<>();
            polyList.add(List.of(polygonGround));
            polyList.add(List.of(polygonRoof));

            iriToPolygonMap.put(mapEntry.getKey(), polyList);
        });
        return iriToPolygonMap;
    }

    /**
     * queries for buildings near each static point
     * 
     * @param allSources
     * @param allBuildings
     * @return
     */
    List<Building> getBuildings(List<PointSource> allSources, Map<String, Building> allBuildings) {
        List<Building> buildings = new ArrayList<>();

        String sridIri = "<http://www.opengis.net/def/crs/OGC/1.3/CRS84>";

        allSources.stream().forEach(source -> {
            Polygon boundingBox = getBoundingBoxOfPointSources2(source);

            SelectQuery query = Queries.SELECT();
            Variable buildingVar = query.var();
            Variable wktVar = query.var();
            Variable surfaceParentVar = query.var();
            Variable surfaceVar = query.var();

            GraphPattern queryPattern = GraphPatterns.and(
                    buildingVar.has(LOD0_FOOTPRINT, surfaceParentVar),
                    surfaceVar.has(PARENT, surfaceParentVar).andHas(AS_WKT, wktVar));

            Expression<?> expression = Expressions
                    .and(GeoSPARQL.sfWithin(wktVar, Rdf.literalOfType(sridIri + " " + boundingBox.toString(),
                            iri(GEO.GEO_WKT_LITERAL.getIRIString()))));

            query.select(buildingVar).where(ontopService.service(queryPattern.filter(expression)))
                    .prefix(P_BLDG, P_GEO, P_GRP, P_GEOF).distinct();

            JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

            for (int i = 0; i < queryResult.length(); i++) {
                String buildingIri = queryResult.getJSONObject(i).getString(buildingVar.getQueryString().substring(1));

                // if this throws an error then there's something wrong, bounding box is a
                // subset of scope
                if (allBuildings.get(buildingIri) != null && !buildings.contains(allBuildings.get(buildingIri))) {
                    buildings.add(allBuildings.get(buildingIri));
                }
            }
        });

        return buildings;
    }

    Map<String, Building> getBuildingsWithinScope(String scopeIri) {
        SelectQuery query = Queries.SELECT();

        Variable buildingVar = query.var();
        Variable footPrintVar = query.var();
        Variable heightVar = query.var();
        Variable geometryVar = query.var();
        Variable footPrintWktVar = query.var();
        Variable scopeWktVar = query.var();

        GraphPattern queryPattern = GraphPatterns.and(
                buildingVar.has(LOD0_FOOTPRINT, footPrintVar).andHas(MEASURED_HEIGHT, heightVar),
                geometryVar.has(PARENT, footPrintVar).andHas(AS_WKT, footPrintWktVar),
                iri(scopeIri).has(PropertyPaths.path(HAS_GEOMETRY, AS_WKT), scopeWktVar))
                .filter(Expressions.and(GeoSPARQL.sfWithin(footPrintWktVar, scopeWktVar)));

        query.where(ontopService.service(queryPattern)).prefix(P_GRP, P_GEO, P_GEOF, P_BLDG).select(footPrintWktVar,
                buildingVar, heightVar);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        Map<String, List<Polygon>> buildingToPolygonsMap = new HashMap<>();
        Map<String, Double> buildingToHeightMap = new HashMap<>();

        for (int i = 0; i < queryResult.length(); i++) {
            String buildingIri = queryResult.getJSONObject(i).getString(buildingVar.getQueryString().substring(1));
            String wktLiteral = queryResult.getJSONObject(i).getString(footPrintWktVar.getQueryString().substring(1));
            double height = queryResult.getJSONObject(i).getDouble(heightVar.getQueryString().substring(1));

            Polygon footPrintPolygon = (Polygon) WKTReader.extract(wktLiteral).getGeometry();

            buildingToPolygonsMap.computeIfAbsent(buildingIri, k -> new ArrayList<>());
            buildingToPolygonsMap.get(buildingIri).add(footPrintPolygon);

            buildingToHeightMap.computeIfAbsent(buildingIri, k -> height);
        }

        Map<String, Building> iriToBuildingMap = new HashMap<>();

        buildingToPolygonsMap.entrySet().forEach(entrySet -> {
            String buildingIri = entrySet.getKey();
            List<Polygon> footprintPolygons = entrySet.getValue();
            double height = buildingToHeightMap.get(buildingIri);

            Building building = new Building(footprintPolygons, height);
            building.setIri(buildingIri);
            building.getFootprint().setSRID(4326);
            building.getLocation().setSRID(4326);

            iriToBuildingMap.put(buildingIri, building);
        });

        return iriToBuildingMap;
    }

    // This method only retrieves items with an object Class ID of 26, which is the
    // OCGML identifier for buildings.
    // See
    // https://3dcitydb-docs.readthedocs.io/en/latest/3dcitydb/schema/core.html
    // for more details.

    public Map<String, List<List<Polygon>>> getBuildingsNearPollutantSources(List<PointSource> allSources)
            throws org.apache.jena.sparql.lang.sparql_11.ParseException {

        List<String> cityObjectIRIList = allSources.stream().filter(i -> i.getClass() == StaticPointSource.class)
                .map(i -> ((StaticPointSource) i).getOcgmlIri().replace("building", "cityobject")
                        .replace("cityfurniture", "cityobject"))
                .collect(Collectors.toList());

        Polygon boundingBox = getBoundingBoxOfPointSources(allSources);
        List<String> newBoxBounds = getCornersForCitiesQuery(boundingBox);

        String lowerBounds = newBoxBounds.get(0);
        String upperBounds = newBoxBounds.get(1);

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
                .addWhere("?cityObject", "ocgml:EnvelopeType", "?envelope")
                .addFilter("?id=26");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?cityObject").addVar("?envelope");

        Query query = sb.build();
        // add geospatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(geoUri + "search", wb.build().getQueryPattern()));
        body.addElement(wb2.build().getQueryPattern());
        query.setQueryPattern(body);

        String queryString = query.toString().replace("PLACEHOLDER", "");
        JSONArray buildingIRIQueryResult = AccessAgentCaller.queryStore(citiesNamespace, queryString);

        List<String> buildingOCGMLIRIList = new ArrayList<>();

        Map<String, String> buildingToEnvelopeMap = new HashMap<>();
        for (int i = 0; i < buildingIRIQueryResult.length(); i++) {
            String envelopeString = buildingIRIQueryResult.getJSONObject(i).getString("envelope");
            String buildingIri = buildingIRIQueryResult.getJSONObject(i).getString("cityObject").replace("cityobject",
                    "building");
            buildingToEnvelopeMap.put(buildingIri, envelopeString);
            buildingOCGMLIRIList.add(buildingIri);
        }

        if (citiesNamespace.contentEquals("singaporeEPSG24500")) {
            // temporary hack, bad code!
            return estimatePolygonsFromEnvelope(buildingToEnvelopeMap);
        } else {
            return buildingsQuery(buildingOCGMLIRIList);
        }
    }

    /**
     * creates a rectangle from min/max of coordinates from allSources
     * then adds a buffer
     */
    private Polygon getBoundingBoxOfPointSources(List<PointSource> allSources) {
        double buffer = 500;
        GeometryFactory geoFactory = new GeometryFactory();
        List<Point> convertedPoints = allSources.stream().map(s -> {
            double[] xyOriginal = { s.getLocation().getX(), s.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform("EPSG:" + s.getLocation().getSRID(), namespaceCRS,
                    xyOriginal);
            return geoFactory.createPoint(new Coordinate(xyTransformed[0], xyTransformed[1]));
        }).collect(Collectors.toList());

        List<Double> xCoordinates = convertedPoints.stream().map(Point::getX).collect(Collectors.toList());
        List<Double> yCoordinates = convertedPoints.stream().map(Point::getY).collect(Collectors.toList());

        double xMin = Collections.min(xCoordinates);
        double yMin = Collections.min(yCoordinates);
        double xMax = Collections.max(xCoordinates);
        double yMax = Collections.max(yCoordinates);

        if (allSources.size() == 1) {
            double expandRange = 10.0;
            xMin -= expandRange;
            xMax += expandRange;
            yMin -= expandRange;
            yMax += expandRange;
        }

        Coordinate[] coordinates = { new Coordinate(xMin, yMin), new Coordinate(xMax, yMin),
                new Coordinate(xMax, yMax), new Coordinate(xMin, yMax), new Coordinate(xMin, yMin) };

        GeometryFactory geometryFactory = new GeometryFactory();
        Polygon boundingBox = geometryFactory.createPolygon(coordinates);

        return (Polygon) boundingBox.buffer(buffer);
    }

    /**
     * at the time of writing this is assumes data is in lat lon (epsg:4326)
     * 
     * @param allSources
     * @return
     */
    private Polygon getBoundingBoxOfPointSources2(PointSource source) {
        double buffer = 0.002;
        double[] xyOriginal = { source.getLocation().getX(), source.getLocation().getY() };
        double[] xyTransformed = CRSTransformer.transform("EPSG:" + source.getLocation().getSRID(), "EPSG:4326",
                xyOriginal);

        double expandRange = 0.001;
        double xMin = xyTransformed[0] - expandRange;
        double yMin = xyTransformed[1] + expandRange;
        double xMax = xyTransformed[0] - expandRange;
        double yMax = xyTransformed[0] + expandRange;

        Coordinate[] coordinates = { new Coordinate(xMin, yMin), new Coordinate(xMax, yMin),
                new Coordinate(xMax, yMax), new Coordinate(xMin, yMax), new Coordinate(xMin, yMin) };

        GeometryFactory geometryFactory = new GeometryFactory();
        Polygon boundingBox = geometryFactory.createPolygon(coordinates);
        boundingBox.setSRID(4326);

        return (Polygon) boundingBox.buffer(buffer);
    }

    private List<String> getCornersForCitiesQuery(Polygon boundingBox) {
        double zMax = 800.0;

        List<Coordinate> coordinatesList = List.of(boundingBox.getCoordinates());
        List<Double> xCoordinates = coordinatesList.stream().map(c -> c.getX()).collect(Collectors.toList());
        List<Double> yCoordinates = coordinatesList.stream().map(c -> c.getY()).collect(Collectors.toList());
        double xMin = Collections.min(xCoordinates);
        double yMin = Collections.min(yCoordinates);
        double xMax = Collections.max(xCoordinates);
        double yMax = Collections.max(yCoordinates);

        List<String> lowerCornerSequence = new ArrayList<>();
        List<String> upperCornerSequence = new ArrayList<>();

        for (int i = 0; i < 5; i++) {
            lowerCornerSequence.add(String.valueOf(xMin));
            lowerCornerSequence.add(String.valueOf(yMin));
            lowerCornerSequence.add(String.valueOf(0));

            upperCornerSequence.add(String.valueOf(xMax));
            upperCornerSequence.add(String.valueOf(yMax));
            upperCornerSequence.add(String.valueOf(zMax));
        }

        List<String> corners = new ArrayList<>();
        corners.add(String.join("#", lowerCornerSequence));
        corners.add(String.join("#", upperCornerSequence));

        return corners;
    }

    DSLContext getContext(Connection conn) {
        return DSL.using(conn, SQLDialect.POSTGRES);
    }

    boolean tableExists(String tableName) {
        try (Connection conn = rdbStoreClient.getConnection()) {
            return tableExists(tableName, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            return false;
        }
    }

    boolean tableExists(String tableName, Connection conn) {
        String condition = String.format("table_name = '%s'", tableName);
        return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0,
                int.class) == 1;
    }

    public void setElevation(List<PointSource> pointSources, List<Building> buildings, int simulationSrid) {

        String elevationTable = EnvConfig.ELEVATION_TABLE;

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {
            for (int i = 0; i < pointSources.size(); i++) {
                PointSource ps = pointSources.get(i);
                String originalSrid = "EPSG:" + ps.getLocation().getSRID();
                double[] xyOriginal = { ps.getLocation().getX(), ps.getLocation().getY() };
                double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);

                String sqlString = String.format(
                        "SELECT ST_Value(rast, ST_Transform(ST_SetSRID(ST_MakePoint(%f,%f),%d),ST_SRID(rast))) AS val "
                                +
                                "FROM %s WHERE ST_Intersects(rast, ST_Transform(ST_SetSRID(ST_MakePoint(%f,%f),%d),ST_SRID(rast)));",
                        xyTransformed[0], xyTransformed[1], simulationSrid, elevationTable,
                        xyTransformed[0], xyTransformed[1], simulationSrid);

                ResultSet result = stmt.executeQuery(sqlString);
                if (result.next()) {
                    double elevation = result.getDouble("val");
                    ps.setElevation(elevation);
                } else {
                    LOGGER.warn(
                            "Could not find elevation data for the point source located at ({},{}) in EPSG:{} coordinates. Its elevation will be set to zero when running BPIPPRM and AERMOD.",
                            xyTransformed[0], xyTransformed[1], simulationSrid);
                }

            }

            for (int i = 0; i < buildings.size(); i++) {
                Building building = buildings.get(i);
                String originalSrid = building.getSrid();
                double[] xyOriginal = { building.getLocation().getX(), building.getLocation().getY() };
                double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);

                String sqlString = String.format(
                        "SELECT ST_Value(rast, ST_Transform(ST_SetSRID(ST_MakePoint(%f,%f),%d),ST_SRID(rast))) AS val "
                                +
                                "FROM %s WHERE ST_Intersects(rast, ST_Transform(ST_SetSRID(ST_MakePoint(%f,%f),%d),ST_SRID(rast)));",
                        xyTransformed[0], xyTransformed[1], simulationSrid, elevationTable,
                        xyTransformed[0], xyTransformed[1], simulationSrid);

                ResultSet result = stmt.executeQuery(sqlString);
                if (result.next()) {
                    double elevation = result.getDouble("val");
                    building.setElevation(elevation);
                } else {
                    LOGGER.warn(
                            "Could not find elevation data for the building located at ({},{}) in EPSG:{} coordinates. Its elevation will be set to zero when running BPIPPRM and AERMOD.",
                            xyTransformed[0], xyTransformed[1], simulationSrid);
                }

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }
    }

    boolean hasElevationData(Polygon scope) {
        boolean hasElevationData = false;
        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement()) {
            String sql = String.format("SELECT EXISTS (SELECT * FROM %s"
                    + " WHERE ST_Intersects(rast, ST_Transform(ST_GeomFromText('%s',4326),ST_SRID(rast))))",
                    EnvConfig.ELEVATION_TABLE, scope.toText());
            ResultSet result = stmt.executeQuery(sql);
            if (result.next()) {
                hasElevationData = result.getBoolean("exists");
            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return hasElevationData;
    }

    void updateOutputs(String derivation, Map<String, DispersionOutput> zIriToOutputMap, boolean hasShips,
            long timeStamp, boolean hasStaticPoints, boolean hasBuildings, boolean usesElevation) {
        SelectQuery query = Queries.SELECT();

        Variable entity = query.var();
        Variable pollutant = query.var();
        Variable pollutantIri = query.var();
        Variable dispMatrix = query.var();
        Variable dispRaster = query.var();
        Variable dispColourBar = query.var();
        Variable zVar = query.var();

        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        query.where(
                entity.has(belongsTo, iri(derivation)).andHas(HAS_POLLUTANT_ID, pollutantIri).andHas(HAS_HEIGHT, zVar)
                        .andHas(HAS_DISPERSION_MATRIX, dispMatrix).andHas(HAS_DISPERSION_RASTER, dispRaster)
                        .andHas(HAS_DISPERSION_COLOUR_BAR, dispColourBar),
                pollutantIri.isA(pollutant)).prefix(P_DISP)
                .select(zVar, pollutant, dispMatrix, dispRaster, dispColourBar);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        List<String> tsDataList = new ArrayList<>();
        List<List<?>> tsValuesList = new ArrayList<>();

        for (int i = 0; i < queryResult.length(); i++) {
            String zIri = queryResult.getJSONObject(i).getString(zVar.getQueryString().substring(1));
            String pollutantIRI = queryResult.getJSONObject(i).getString(pollutant.getQueryString().substring(1));
            String dispersionMatrixIRI = queryResult.getJSONObject(i)
                    .getString(dispMatrix.getQueryString().substring(1));
            String dispersionRasterIRI = queryResult.getJSONObject(i)
                    .getString(dispRaster.getQueryString().substring(1));
            String dispersionColourBarIRI = queryResult.getJSONObject(i)
                    .getString(dispColourBar.getQueryString().substring(1));

            PollutantType pollutantType = Pollutant.getPollutantType(pollutantIRI);
            DispersionOutput dispersionOutput = zIriToOutputMap.get(zIri);
            if (dispersionOutput.hasPollutant(pollutantType)) {
                tsDataList.add(dispersionMatrixIRI);
                tsDataList.add(dispersionRasterIRI);
                tsDataList.add(dispersionColourBarIRI);
                String dispersionMatrix = dispersionOutput.getDispMatrix(pollutantType);
                String dispersionRaster = dispersionOutput.getDispRaster(pollutantType);
                String dispersionColourBar = dispersionOutput.getColourBar(pollutantType);
                tsValuesList.add(List.of(dispersionMatrix));
                tsValuesList.add(List.of(dispersionRaster));
                tsValuesList.add(List.of(dispersionColourBar));
            }
        }

        SelectQuery query2 = Queries.SELECT();
        Variable entityType = SparqlBuilder.var("entityType");
        Variable layerVar = SparqlBuilder.var("layerVar");
        ValuesPattern<Iri> valuesPattern = new ValuesPattern<>(entityType,
                List.of(iri(SHIPS_LAYER), iri(STATIC_POINT_SOURCE_LAYER), iri(BUILDINGS_LAYER), iri(ELEVATION_LAYER)),
                Iri.class);

        query2.where(layerVar.isA(entityType).andHas(belongsTo, iri(derivation)), valuesPattern).prefix(P_DISP);

        queryResult = storeClient.executeQuery(query2.getQueryString());

        for (int i = 0; i < queryResult.length(); i++) {
            String entityIri = queryResult.getJSONObject(i).getString(layerVar.getQueryString().substring(1));
            String entityTypeString = queryResult.getJSONObject(i)
                    .getString(entityType.getQueryString().substring(1));
            if (entityTypeString.contentEquals(SHIPS_LAYER)) {
                tsDataList.add(entityIri);
                tsValuesList.add(List.of(hasShips));
            } else if (entityTypeString.contentEquals(STATIC_POINT_SOURCE_LAYER)) {
                tsDataList.add(entityIri);
                tsValuesList.add(List.of(hasStaticPoints));
            } else if (entityTypeString.contentEquals(BUILDINGS_LAYER)) {
                tsDataList.add(entityIri);
                tsValuesList.add(List.of(hasBuildings));
            } else if (entityTypeString.contentEquals(ELEVATION_LAYER)) {
                tsDataList.add(entityIri);
                tsValuesList.add(List.of(usesElevation));
            }
        }

        TimeSeries<Long> timeSeries = new TimeSeries<>(List.of(timeStamp), tsDataList, tsValuesList);

        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClientLong.addTimeSeriesData(timeSeries, conn);
        } catch (SQLException e) {
            LOGGER.error("Failed at closing connection");
            LOGGER.error(e.getMessage());
        }

    }

    void dropRasterConstraints() {
        try (Connection conn = rdbStoreClient.getConnection(); Statement statement = conn.createStatement();) {
            String sql = String.format("SELECT DropRasterConstraints('%s', 'rast')", EnvConfig.DISPERSION_RASTER_TABLE);
            statement.executeQuery(sql);
        } catch (SQLException e) {
            LOGGER.error("Failed at closing connection");
            LOGGER.error(e.getMessage());
        }
    }

    /**
     * will be replaced by new postgis cities kg
     */
    void createBuildingsLayer(List<Building> buildings, String derivationIri, long time) {
        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");

        JSONArray features = new JSONArray();

        buildings.stream().forEach(building -> {
            JSONObject feature = new JSONObject();
            feature.put("type", "Feature");

            JSONObject properties = new JSONObject();
            properties.put("color", "#666666");
            properties.put("opacity", 0.66);
            properties.put("base", 0);
            properties.put("height", building.getHeight());
            properties.put("iri", building.getIri());
            properties.put("derivation", derivationIri);
            properties.put("time", time);
            feature.put("properties", properties);

            JSONObject geometry = new JSONObject();
            geometry.put("type", "Polygon");
            JSONArray coordinates = new JSONArray();

            JSONArray footprintPolygon = new JSONArray();
            String srid = building.getSrid();
            for (Coordinate coordinate : building.getFootprint().getCoordinates()) {
                JSONArray point = new JSONArray();
                double[] xyOriginal = { coordinate.getX(), coordinate.getY() };
                double[] xyTransformed = CRSTransformer.transform(srid, "EPSG:4326", xyOriginal);
                point.put(xyTransformed[0]).put(xyTransformed[1]);
                footprintPolygon.put(point);
            }
            coordinates.put(footprintPolygon);
            geometry.put("coordinates", coordinates);

            feature.put("geometry", geometry);
            features.put(feature);
        });

        featureCollection.put("features", features);

        GDALClient gdalClient = GDALClient.getInstance();
        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, EnvConfig.BUILDINGS_TABLE,
                featureCollection.toString(), new Ogr2OgrOptions(), true);

        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        geoServerClient.createWorkspace(EnvConfig.GEOSERVER_WORKSPACE);

        geoServerClient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE,
                EnvConfig.BUILDINGS_TABLE, new GeoServerVectorSettings());
    }

    List<List<Double>> getReceptorElevation(Polygon scope, int nx, int ny, int simulationSrid) {
        List<Double> xDoubles = new ArrayList<>();
        List<Double> yDoubles = new ArrayList<>();

        for (int i = 0; i < scope.getCoordinates().length; i++) {

            String originalSrid = "EPSG:4326";
            double[] xyOriginal = { scope.getCoordinates()[i].x, scope.getCoordinates()[i].y };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);

            xDoubles.add(xyTransformed[0]);
            yDoubles.add(xyTransformed[1]);
        }

        double xlo = Collections.min(xDoubles);
        double xhi = Collections.max(xDoubles);
        double ylo = Collections.min(yDoubles);
        double yhi = Collections.max(yDoubles);

        double dx = (xhi - xlo) / nx;
        double dy = (yhi - ylo) / ny;

        List<List<Double>> allValues = new ArrayList<>();
        LOGGER.info("Querying elevation data");
        try (Connection conn = rdbStoreClient.getConnection(); Statement stmt = conn.createStatement();) {
            int numRecWithoutElev = 0;

            for (int row = 0; row < ny; row++) { // y loop
                double y = ylo + row * dy;
                List<Double> elevationValues = new ArrayList<>();
                for (int column = 0; column < nx; column++) { // x loop
                    double x = xlo + column * dx;
                    String sqlString = String.format(
                            "SELECT ST_Value(rast, ST_Transform(ST_SetSRID(ST_Point(%f,%f),%d),ST_SRID(rast))) AS val "
                                    +
                                    "FROM %s WHERE ST_Intersects(rast, ST_Transform(ST_SetSRID(ST_Point(%f,%f),%d),ST_SRID(rast)));",
                            x, y, simulationSrid, EnvConfig.ELEVATION_TABLE,
                            x, y, simulationSrid);

                    ResultSet result = stmt.executeQuery(sqlString);
                    double val = 0.0;
                    while (result.next() && val == 0.0) {
                        val = result.getDouble("val");
                    }

                    if (val == 0.0) {
                        numRecWithoutElev++;
                    }

                    elevationValues.add(val);
                }
                allValues.add(elevationValues);
            }

            if (numRecWithoutElev > 0) {
                LOGGER.warn("Number of receptors without elevation values in the database: {}", numRecWithoutElev);
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }
        return allValues;
    }

    boolean hasElevationContourData(String derivationIri) {
        String sql = String.format("select count(*) from %s where derivation='%s'", EnvConfig.ELEVATION_CONTOURS_TABLE,
                derivationIri);
        try (Connection conn = rdbStoreClient.getConnection(); Statement stmt = conn.createStatement()) {
            ResultSet result = stmt.executeQuery(sql);
            while (result.next()) {
                return result.getInt("count") > 0;
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }
        return false;
    }

    void setStaticPointSourceLabel(List<StaticPointSource> staticPointSources) {
        SelectQuery query = Queries.SELECT();
        Variable psVar = query.var();
        Variable labelVar = query.var();

        ValuesPattern<Iri> valuesPattern = new ValuesPattern<>(psVar,
                staticPointSources.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);

        GraphPattern queryPattern = psVar.has(RDFS.LABEL, labelVar);

        query.where(valuesPattern, queryPattern);

        Map<String, StaticPointSource> iriToSpsMap = new HashMap<>();
        staticPointSources.forEach(sps -> iriToSpsMap.put(sps.getIri(), sps));

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        for (int i = 0; i < queryResult.length(); i++) {
            String spsIri = queryResult.getJSONObject(i).getString(psVar.getQueryString().substring(1));
            String labelString = queryResult.getJSONObject(i).getString(labelVar.getQueryString().substring(1));

            iriToSpsMap.get(spsIri).setLabel(labelString);
        }
    }
}
