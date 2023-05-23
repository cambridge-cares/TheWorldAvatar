package com.cmclinnovations.aermod;

import org.eclipse.rdf4j.model.vocabulary.GEOF;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.geom.Polygon;
import org.postgis.Point;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;

import com.cmclinnovations.aermod.sparqlbuilder.GeoSPARQL;
import com.cmclinnovations.aermod.sparqlbuilder.ValuesPattern;
import com.cmclinnovations.aermod.objects.Ship;
import com.cmclinnovations.aermod.objects.WeatherData;

import it.unibz.inf.ontop.model.vocabulary.GEO;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;



public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontopStoreClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private TimeSeriesClient<Long> tsClientLong;
    private TimeSeriesClient<Instant> tsClientInstant;

    // prefixes
    private static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final String PREFIX_DISP = "http://www.theworldavatar.com/kg/dispersion/";
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String CHEM = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#" ;
    public static final String ONTO_CITYGML = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";

    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri(OM_STRING));
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX_DISP));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));
    private static final Prefix P_GEOF = SparqlBuilder.prefix("geof", iri(GEOF.NAMESPACE));
    private static final Prefix P_EMS = SparqlBuilder.prefix("ems", iri(ONTO_EMS));
    // classes
    public static final String REPORTING_STATION = "https://www.theworldavatar.com/kg/ontoems/ReportingStation";
    public static final String NX = PREFIX_DISP + "nx";
    public static final String NY = PREFIX_DISP + "ny";
    public static final String SCOPE = PREFIX_DISP + "Scope";
    public static final String CITIES_NAMESPACE = "https://www.theworldavatar.com/kg/ontodispersion/OntoCityGMLNamespace";
    public static final String SIMULATION_TIME = PREFIX_DISP + "SimulationTime";
    public static final String NO_X = PREFIX_DISP + "NOx";
    public static final String UHC = PREFIX_DISP + "uHC";
    public static final String CO = PREFIX_DISP + "CO";
    public static final String SO2 = PREFIX_DISP + "SO2";
    public static final String PM10 = PREFIX_DISP + "PM10";
    public static final String PM25 = PREFIX_DISP + "PM2.5";
    public static final String DENSITY = OM_STRING + "Density";
    public static final String TEMPERATURE = OM_STRING + "Temperature";
    public static final String MASS_FLOW = OM_STRING + "MassFlow";
    private static final Iri SHIP = P_DISP.iri("Ship");
    private static final Iri MEASURE = P_OM.iri("Measure");
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

    
    // fixed units for each measured property
	private static final Map<String, Iri> UNIT_MAP = new HashMap<>(); 
	static {
    	UNIT_MAP.put(CLOUD_COVER, UNIT_PERCENTAGE);
    	UNIT_MAP.put(AIR_TEMPERATURE, UNIT_CELCIUS);
    	UNIT_MAP.put(RELATIVE_HUMIDITY, UNIT_PERCENTAGE);
    	UNIT_MAP.put(WIND_SPEED, UNIT_MS);
    	UNIT_MAP.put(WIND_DIRECTION, UNIT_DEGREE);
    }

    public QueryClient(RemoteStoreClient storeClient, RemoteStoreClient ontopStoreClient, RemoteRDBStoreClient rdbStoreClient) {
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

    List<Ship> getShipsWithinTimeAndScopeViaTsClient(long simulationTime, Geometry scope) {
        long simTimeUpperBound = simulationTime + 1800; // +30 minutes
        long simTimeLowerBound = simulationTime - 1800; // -30 minutes

        Map<String,String> measureToShipMap = getMeasureToShipMap();
        List<String> measures = new ArrayList<>(measureToShipMap.keySet());

        List<Ship> ships = new ArrayList<>();
        try (Connection conn = rdbStoreClient.getConnection()) {
            measures.stream().forEach(measure -> {
                TimeSeries<Long> ts = tsClientLong.getTimeSeriesWithinBounds(List.of(measure), simTimeLowerBound, simTimeUpperBound, conn);
                if (ts.getValuesAsPoint(measure).size() > 1) {
                    LOGGER.warn("More than 1 point within this time inverval");
                } else if (ts.getValuesAsPoint(measure).isEmpty()) {
                    return;
                }

                try {
                    // this is to convert from org.postgis.Point to the Geometry class
                    Point postgisPoint = ts.getValuesAsPoint(measure).get(0);
                    String wktLiteral = postgisPoint.getTypeString() + postgisPoint.getValue();

                    Geometry point = new org.locationtech.jts.io.WKTReader().read(wktLiteral);
                    
                    if (scope.covers(point)) {
                        // measureToShipMap.get(measure) gives the iri
                        Ship ship = new Ship(measureToShipMap.get(measure));
                        ship.setLocation(postgisPoint);
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

    Map<String,String> getMeasureToShipMap() {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();
        Variable locationMeasure = query.var();
        Variable property = query.var();

        GraphPattern gp = GraphPatterns.and(ship.isA(SHIP).andHas(HAS_PROPERTY, property),
        property.isA(LOCATION).andHas(HAS_VALUE, locationMeasure));

        query.where(gp).prefix(P_OM,P_DISP);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        Map<String,String> locationMeasureToShipMap = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String locationMeasureIri = queryResult.getJSONObject(i).getString(locationMeasure.getQueryString().substring(1));
            String shipIri = queryResult.getJSONObject(i).getString(ship.getQueryString().substring(1));
            locationMeasureToShipMap.put(locationMeasureIri, shipIri);
        }

        return locationMeasureToShipMap;
    }

    void setEmissions(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable ship = query.var();
        Variable entity = query.var();
        Variable entityType = query.var();
        Variable quantity = query.var();
        Variable quantityType = query.var();
        Variable numericalValue = query.var();

        ValuesPattern<Iri> shipValues = new ValuesPattern<>(ship, ships.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);
        
        GraphPattern gp = GraphPatterns.and(derivation.has(IS_DERIVED_FROM, ship), entity.has(BELONGS_TO, derivation).andIsA(entityType)
        .andHas(HAS_QUANTITY, quantity), quantity.isA(quantityType).andHas(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE),numericalValue));

        query.where(gp,shipValues).prefix(P_OM);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        // create a look up map to get ship object based on IRI
        Map<String, Ship> iriToShipMap = new HashMap<>();
        ships.stream().forEach(s -> iriToShipMap.put(s.getIri(), s));

        for (int i = 0; i < queryResult.length(); i++) {
            String shipIri = queryResult.getJSONObject(i).getString(ship.getQueryString().substring(1));
            double literalValue = queryResult.getJSONObject(i).getDouble(numericalValue.getQueryString().substring(1));
            Ship shipObject = iriToShipMap.get(shipIri);

            String entityTypeIri = queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1));
            String quantityTypeIri = queryResult.getJSONObject(i).getString(quantityType.getQueryString().substring(1));

            if (entityTypeIri.contentEquals(PM10) && quantityTypeIri.contentEquals(MASS_FLOW)) {
                // PM10 flowrate
                shipObject.getChimney().setPM10(literalValue);
            } else if (entityTypeIri.contentEquals(PM25) && quantityTypeIri.contentEquals(MASS_FLOW)) {
                // PM2.5 flowrate
                shipObject.getChimney().setPM25(literalValue);
            } else if (entityTypeIri.contentEquals(PM25) && quantityTypeIri.contentEquals(DENSITY)) {
                // particle density
                shipObject.getChimney().setParticleDensity(literalValue);
            } else if (entityTypeIri.contentEquals(SO2) && quantityTypeIri.contentEquals(TEMPERATURE)) {
                // all gas mixtures share the same temperature
                shipObject.getChimney().setMixtureTemperatureInKelvin(literalValue);
            } else if (entityTypeIri.contentEquals(SO2) && quantityTypeIri.contentEquals(DENSITY)) {
                // all gas mixtures share the same density
                shipObject.getChimney().setMixtureDensityInKgm3(literalValue);
            } else if (entityTypeIri.contentEquals(SO2) && quantityTypeIri.contentEquals(MASS_FLOW)) {
                shipObject.getChimney().setFlowrateSO2(literalValue);
            }
        }
    }

    /**
     * returns derivation IRIs for each ship by querying
     * <derivation> isDerivedFrom <ship>
     * @param ships
     * @return
     */
    List<String> getDerivationsOfShips(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable ship = query.var();
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        ValuesPattern<Iri> vp = new ValuesPattern<>(ship, ships.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = derivation.has(isDerivedFrom, ship);

        query.where(gp,vp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        
        List<String> derivations = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            derivations.add(queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1)));
        }

        return derivations;
    }

       // SPARQL query for weather data instantiated by OpenMeteo agent as a timeseries. Each parameter has multiple values.
       // The weather data is stored as one timeseries per parameter. Hence, the values for each parameter are in separate postgres tables.
       WeatherData getOpenMeteoData(String stationIRI) {
        SelectQuery query = Queries.SELECT();

        Variable weatherType = query.var();
        Variable quantity = query.var();
        Variable measure = query.var();
        Variable weatherUnit = query.var();
        Variable station = query.var();


        // RDF types for weather data
        List<String> weatherTypeList = List.of(CLOUD_COVER, AIR_TEMPERATURE, RELATIVE_HUMIDITY, WIND_SPEED, WIND_DIRECTION);

        ValuesPattern<Iri> vp = new ValuesPattern<>(weatherType, weatherTypeList.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);

        GraphPattern gp = GraphPatterns.and(iri(stationIRI).has(RDF.TYPE,iri(REPORTING_STATION)).andHas(REPORTS, quantity),
        quantity.isA(weatherType).andHas(HAS_VALUE, measure), measure.has(HAS_UNIT, weatherUnit));

        query.prefix(P_OM, P_EMS).where(gp,vp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        WeatherData weatherData = new WeatherData();


        for (int i = 0; i < queryResult.length(); i++) {
            String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
            String weatherTypeIri = queryResult.getJSONObject(i).getString(weatherType.getQueryString().substring(1));
            TimeSeries<Instant> ts;
            try (Connection conn = rdbStoreClient.getConnection()) {
                ts = tsClientInstant.getTimeSeriesWithinBounds(Arrays.asList(measureIri), null, null, conn);
            } catch (SQLException e) {
                String errmsg = "Failed to obtain time series from weather station instantiated by OpenMeteo Agent.";
                LOGGER.fatal(errmsg);
                throw new RuntimeException(errmsg, e);
            }
            List<Double> values = ts.getValuesAsDouble(measureIri);
            switch (weatherTypeIri) {
                case CLOUD_COVER:
                    weatherData.setCloudCoverInPercentage(values);
                    break;
                case AIR_TEMPERATURE:
                    weatherData.setTemperatureInCelcius(values);
                    break;
                case RELATIVE_HUMIDITY:
                    weatherData.setHumidityInPercentage(values);
                    break;
                case WIND_SPEED:
                    weatherData.setWindSpeedInMetreSecond(values);
                    break;
                case WIND_DIRECTION:
                    weatherData.setWindDirectionInDegrees(values);
                    break;
                default:
                    LOGGER.info("Unknown weather RDF type: <{}>", weatherTypeIri);
            }

        }

        return weatherData;
    }

    void updateVirtualSensorData(List<String> timeStamps, JSONArray virtualSensorConcentrations) {
        // Create OntoEMS stations
        LOGGER.info("Instantiating pollutant concentrations of timeseries for virtual sensors");

        // valuesListForTimeSeries cannot be of type List<List<Double>>> to be used with TimeSeries.  
        List<String> dataListForTimeSeries = new ArrayList<>();
        List<List<?>> valuesListForTimeSeries = new ArrayList<>();

        ModifyQuery modify = Queries.MODIFY();
        modify.prefix(P_EMS,P_OM);


        // create a JSONObject that represents a GeoJSON Feature Collection
        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");
        JSONArray features = new JSONArray();

        for (int i = 0; i < virtualSensorConcentrations.length(); i++) {
            JSONObject sensorProperties = virtualSensorConcentrations.getJSONObject(i);
            String stationIRI = ONTO_EMS + "sensorstation_" + UUID.randomUUID();
            Iri station = iri(stationIRI);

            
            double lat = sensorProperties.getDouble("latitude");
            double lon = sensorProperties.getDouble("longitude");

            // Create POSTGIS and GeoServer feature for this OntoEMS station
            JSONObject geometry = new JSONObject();
            geometry.put("type", "Point");
            List<Double> coordinate = Arrays.asList(lon,lat);
            geometry.put("coordinates", new JSONArray(coordinate));
            JSONObject feature = new JSONObject();
            feature.put("type", "Feature");
            feature.put("geometry", geometry);
            feature.put("iri",stationIRI);
            feature.put("name", "VirtualSensor_"+(i+1));
            feature.put("endpoint",storeClient.getUpdateEndpoint());
            features.put(feature);


            // Update triples for station in blazegraph
            String locString = String.valueOf(lat) + "#" + String.valueOf(lon);
            modify.insert(station.isA(iri(REPORTING_STATION)).andHas(OBSERVATION_LOCATION, locString));

            Iri quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
			String measureIri = ONTO_EMS + "measure_" + UUID.randomUUID();
    		Iri measure = iri(measureIri);

                      // triples to insert for each station
    		modify.insert(station.has(REPORTS,quantity));
    		modify.insert(quantity.isA(iri(NO2_CONC)).andHas(HAS_VALUE,measure));
    		modify.insert(measure.isA(MEASURE).andHas(HAS_UNIT,UNIT_POLLUTANT_CONC)); 

            dataListForTimeSeries.add(measureIri);           
  
            
            JSONArray concentrationTimeSeries = sensorProperties.getJSONArray("concentrations");
            List<Double> concentrationValues = new ArrayList<>();

            for (int j = 0; j < timeStamps.size(); j++){
                concentrationValues.add(concentrationTimeSeries.getDouble(j));
            }

            List<?> tmp = (List<?>) concentrationValues ;
            valuesListForTimeSeries.add(tmp);

        }

        storeClient.executeUpdate(modify.getQueryString());

        List<Class<?>> classListForTimeSeries = Collections.nCopies(dataListForTimeSeries.size(), Double.class);
        TimeSeriesClient<LocalDateTime> tsClientLocalDateTime = new TimeSeriesClient<>(storeClient, LocalDateTime.class);

        List<LocalDateTime> timesList =  timeStamps.stream().
        map(i -> LocalDateTime.parse(i, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))).collect(Collectors.toList());

        TimeSeries<LocalDateTime> ts = new TimeSeries<>(timesList, dataListForTimeSeries, valuesListForTimeSeries);   
        

        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClientLocalDateTime.initTimeSeries(dataListForTimeSeries, classListForTimeSeries, LocalDateTime.class.getSimpleName(),conn);
            tsClientLocalDateTime.addTimeSeriesData(ts,conn); 
        } catch (SQLException e) {
            LOGGER.error("SQL exception when updating virtual sensor data.");
            LOGGER.error(e.getMessage());
        }

        // Upload virtual sensor layer to POSTGIS and GeoServer for visualization purposes

        featureCollection.put("features", features);

        LOGGER.info("Uploading virtual sensors GeoJSON to PostGIS");
		GDALClient gdalclient = GDALClient.getInstance();
		gdalclient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, "sensor_layer", featureCollection.toString(), new Ogr2OgrOptions(), true);

		LOGGER.info("Creating plant items layer in Geoserver");
		GeoServerClient geoserverclient = GeoServerClient.getInstance();
		geoserverclient.createWorkspace(EnvConfig.GEOSERVER_WORKSPACE);
		geoserverclient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, "sensor_layer", new GeoServerVectorSettings());


        /* Offset not used for the time being */
        // List<OffsetDateTime> offsetTimesList = new ArrayList<>();
        // for (int i = 0; i < timesList.size(); i++) {
        //     LocalDateTime ldt = timesList.get(i);
        //     OffsetDateTime odt = OffsetDateTime.of(ldt, ZoneOffset.UTC);
        //     offsetTimesList.add(odt);
        // }
        
    }






    // SPARQL query for weather data instantiated by weather agent as a timeseries. Each parameter has a single value.
    WeatherData getWeatherData(String station, long timestamp) {
        SelectQuery query = Queries.SELECT();

        Variable weatherType = query.var();
        Variable quantity = query.var();
        Variable measure = query.var();
        Variable weatherUnit = query.var();

        // RDF types for weather data
        List<String> weatherTypeList = List.of(CLOUD_COVER, AIR_TEMPERATURE, RELATIVE_HUMIDITY, WIND_SPEED, WIND_DIRECTION);

        ValuesPattern<Iri> vp = new ValuesPattern<>(weatherType, weatherTypeList.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);

        // ValuesPattern vp = new ValuesPattern(weatherType, weatherUnit);
        // weatherTypeList.stream().forEach(type -> vp.addValuePairForMultipleVariables(iri(type), UNIT_MAP.get(type)));

        GraphPattern gp = GraphPatterns.and(iri(station).has(REPORTS, quantity),
        quantity.isA(weatherType).andHas(HAS_VALUE, measure), measure.has(HAS_UNIT, weatherUnit));

        query.prefix(P_OM, P_EMS).where(gp,vp);

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
            ts = tsClientInstant.getTimeSeriesWithinBounds(typeToMeasureMap.values().stream().collect(Collectors.toList()), timestampAsInstant, timestampAsInstant, conn);
        } catch (SQLException e) {
            String errmsg = "Failed to obtain time series from weather station";
            LOGGER.fatal(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        WeatherData weatherData = new WeatherData();
        weatherTypeList.stream().forEach(type -> {
            double val = ts.getValuesAsDouble(typeToMeasureMap.get(type)).get(0);
            List<Double> value = Arrays.asList(val);
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

    void updateOutputs(String derivation, String dispersionMatrix, String dispersionLayer, String shipLayer, long timeStamp) {
        // first query the IRIs
        SelectQuery query = Queries.SELECT();

        Variable entity = query.var();
        Variable entityType = query.var();

        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        query.where(entity.has(belongsTo, iri(derivation)).andIsA(entityType)).prefix(P_DISP).select(entity,entityType).distinct();

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

        TimeSeries<Long> timeSeries = new TimeSeries<>(List.of(timeStamp), List.of(dispersionMatrixIri, dispersionLayerIri, shipLayerIri), values);

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

        Map<String,String> measureToShipMap = getMeasureToShipMap();
        List<String> locationMeasures = new ArrayList<>(measureToShipMap.keySet());

        Map<String, String> geometryToMeasureMap = getGeometryToMeasureMap(locationMeasures);
        List<String> geometryIrisWithinTimeBounds = getGeometriesWithinTimeBounds(new ArrayList<>(geometryToMeasureMap.keySet()), simTimeLowerBound, simTimeUpperBound);
        String scopeGeometry = getScopeGeometry(scopeIri);
        List<String> geometriesWithinScopeAndTime = getGeometriesWithinScope(scopeGeometry, geometryIrisWithinTimeBounds);

        List<String> shipsWithinTimeAndScope = new ArrayList<>();
        geometriesWithinScopeAndTime.forEach(g -> 
            shipsWithinTimeAndScope.add(measureToShipMap.get(geometryToMeasureMap.get(g)))
        );

        return shipsWithinTimeAndScope;
    }

    /**
     * to ontop
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
     * @param locationMeasures
     * @return
     */
    @Deprecated
    Map<String, String> getGeometryToMeasureMap(List<String> locationMeasures) {
        SelectQuery query = Queries.SELECT();
        Variable locationMeasure = query.var();
        Variable geometry = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(locationMeasure, locationMeasures.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = locationMeasure.has(HAS_GEOMETRY,geometry);

        query.prefix(P_GEO).where(vp,gp);

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
     * @param geometryIris
     * @param simTimeLowerBound
     * @param simTimeUpperBound
     */
    @Deprecated
    List<String> getGeometriesWithinTimeBounds(List<String> geometryIris, long simTimeLowerBound, long simTimeUpperBound) {
        SelectQuery query = Queries.SELECT();

        Variable shipTime = query.var();
        Variable geometry = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(geometry, geometryIris.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = GraphPatterns.and(vp,geometry.has(P_DISP.iri("hasTime"), shipTime));

        query.prefix(P_GEO,P_GEOF,P_OM,P_DISP).
        where(gp.filter
        (Expressions.and(Expressions.gt(shipTime, simTimeLowerBound), Expressions.lt(shipTime, simTimeUpperBound))));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        List<String> geometriesWithinTimeBounds = new ArrayList<>();

        for (int i = 0; i < queryResult.length(); i++) {
            geometriesWithinTimeBounds.add(queryResult.getJSONObject(i).getString(geometry.getQueryString().substring(1)));
        }

        return geometriesWithinTimeBounds;
    }

    @Deprecated
    List<String> getGeometriesWithinScope(String scopeGeometry, List<String> shipGeometriesWithinTimeBounds) {
        SelectQuery query = Queries.SELECT();
        Variable shipGeometry = query.var();
        Variable scopeWkt = query.var();
        Variable shipWkt = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(shipGeometry, shipGeometriesWithinTimeBounds.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);

        GraphPattern gp = GraphPatterns.and(vp, shipGeometry.has(AS_WKT, shipWkt), iri(scopeGeometry).has(AS_WKT, scopeWkt));

        query.where(gp.filter(Expressions.and(GeoSPARQL.sfIntersects(shipWkt, scopeWkt)))).prefix(P_GEO,P_GEOF);

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        List<String> geometriesWithinScope = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            geometriesWithinScope.add(queryResult.getJSONObject(i).getString(shipGeometry.getQueryString().substring(1)));
        }
        return geometriesWithinScope;
    }
}
