package uk.ac.cam.cares.jps.agent.weather;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.SQLException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.StringLiteral;
import org.json.JSONArray;
import org.json.JSONObject;
import org.postgis.Point;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.VTGeometryEncoder;

import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

class WeatherQueryClient {
	private static final Logger LOGGER = LogManager.getLogger(WeatherQueryClient.class);

	// prefix
	static String ontoems = "https://www.theworldavatar.com/kg/ontoems/";
    static Prefix p_ems = SparqlBuilder.prefix("ems",iri(ontoems));
	static Prefix p_om = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
	private static Prefix p_geo = SparqlBuilder.prefix("geo",iri("http://www.bigdata.com/rdf/geospatial#"));
    
    // classes
	private static Iri ReportingStation = p_ems.iri("ReportingStation");
	private static Iri lat_lon = iri("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon");
    static String CloudCover = ontoems +"CloudCover"; // not in ems tbox, should be subclass of om:Ratio
    static String Rainfall = ontoems + "Rainfall";
    static String AtmosphericPressure = ontoems + "AtmosphericPressure";
    static String AirTemperature = ontoems + "AirTemperature";
    static String RelativeHumidity = ontoems + "RelativeHumidity";
    static String WindSpeed = ontoems + "WindSpeed";
    static String WindDirection = ontoems + "WindDirection";
	static Iri Measure = p_om.iri("Measure");
    
    // properties
    static Iri reports = p_ems.iri("reports");
    static Iri hasValue = p_om.iri("hasValue");
    private static Iri hasUnit = p_om.iri("hasUnit");
	private static Iri hasObservationLocation = p_ems.iri("hasObservationLocation");
	private static Iri asWKT = iri("http://www.opengis.net/ont/geosparql#asWKT");
    
    // IRI of units used
    private static Iri unit_mm = p_om.iri("millimetre");
    private static Iri unit_degree = p_om.iri("degree");
    private static Iri unit_mbar = p_om.iri("millibar");
    private static Iri unit_celcius = p_om.iri("degreeCelsius");
    private static Iri unit_ms = p_om.iri("metrePerSecond-Time");
    private static Iri unit_fraction = p_om.iri("RatioUnit"); 
    private static Iri unit_percentage = p_om.iri("PercentageUnit");
    
    // measured properties
    private static List<String> weatherClasses = Arrays.asList(CloudCover, Rainfall,
	    AtmosphericPressure, AirTemperature, RelativeHumidity, WindSpeed, WindDirection);
    
    // fixed units for each measured property
    @SuppressWarnings("serial")
	static Map<String, Iri> unitMap = new HashMap<String , Iri>() {{
    	put(CloudCover, unit_percentage);
    	put(Rainfall, unit_mm);
    	put(AtmosphericPressure, unit_mbar);
    	put(AirTemperature, unit_celcius);
    	put(RelativeHumidity, unit_fraction);
    	put(WindSpeed, unit_ms);
    	put(WindDirection, unit_degree);
    }};
    
    StoreClientInterface storeClient;
    TimeSeriesClient<Instant> tsClient;
	
    // SERVICE keyword is not supported by query builder
    private static String geospatialQueryTemplate = "PREFIX geo: <http://www.bigdata.com/rdf/geospatial#>\r\n"
    		+ "PREFIX ems: <%s>\r\n"
    		+ "\r\n"
    		+ "SELECT * WHERE {\r\n"
    		+ "    SERVICE geo:search \r\n"
    		+ "	%s\r\n"
    		+ "}";
    		
    // constructor 1
    WeatherQueryClient(StoreClientInterface storeClient, TimeSeriesClient<Instant> tsClient) {
    	this.storeClient = storeClient;
    	this.tsClient = tsClient;
    }
    
    // constructor 2 for situations when time series is not needed
    WeatherQueryClient(StoreClientInterface storeClient) {
    	this.storeClient = storeClient;
    }
    
    /**
     * create a new station at the given coordinates, must be EPSG:4326
     * @param lat
     * @param lon
     * @return
     */
    String createStation(double lat, double lon, String name) {
		String station_iri = ontoems + "weatherstation_" + UUID.randomUUID();

		// create geojson object for PostGIS
		JSONObject geojson = new JSONObject();
		JSONObject geometry = new JSONObject();
		JSONObject properties = new JSONObject();
		geometry.put("type","Point");
		geometry.put("coordinates", new JSONArray().put(lon).put(lat));
		properties.put("iri", station_iri);
		properties.put("type", "weather");
		if (name != null) {
			properties.put("name", name);
		} else {
			properties.put("name", String.format("Weather Station at (%f, %f)", lat, lon));
		}
		
		geojson.put("type","Feature").put("properties",properties).put("geometry",geometry);

		LOGGER.info("Uploading GeoJSON to PostGIS");
		GDALClient gdalclient = new GDALClient();
		gdalclient.uploadVectorStringToPostGIS(Config.DATABASE, Config.LAYERNAME, geojson.toString(), new Ogr2OgrOptions(), true);

		LOGGER.info("Creating layer in Geoserver");
		GeoServerClient geoserverclient = new GeoServerClient();
		geoserverclient.createWorkspace(Config.GEOSERVER_WORKSPACE);
		geoserverclient.createPostGISLayer(null, Config.GEOSERVER_WORKSPACE, Config.DATABASE, Config.LAYERNAME, new GeoServerVectorSettings());

		LOGGER.info("Instantiating weather station in triple-store");
		ModifyQuery modify = Queries.MODIFY();
    	
    	Iri station = iri(station_iri);

    	modify.insert(station.isA(ReportingStation));
    	
    	List<String> datalist_for_timeseries = new ArrayList<>();
    	List<Class<?>> classlist_for_timeseries = new ArrayList<>();
    	
    	// add 1 sensor per property
    	for (String weatherClass : weatherClasses) {
    		Iri quantity = p_ems.iri("quantity_" + UUID.randomUUID());
			String measureIri = ontoems + "measure_" + UUID.randomUUID();
    		Iri measure = iri(measureIri);
    		
    		// to create time series table later
    		datalist_for_timeseries.add(measureIri);
    		classlist_for_timeseries.add(Double.class);
    		
    		// triples to insert
    		modify.insert(station.has(reports,quantity));
    		modify.insert(quantity.isA(iri(weatherClass)).andHas(hasValue,measure));
    		modify.insert(measure.isA(Measure).andHas(hasUnit,unitMap.get(weatherClass)));
    	}
    	
    	modify.prefix(p_ems,p_om);
    	
    	// insert triples in the triple-store
    	storeClient.executeUpdate(modify.getQueryString());
    	
		LOGGER.info("Creating time series for station");
    	// then create a table for this weather station
    	tsClient.initTimeSeries(datalist_for_timeseries, classlist_for_timeseries, null);
    	
    	// populate with current weather data
    	updateStation(station_iri);
    	
    	return station_iri;
    }
    
    /**
     * deletes the given station, including its associated time series
     * @param station_iri
     */
    void deleteStation(String station_iri) {
    	Iri station = iri(station_iri);
    	
    	SelectQuery query = Queries.SELECT();
    	
    	Variable coordinates = query.var();
    	Variable quantity = query.var();
    	Variable weatherClass = query.var();
    	Variable datavalue = query.var();
    	Variable units = query.var();
    	
    	TriplePattern[] queryPattern = {station.isA(ReportingStation).andHas(hasObservationLocation,coordinates),
    			station.has(reports,quantity),
    			quantity.isA(weatherClass).andHas(hasValue,datavalue),
    			datavalue.isA(Measure).andHas(hasUnit,units)};
    	
    	// first we record the data value IRIs, required to delete in RDB
    	query.prefix(p_om,p_ems).where(queryPattern).select(datavalue);
    	
    	// this is the list of iris with a corresponding time series table
		@SuppressWarnings("unchecked")
		// substring(1) to get rid of "?"
		List<String> datalist = storeClient.executeQuery(query.getQueryString()).toList().stream()
				.map(datavalueiri -> ((HashMap<String,String>) datavalueiri).get(datavalue.getQueryString().substring(1))).collect(Collectors.toList());
    	
		// this deletes the station instance, but not the time series triples
    	ModifyQuery modify = Queries.MODIFY();
    	modify.prefix(p_ems,p_om).delete(queryPattern).where(queryPattern);
    	storeClient.executeUpdate(modify.getQueryString());
    	
    	// use time series client to delete time series related data
    	// get time series IRI for this data set, any data IRI will give the same time series
    	TimeSeriesSparql tsSparql = new TimeSeriesSparql(storeClient);
    	String timeseriesIRI = tsSparql.getTimeSeries(datalist.get(0));

    	// then delete all time series data in one go
    	tsClient.deleteTimeSeries(timeseriesIRI);
    }
    
    Instant getLastUpdateTime(String station_iri) {
    	// query measure IRI
		SelectQuery query = Queries.SELECT();

		Variable measure = query.var();
		Iri station = iri(station_iri);

		GraphPattern gp =  station.has(PropertyPaths.path(reports,hasValue),measure);

		query.prefix(p_ems,p_om).where(gp).select(measure);

		List<String> measures = storeClient.executeQuery(query.getQueryString()).toList().stream()
		.map(m -> ((HashMap<String,String>) m).get(measure.getQueryString().substring(1))).collect(Collectors.toList());

		if (measures.size() > 0) {
			return tsClient.getLatestData(measures.get(0)).getTimes().get(0);
		} else {
			String errmsg = station_iri + " probably does not exist";
			LOGGER.error(errmsg);
			throw new RuntimeException(errmsg);
		}
    }
    
    /**
     * updates weather station with the latest data
     * @param station_iri
     * @throws SQLException
     */
    void updateStation(String station_iri) {
		// get the coordinates of this station
		// build coordinate query
		SelectQuery query2 = Queries.SELECT();
		Variable wkt = query2.var();
		query2.select(wkt).where(iri(station_iri).has(asWKT,wkt));
		
		// submit coordinate query to ontop
		String wkt_string = new RemoteStoreClient(Config.ontop_url).executeQuery(query2.getQueryString()).getJSONObject(0).getString(wkt.getQueryString().substring(1));
		Point point;
		try {
			point = new Point(wkt_string);
		} catch (SQLException e) {
			LOGGER.error("Error parsing queried wkt literal");
			throw new RuntimeException(e);
		}
		
		// the key for this map is the weather class, value is the corresponding value
		Map<String,Double> newWeatherData = WeatherAPIConnector.getWeatherDataFromOpenWeather(point.getY(), point.getX());
	    
		// to construct time series object
		List<String> datavalue_list = new ArrayList<>();
		List<List<?>> value_list = new ArrayList<>();
		
		// query measure IRIs and match numerical values to it
		SelectQuery query3 = Queries.SELECT();
		Variable quantity = query3.var();
		Variable measure = query3.var();
		Variable weatherType = query3.var();
		
		GraphPattern gp = GraphPatterns.and(iri(station_iri).has(reports,quantity), quantity.isA(weatherType).andHas(hasValue,measure));
		
		query3.select(measure, weatherType).where(gp).prefix(p_om,p_ems);

		JSONArray queryResults = storeClient.executeQuery(query3.getQueryString());

		for (int i = 0; i < queryResults.length(); i++) {
			JSONObject queryResult = queryResults.getJSONObject(i);
			datavalue_list.add(queryResult.getString(measure.getQueryString().substring(1)));
			double numericalValue = newWeatherData.get(queryResult.getString(weatherType.getQueryString().substring(1)));
			value_list.add(Arrays.asList(numericalValue));
		}
		
		// append new values to time series table
		TimeSeries<Instant> ts = new TimeSeries<Instant>(Arrays.asList(Instant.now()), datavalue_list, value_list);
		tsClient.addTimeSeriesData(ts);
    }

    /**
     * get latest weather data
     * @param station_iri
     */
	TimeSeries<Instant> getLatestWeatherData(String station_iri) {
    	// first query the data IRIs
    	SelectQuery query = Queries.SELECT();
    	
    	Iri station = iri(station_iri);
    	Variable measure = query.var();
    	
    	GraphPattern queryPattern = station.has(PropertyPaths.path(reports,hasValue), measure);
    	
    	query.select(measure).where(queryPattern).prefix(p_ems,p_om);
    	
    	List<String> datalist = storeClient.executeQuery(query.getQueryString()).toList().stream()
				.map(datavalueiri -> ((HashMap<String,String>) datavalueiri).get(measure.getQueryString().substring(1))).collect(Collectors.toList());
    	
    	Instant latestTime = tsClient.getMaxTime(datalist.get(0));
    	TimeSeries<Instant> ts = tsClient.getTimeSeriesWithinBounds(datalist, latestTime, latestTime);
    	
    	return ts;
    }
    
	/**
	 * returns the historical weather data up to number of hours before current time
	 * @param station_iri
	 * @return
	 */
	TimeSeries<Instant> getHistoricalWeatherData(String station_iri, int hour) {
		// first query the data IRIs
    	SelectQuery query = Queries.SELECT();
    	
    	Iri station = iri(station_iri);
    	Variable measure = query.var();
    	
    	GraphPattern queryPattern = station.has(PropertyPaths.path(reports,hasValue),measure);
    	
    	query.select(measure).where(queryPattern).prefix(p_ems,p_om);
    	
    	List<String> datalist = storeClient.executeQuery(query.getQueryString()).toList().stream()
				.map(datavalueiri -> ((HashMap<String,String>) datavalueiri).get(measure.getQueryString().substring(1))).collect(Collectors.toList());
    	
    	Instant latestTime = tsClient.getMaxTime(datalist.get(0));
    	Instant queryEarliestTime = Instant.now().minus(Duration.ofHours(1));
    	TimeSeries<Instant> ts = tsClient.getTimeSeriesWithinBounds(datalist, queryEarliestTime, latestTime);
    	
    	return ts;
	}
}
