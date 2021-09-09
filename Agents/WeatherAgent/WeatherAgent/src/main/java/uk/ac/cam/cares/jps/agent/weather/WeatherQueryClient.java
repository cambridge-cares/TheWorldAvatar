package uk.ac.cam.cares.jps.agent.weather;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
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

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

class WeatherQueryClient {
	// prefix
	static String ontostation = "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontostation/OntoStation.owl#";
    static Prefix p_station = SparqlBuilder.prefix("station",iri(ontostation));
    static Prefix p_ontosensor = SparqlBuilder.prefix("sensor",iri("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#"));
    static Prefix p_system = SparqlBuilder.prefix("system",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"));
    private static Prefix p_derived_SI_unit = SparqlBuilder.prefix("derived_SI_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#"));
    private static Prefix p_time = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
    private static Prefix p_geo = SparqlBuilder.prefix("geo",iri("http://www.bigdata.com/rdf/geospatial#"));
    
    // classes
    private static Iri WeatherStation = p_station.iri("WeatherStation");
    private static Iri lat_lon = iri("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon");
    private static Iri MeasuringInstrument = iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_system.owl#MeasuringInstrument");
    private static Iri ScalarValue = p_system.iri("ScalarValue");
    static Iri OutsideAirCloudCover = p_ontosensor.iri("OutsideAirCloudCover");
    static Iri OutsideAirPrecipitation = p_ontosensor.iri("OutsideAirPrecipitation");
    static Iri OutsideAirPressure = p_ontosensor.iri("OutsideAirPressure");
    static Iri OutsideAirTemperature = p_ontosensor.iri("OutsideAirTemperature");
    static Iri OutsideAirRelativeHumidity = p_ontosensor.iri("OutsideAirRelativeHumidity");
    static Iri OutsideWindSpeed = p_ontosensor.iri("OutsideWindSpeed");
    static Iri OutsideWindDirection = p_ontosensor.iri("OutsideWindDirection");
    
    // properties
    static Iri hasCoordinates = p_station.iri("hasCoordinates");
    static Iri hasSubsystem = p_system.iri("hasSubsytem");
    static Iri observes = p_ontosensor.iri("observes");
    static Iri hasValue = p_system.iri("hasValue");
    private static Iri hasUnitOfMeasure = p_system.iri("hasUnitOfMeasure");
    private static Iri hasTime = p_time.iri("hasTime");
	private static Iri numericPosition = p_time.iri("numericPosition");
	private static Iri inTimePosition = p_time.iri("inTimePosition");
    
    // IRI of units used
    private static Iri unit_mm = p_derived_SI_unit.iri("mm");
    private static Iri unit_degree = p_derived_SI_unit.iri("degree");
    private static Iri unit_mbar = p_derived_SI_unit.iri("mBar");
    private static Iri unit_celcius = p_derived_SI_unit.iri("Celsius");
    private static Iri unit_ms = p_derived_SI_unit.iri("m_per_s");
    private static Iri unit_fraction = p_derived_SI_unit.iri("fraction"); // made up, does not exist in ontology
    private static Iri unit_percentage = p_derived_SI_unit.iri("percentage"); // made up, does not exist in ontology
    
    // measured properties
    private static List<Iri> weatherClasses = Arrays.asList(OutsideAirCloudCover, OutsideAirPrecipitation,
    		OutsideAirPressure, OutsideAirTemperature, OutsideAirRelativeHumidity, OutsideWindSpeed, OutsideWindDirection);
    
    // fixed units for each measured property
    @SuppressWarnings("serial")
	static Map<Iri, Iri> unitMap = new HashMap<Iri , Iri>() {{
    	put(OutsideAirCloudCover, unit_percentage);
    	put(OutsideAirPrecipitation, unit_mm);
    	put(OutsideAirPressure, unit_mbar);
    	put(OutsideAirTemperature, unit_celcius);
    	put(OutsideAirRelativeHumidity, unit_fraction);
    	put(OutsideWindSpeed, unit_ms);
    	put(OutsideWindDirection, unit_degree);
    }};
    
    StoreClientInterface storeClient;
    TimeSeriesClient<Long> tsClient;
	
    // SERVICE keyword is not supported by query builder
    private static String geospatialQueryTemplate = "PREFIX geo: <http://www.bigdata.com/rdf/geospatial#>\r\n"
    		+ "PREFIX station: <%s>\r\n"
    		+ "\r\n"
    		+ "SELECT * WHERE {\r\n"
    		+ "    SERVICE geo:search \r\n"
    		+ "	%s\r\n"
    		+ "}";
    		
    // constructor 1
    WeatherQueryClient(StoreClientInterface storeClient, TimeSeriesClient<Long> tsClient) {
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
    String createStation(String latlon) {
    	ModifyQuery modify = Queries.MODIFY();
    	String station_name = "weatherstation_" + UUID.randomUUID();
    	Iri station = p_station.iri(station_name);
    	
    	// coordinates
    	StringLiteral coordinatesLiteral = Rdf.literalOfType(latlon, lat_lon);
    	modify.insert(station.isA(WeatherStation).andHas(hasCoordinates,coordinatesLiteral));
    	
    	List<String> datalist_for_timeseries = new ArrayList<>();
    	List<Class<?>> classlist_for_timeseries = new ArrayList<>();
    	
    	// add 1 sensor per property
    	for (Iri weatherClass : weatherClasses) {
    		Iri sensor = p_station.iri("sensor_" + UUID.randomUUID());
    		Iri data = p_station.iri("data_" + UUID.randomUUID());
    		String datavalue = ontostation + "datavalue_" + UUID.randomUUID();
    		Iri datavalue_iri = iri(datavalue);
    		
    		// to create time series table later
    		datalist_for_timeseries.add(datavalue);
    		classlist_for_timeseries.add(Double.class);
    		
    		// triples to insert
    		modify.insert(station.has(hasSubsystem,sensor));
    		modify.insert(sensor.isA(MeasuringInstrument).andHas(observes,data));
    		modify.insert(data.isA(weatherClass).andHas(hasValue,datavalue_iri));
    		modify.insert(datavalue_iri.isA(ScalarValue).andHas(hasUnitOfMeasure,unitMap.get(weatherClass)));
    	}
    	
    	modify.prefix(p_station,p_system,p_ontosensor,p_derived_SI_unit);
    	
    	// insert triples in the triple-store
    	storeClient.executeUpdate(modify.getQueryString());
    	
    	// then create a table for this weather station
    	tsClient.initTimeSeries(datalist_for_timeseries, classlist_for_timeseries, null);
    	
    	// add timestamp to the station to use this as an input to a derivation
    	DerivationClient dClient = new DerivationClient(storeClient);
    	dClient.addTimeInstance(ontostation+station_name);
    	
    	// populate with current weather data
    	updateStation(ontostation+station_name);
    	
    	return ontostation+station_name;
    }
    
    /**
     * deletes the given station, including its associated time series
     * @param station_iri
     */
    void deleteStation(String station_iri) {
    	Iri station = iri(station_iri);
    	
    	SelectQuery query = Queries.SELECT();
    	
    	Variable coordinates = query.var();
    	Variable sensor = query.var();
    	Variable data = query.var();
    	Variable weatherClass = query.var();
    	Variable datavalue = query.var();
    	Variable units = query.var();
    	
    	TriplePattern[] queryPattern = {station.isA(WeatherStation).andHas(hasCoordinates,coordinates),
    			station.has(hasSubsystem,sensor),
    			sensor.isA(MeasuringInstrument).andHas(observes,data),
    			data.isA(weatherClass).andHas(hasValue,datavalue),
    			datavalue.isA(ScalarValue).andHas(hasUnitOfMeasure,units)};
    	
    	// first we record the data value IRIs, required to delete in RDB
    	query.prefix(p_station,p_system,p_ontosensor).where(queryPattern).select(datavalue);
    	
    	// this is the list of iris with a corresponding time series table
		@SuppressWarnings("unchecked")
		// substring(1) to get rid of "?"
		List<String> datalist = storeClient.executeQuery(query.getQueryString()).toList().stream()
				.map(datavalueiri -> ((HashMap<String,String>) datavalueiri).get(datavalue.getQueryString().substring(1))).collect(Collectors.toList());
    	
		// this deletes the station instance, but not the time series triples
    	ModifyQuery modify = Queries.MODIFY();
    	modify.prefix(p_station,p_system,p_ontosensor).delete(queryPattern).where(queryPattern);
    	storeClient.executeUpdate(modify.getQueryString());
    	
    	// use time series client to delete time series related data
    	// get time series IRI for this data set, any data IRI will give the same time series
    	TimeSeriesSparql tsSparql = new TimeSeriesSparql(storeClient);
    	String timeseriesIRI = tsSparql.getTimeSeries(datalist.get(0));

    	// then delete all time series data in one go
    	tsClient.deleteTimeSeries(timeseriesIRI);
    }
    
    long getLastUpdateTime(String station_iri) {
    	// construct query
    	SelectQuery query = Queries.SELECT();
    	Variable timestamp = query.var();
    	Iri[] time_predicates = {hasTime,inTimePosition,numericPosition};
    	GraphPattern queryPattern = getQueryGraphPattern(query,time_predicates,iri(station_iri),timestamp);
    	query.select(timestamp).where(queryPattern).prefix(p_time);
    	long lastupdate = storeClient.executeQuery(query.getQueryString()).getJSONObject(0).getLong(timestamp.getQueryString().substring(1));
    	return lastupdate;
    }
    
    /**
     * updates weather station with the latest data
     * @param station_iri
     */
    void updateStation(String station_iri) {
		// get the coordinates of this station
		// build coordinate query
		SelectQuery query2 = Queries.SELECT();
		Variable coord = query2.var();
		query2.select(coord).where(iri(station_iri).has(hasCoordinates,coord)).prefix(p_station);
		
		// submit coordinate query
		String coordinates = storeClient.executeQuery(query2.getQueryString()).getJSONObject(0).getString(coord.getQueryString().substring(1));
		String[] latlon = coordinates.split("#");
		
		// the key for this map is the weather class, value is the corresponding value
		Map<Iri,Double> newWeatherData = WeatherAPIConnector.getWeatherDataFromOpenWeather(Double.parseDouble(latlon[0]), Double.parseDouble(latlon[1]));
		Iterator<Iri> weatherClasses = newWeatherData.keySet().iterator();
	    
		// to construct time series object
		List<String> datavalue_list = new ArrayList<>();
		List<List<?>> value_list = new ArrayList<>();
		
		// we need to find the data value for the corresponding weather class
		while (weatherClasses.hasNext()) {
			Iri weatherClass = weatherClasses.next();
			SelectQuery query3 = Queries.SELECT();
			Variable data = query3.var();
			Variable datavalue = query3.var();
			
			query3.select(datavalue).where(data.isA(weatherClass).andHas(hasValue,datavalue)).prefix(p_ontosensor,p_system);
			
			datavalue_list.add(storeClient.executeQuery(query3.getQueryString()).getJSONObject(0).getString(datavalue.getQueryString().substring(1)));
			double numericalValue = newWeatherData.get(weatherClass);
			value_list.add(Arrays.asList(numericalValue));
		}
		
		// append new values to time series table
		TimeSeries<Long> ts = new TimeSeries<Long>(Arrays.asList(Instant.now().getEpochSecond()), datavalue_list, value_list);
		tsClient.addTimeSeriesData(ts);
		
		// update last updated timestamp
		DerivationClient dClient = new DerivationClient(storeClient);
		dClient.updateTimestamp(station_iri);
    }

    /**
     * get latest weather data
     * @param station_iri
     */
	TimeSeries<Long> getLatestWeatherData(String station_iri) {
    	// first query the data IRIs
    	SelectQuery query = Queries.SELECT();
    	
    	Iri station = iri(station_iri);
    	Variable datavalue = query.var();
    	
    	Iri[] predicates = {hasSubsystem,observes,hasValue};
    	GraphPattern queryPattern = getQueryGraphPattern(query,predicates,station,datavalue);
    	
    	query.select(datavalue).where(queryPattern).prefix(p_station,p_system,p_ontosensor);
    	
    	List<String> datalist = storeClient.executeQuery(query.getQueryString()).toList().stream()
				.map(datavalueiri -> ((HashMap<String,String>) datavalueiri).get(datavalue.getQueryString().substring(1))).collect(Collectors.toList());
    	
    	long latestTime = tsClient.getMaxTime(datalist.get(0));
    	TimeSeries<Long> ts = tsClient.getTimeSeriesWithinBounds(datalist, latestTime, latestTime);
    	
    	return ts;
    }
    
	/**
	 * returns the historical weather data up to number of hours before current time
	 * @param station_iri
	 * @return
	 */
	TimeSeries<Long> getHistoricalWeatherData(String station_iri, int hour) {
		// first query the data IRIs
    	SelectQuery query = Queries.SELECT();
    	
    	Iri station = iri(station_iri);
    	Variable datavalue = query.var();
    	
    	Iri[] predicates = {hasSubsystem,observes,hasValue};
    	GraphPattern queryPattern = getQueryGraphPattern(query,predicates,station,datavalue);
    	
    	query.select(datavalue).where(queryPattern).prefix(p_station,p_system,p_ontosensor);
    	
    	List<String> datalist = storeClient.executeQuery(query.getQueryString()).toList().stream()
				.map(datavalueiri -> ((HashMap<String,String>) datavalueiri).get(datavalue.getQueryString().substring(1))).collect(Collectors.toList());
    	
    	long latestTime = tsClient.getMaxTime(datalist.get(0));
    	long queryEarliestTime = Instant.now().getEpochSecond() - hour * 3600;
    	TimeSeries<Long> ts = tsClient.getTimeSeriesWithinBounds(datalist, queryEarliestTime, latestTime);
    	
    	return ts;
	}

	/**
	 * Returns list of stations that are located within the given radius (in km)
	 * centre needs to be in the format "<lat>#<lon>", e.g. "50.1#1.0"
	 * query string is partially hardcoded (geospatialQueryTemplate)
	 * @param latlon
	 * @param radius (in km)
	 */
	List<String> getStationsInCircle(String centre, double radius) {
		String varKey = "station";
		Variable station = SparqlBuilder.var(varKey);
		
		// construct query triples using blazegraph's magic predicates
		GraphPattern queryPattern = GraphPatterns.and(station.has(p_geo.iri("search"), "inCircle")
				.andHas(p_geo.iri("searchDatatype"),lat_lon)
				.andHas(p_geo.iri("predicate"), hasCoordinates)
				.andHas(p_geo.iri("spatialCircleCenter"), centre)
				.andHas(p_geo.iri("spatialCircleRadius"), radius));
		
		TriplePattern matchRdfType = station.isA(WeatherStation);
		
		String query = String.format(geospatialQueryTemplate, ontostation, queryPattern.getQueryString() + matchRdfType.getQueryString());
		
		List<String> queryResult = storeClient.executeQuery(query).toList().stream()
				.map(station_iri -> ((HashMap<String,String>) station_iri).get(varKey))
				.collect(Collectors.toList());
		
		return queryResult;
	}
	
	/**
	 * Returns list of stations that are located within the given bounds
	 * both southwest and northeast need to be in the format "<lat>#<lon>", e.g. "50.1#1.0"
	 * query string is partially hardcoded (geospatialQueryTemplate)
	 * @param southwest
	 * @param northeast
	 */
	List<String> getStationsInRectangle(String southwest, String northeast) {
		String varKey = "station";
		Variable station = SparqlBuilder.var(varKey);
		
		// construct query triples using blazegraph's magic predicates
		GraphPattern queryPattern = GraphPatterns.and(station.has(p_geo.iri("search"), "inRectangle")
				.andHas(p_geo.iri("searchDatatype"),lat_lon)
				.andHas(p_geo.iri("predicate"), hasCoordinates)
				.andHas(p_geo.iri("spatialRectangleSouthWest"), southwest)
				.andHas(p_geo.iri("spatialRectangleNorthEast"), northeast));
		
		TriplePattern matchRdfType = station.isA(WeatherStation);
		
		String query = String.format(geospatialQueryTemplate, ontostation, queryPattern.getQueryString() + matchRdfType.getQueryString());
		
		List<String> queryResult = storeClient.executeQuery(query).toList().stream()
				.map(station_iri -> ((HashMap<String,String>) station_iri).get(varKey))
				.collect(Collectors.toList());
		
		return queryResult;
	}
	
    /**
     * constructs a query pattern given the list of predicates in between the given FirstNode and LastNode
     * @param Query
     * @param Predicates
     * @param FirstNode
     * @param LastNode
     * @return
     */
    GraphPattern getQueryGraphPattern (SelectQuery Query, Iri[] Predicates, Iri FirstNode, Variable LastNode) {
        GraphPattern CombinedGP = null;
    	
    	Variable[] Variables = new Variable[Predicates.length];
    	
    	// initialise intermediate nodes
    	for (int i=0; i < Variables.length-1; i++) {
    		Variables[i] = Query.var();
    	}
    	Variables[Variables.length-1] = LastNode;
    	
    	// first triple
    	GraphPattern firstTriple = FirstNode.has(Predicates[0],Variables[0]);
    	CombinedGP = GraphPatterns.and(firstTriple);
    	
    	// the remaining
    	for (int i=0; i < Variables.length-1; i++) {
    		GraphPattern triple = Variables[i].has(Predicates[i+1],Variables[i+1]);
    		CombinedGP.and(triple);
    	}
    	
    	return CombinedGP;
    }
}
