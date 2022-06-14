package uk.ac.cam.cares.jps.agent.flood;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.time.Duration;
import java.time.Instant;

import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.StringLiteral;
import org.json.JSONArray;
import org.json.JSONObject;

import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.agent.flood.objects.Station;
import uk.ac.cam.cares.jps.agent.flood.objects.Measure;
import uk.ac.cam.cares.jps.agent.flood.sparqlbuilder.ServicePattern;
import uk.ac.cam.cares.jps.agent.flood.sparqlbuilder.ValuesPattern;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * contains a collection of methods to query and update the KG
 * @author Kok Foong Lee
 *
 */
public class FloodSparql {
    private StoreClientInterface storeClient;
    
    // prefix
	private static String ontoems = "https://www.theworldavatar.com/kg/ontoems/";
    private static Prefix p_ems = SparqlBuilder.prefix("ems",iri(ontoems));
    private static Prefix p_geo = SparqlBuilder.prefix("geo",iri("http://www.bigdata.com/rdf/geospatial#"));
	private static Prefix p_om = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    
    // classes
	private static Iri ReportingStation = p_ems.iri("ReportingStation");

	// subclass of Quantity
	private static Iri WaterLevel = p_ems.iri("WaterLevel");
	private static Iri Rainfall = p_ems.iri("Rainfall");
	private static Iri WaterFlow = p_ems.iri("WaterFlow");
	private static Iri AirTemperature = p_ems.iri("AirTemperature");
	private static Iri WindSpeed = p_ems.iri("WindSpeed");
	private static Iri WindDirection = p_ems.iri("WindDirection");
	private static Iri WetBulbTemperature = p_ems.iri("WetBulbTemperature");
	
	private static Iri Measure = p_om.iri("Measure");

	//ranges
	private static Iri NormalRange = p_ems.iri("NormalRange");
	private static Iri HighRange = p_ems.iri("HighRange");
	private static Iri LowRange = p_ems.iri("LowRange");
	private static Iri UnavailableRange = p_ems.iri("UnavailableRange");

	// trends
	private static Iri Steady = p_ems.iri("Steady");
	private static Iri Rising = p_ems.iri("Rising");
	private static Iri Falling = p_ems.iri("Falling");
	private static Iri UnavailableTrend = p_ems.iri("UnavailableTrend");

    // properties
	private static Iri hasObservationLocation = p_ems.iri("hasObservationLocation");
	private static Iri hasObservationElevation = p_ems.iri("hasObservationElevation");
	private static Iri hasCurrentRange = p_ems.iri("hasCurrentRange");
	private static Iri hasCurrentTrend = p_ems.iri("hasCurrentTrend");
	private static Iri dataSource = p_ems.iri("dataSource");
	private static Iri hasDownstreamStation = p_ems.iri("hasDownstreamStation");
	private static Iri hasValue = p_om.iri("hasValue");
	private static Iri hasUnit = p_om.iri("hasUnit");
	private static Iri reports = p_ems.iri("reports");
    private static Iri measures = iri("http://environment.data.gov.uk/flood-monitoring/def/core/measures");
    private static Iri stationReference = iri("http://environment.data.gov.uk/flood-monitoring/def/core/stationReference");
    private static Iri lat_lon = iri("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon"); 
    
    private static Iri unitName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/unitName");
    private static Iri parameterName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/parameterName");
	private static Iri qualifier = iri("http://environment.data.gov.uk/flood-monitoring/def/core/qualifier");

	private static Iri lat_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
	private static Iri lon_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#long");

	private static Iri stageScale = iri("http://environment.data.gov.uk/flood-monitoring/def/core/stageScale");
	private static Iri downstageScale = iri("http://environment.data.gov.uk/flood-monitoring/def/core/downstageScale");
	private static Iri typicalRangeHigh = iri("http://environment.data.gov.uk/flood-monitoring/def/core/typicalRangeHigh");
	private static Iri typicalRangeLow = iri("http://environment.data.gov.uk/flood-monitoring/def/core/typicalRangeLow");

    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(FloodSparql.class);
    
    static Map<String, Iri> unitMap = new HashMap<String, Iri>() {
		{
			put("mAOD", iri("http://theworldavatar.com/resource/ontouom/metreAOD"));
			put("m", iri("http://theworldavatar.com/resource/ontouom/metreUnspecified"));
			put("mASD", iri("http://theworldavatar.com/resource/ontouom/metreASD"));
			put("mm", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/millimetre"));
			put("mBDAT", iri("http://theworldavatar.com/resource/ontouom/metreBDAT"));
			put("l/s", iri("http://theworldavatar.com/resource/ontouom/litrePerSecond"));
			put("m3/s", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/cubicMetrePerSecond-Time"));
			put("deg", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/degree"));
			put("m/s", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/PrefixedMetrePerSecond-Time"));
			put("Knots", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/knot-International"));
		}
	};

	public FloodSparql(StoreClientInterface storeClient) {
		this.storeClient = storeClient;
	}
	
	/**
	 * returns a list of stations before adding OntoEMS concepts
	 * it is assumed that there is only one RDF collection in the namespace
	 * a good illustration of how an RDF collection look like 
	 * http://www-kasm.nii.ac.jp/~koide/SWCLOS2/Manual/07RDFCollection.htm
	 * @return
	 */
	List<Station> getStationsOriginal() {
		SelectQuery query = Queries.SELECT();
		
		Variable station = query.var();
		Variable measure = query.var();
		Variable param = query.var();
		Variable qual = query.var();
		Variable unit = query.var();
		Variable lat = query.var();
		Variable lon = query.var();
		
		GraphPattern stationPattern = query.var().has(RDF.FIRST, station);
		GraphPattern stationPropertiesPattern = station.has(measures, measure).andHas(lat_prop,lat).andHas(lon_prop,lon);
		GraphPattern measurePropertiesPattern = measure.has(parameterName,param).andHas(qualifier,qual).andHas(unitName,unit);
		GraphPattern queryPattern = GraphPatterns.and(stationPattern, stationPropertiesPattern, measurePropertiesPattern);
		
		query.where(queryPattern).select(station,measure,param,qual,lat,lon,unit).distinct();
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		Map<String, Station> station_map = new HashMap<>(); // iri to station object map
		List<Station> stations = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
			String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
    		String measureName = queryResult.getJSONObject(i).getString(param.getQueryString().substring(1));
    		String subTypeName = queryResult.getJSONObject(i).getString(qual.getQueryString().substring(1));
			String unitName = queryResult.getJSONObject(i).getString(unit.getQueryString().substring(1));
			String latString = queryResult.getJSONObject(i).getString(lat.getQueryString().substring(1));
			String lonString = queryResult.getJSONObject(i).getString(lon.getQueryString().substring(1));
    		
    		Station stationObject;
    		if (station_map.containsKey(stationIri)) {
    			stationObject = station_map.get(stationIri);
    		} else {
    			stationObject = new Station(stationIri);
				stations.add(stationObject);
			}
			stationObject.setMeasureName(measureIri, measureName);
			stationObject.setMeasureSubTypeName(measureIri, subTypeName);
			stationObject.setMeasureUnit(measureIri, unitName);
			stationObject.setLat(Double.parseDouble(latString));
			stationObject.setLon(Double.parseDouble(lonString));
		}
	    return stations;
	}
	
	/**
	 * add triples related to OntoEMS
	 * @param stations
	 */
	void addStationTypeAndCoordinates(List<Station> stations) {
		ModifyQuery modify = Queries.MODIFY();
		
		for (Station station : stations) {
			modify.insert(iri(station.getIri()).isA(ReportingStation));
			modify.insert(iri(station.getIri()).has(dataSource, "Environment Agency Real Time flood-monitoring"));

			// blazegraph custom literal
			String latlon = String.valueOf(station.getLat()) + "#" + String.valueOf(station.getLon());
			StringLiteral coordinatesLiteral = Rdf.literalOfType(latlon, lat_lon);
			modify.insert(iri(station.getIri()).has(hasObservationLocation,coordinatesLiteral));
		}
		
		modify.prefix(p_ems);
		
		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * add an additional rdf:type for stations reporting water level
	 * @param stations
	 */
	void addWaterLevelReportingStationType(List<Station> stations) {
		ModifyQuery modify = Queries.MODIFY();
	}
	
	/**
	 * replace original triple <station> <measures> <measure> with
	 * <station> <reports> <quantity>, <quantity> <hasValue> <measure>
	 * also adds other OntoEMS concepts such as range, trend
	 */
    void replaceMeasures(List<Station> stations) {
		// delete all <station> <measures> <measure> triple
		ModifyQuery modify = Queries.MODIFY();
		Variable stationvar = SparqlBuilder.var("station");
		Variable measurevar = SparqlBuilder.var("measure");
		modify.delete(stationvar.has(measures,measurevar)).where(stationvar.has(measures,measurevar));

		// add the new ontoEMS triples
		for (Station station : stations) {
			for (String measure : station.getMeasures()) {
				Iri quantityIri = null;
				String paramName = station.getMeasureName(measure);
				String qual = station.getMeasureSubTypeName(measure);
				// determine class of quantity
				switch (paramName) {
					case "Water Level":
						quantityIri = iri(station.getIri() + "/WaterLevel");
						modify.insert(quantityIri.isA(WaterLevel));
						break;
					case "Flow":
						quantityIri = iri(station.getIri() + "/Flow");
						modify.insert(quantityIri.isA(WaterFlow));
						break;
					case "Rainfall":
						quantityIri = iri(station.getIri() + "/Rainfall");
						modify.insert(quantityIri.isA(Rainfall));
						break;
					case "Wind":
						if (qual.contentEquals("Direction")) {
							quantityIri = iri(station.getIri() + "/WindDirection");
							modify.insert(quantityIri.isA(WindDirection));
						} else if (qual.contentEquals("Speed")) {
							quantityIri = iri(station.getIri() + "/WindSpeed");
							modify.insert(quantityIri.isA(WindSpeed));
						}
						break;
					case "Temperature":
						if (qual.contentEquals("Wet Bulb")) {
							quantityIri = iri(station.getIri() + "/WetBulbTemperature");
							modify.insert(quantityIri.isA(WetBulbTemperature));
						} else {
							quantityIri = iri(station.getIri() + "/Temperature");
							modify.insert(quantityIri.isA(AirTemperature));
						}
						break;
				}
				modify.insert(iri(station.getIri()).has(reports, quantityIri));
				modify.insert(quantityIri.has(hasValue, iri(measure)));
				modify.insert(iri(measure).isA(Measure));

				// unit
				if (unitMap.containsKey(station.getMeasureUnit(measure))) {
					modify.insert(iri(measure).has(hasUnit, unitMap.get(station.getMeasureUnit(measure))));
				}

				// add dummy triple for range so that sparql update will work
				modify.insert(iri(measure).has(hasCurrentRange, UnavailableRange));
				modify.insert(iri(measure).has(hasCurrentTrend, UnavailableTrend));
			}
		}
		modify.prefix(p_ems,p_om);
		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * returns all the measures in the endpoint, each station may measure 1-4 quantities
	 * ignores stations without coordinates
	 * @return
	 */
	List<String> getMeasures() {
        SelectQuery query = Queries.SELECT();
		
		Variable measure = query.var();
		Variable station = query.var();
		Variable coord = query.var();
				
		GraphPattern queryPattern = station.has(hasObservationLocation, coord)
		.andHas(PropertyPaths.path(reports,hasValue), measure); 
		
		query.select(measure).where(queryPattern).prefix(p_ems,p_om);
		
	    @SuppressWarnings("unchecked")
		List<String> measure_iri_list = storeClient.executeQuery(query.getQueryString()).toList().stream()
	    .map(datairi -> ((HashMap<String,String>) datairi).get(measure.getQueryString().substring(1))).collect(Collectors.toList());
	    
	    return measure_iri_list;
	}
	
	/**
	 * similar function as above but only query for measures for the given stations
	 * @param stations
	 * @return
	 */
	Map<String, List<String>> getMeasures(Map<String, Station> stations) {
		SelectQuery query = Queries.SELECT();
		
		Variable measure = query.var();
		Variable station = query.var();
				
		GraphPattern queryPattern = station.has(PropertyPaths.path(reports,hasValue), measure);
		List<String> stationIri_list = new ArrayList<>(stations.keySet());
		ValuesPattern stationPattern = new ValuesPattern(station, stationIri_list.stream().map(s -> iri(s)).collect(Collectors.toList()));
		
		query.select(measure,station).where(queryPattern, stationPattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		Map<String, List<String>> station_measure_map = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
			String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
			
			if (station_measure_map.containsKey(stationIri)) {
				station_measure_map.get(stationIri).add(measureIri);
			} else {
				List<String> newMeasureList = new ArrayList<>();
				newMeasureList.add(measureIri);
				station_measure_map.put(stationIri, newMeasureList);
			}
		}
	    
	    return station_measure_map;
	}
	
	/**
	 * add a measure that was not present in the initial RDF file, but present
	 * in the data downloaded later
	 * adds a triple <station> <measures> <measure>
	 * @param station
	 * @param measure
	 */
	void addMeasureToStation(String station, String measure, String unit,
			String paramName, String qual) {

		ModifyQuery modify = Queries.MODIFY();

		Iri quantityIri = null;
		// determine class of quantity
		switch (paramName) {
			case "Water Level":
			    quantityIri = iri(station + "/WaterLevel");
				modify.insert(quantityIri.isA(WaterLevel));
				break;
			case "Flow":
			    quantityIri = iri(station + "/Flow");
				modify.insert(quantityIri.isA(WaterFlow));
				break;
			case "Rainfall":
			    quantityIri = iri(station + "/Rainfall");
				modify.insert(quantityIri.isA(Rainfall));
				break;
			case "Wind":
				if (qual.contentEquals("Direction")) {
					quantityIri = iri(station + "/WindDirection");
					modify.insert(quantityIri.isA(WindDirection));
				} else if (qual.contentEquals("Speed")) {
					quantityIri = iri(station + "/WindSpeed");
					modify.insert(quantityIri.isA(WindSpeed));
				}
				break;
			case "Temperature":
			    if (qual.contentEquals("Wet Bulb")) {
					quantityIri = iri(station + "/WetBulbTemperature");
					modify.insert(quantityIri.isA(WetBulbTemperature));
				} else {
					quantityIri = iri(station + "/Temperature");
					modify.insert(quantityIri.isA(AirTemperature));
				}
				break;
		}

		modify.insert(iri(station).has(reports,quantityIri));

		modify.insert(quantityIri.has(hasValue, iri(measure)));
		modify.insert(iri(measure).has(unitName, unit)
				.andHas(parameterName, paramName)
				.andHas(qualifier,qual)
				.andIsA(Measure));

		// add ems unit 
		if (unitMap.containsKey(unit)) {
			modify.insert(iri(measure).has(hasUnit, unitMap.get(unit)));
		}

		// dummy range triple to modified in sparql update
		modify.insert(iri(measure).has(hasCurrentRange, UnavailableRange));

		modify.prefix(p_ems,p_om);
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/** 
	 * performs a very simple check on whether stations are already initialised
	 * with time series
	 * @return
	 */
	boolean areStationsInitialised() {
		SelectQuery query = Queries.SELECT();
		Variable station = query.var();
		
		GraphPattern queryPattern = station.isA(ReportingStation);
		
		query.prefix(p_ems).where(queryPattern).limit(10);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() >= 1) {
			return true;
		} else {
			return false;
		}
	}

	LocalDate getLatestUpdate(TimeSeriesClient<Instant> tsClient) {
		TimeSeries<Instant> ts = tsClient.getLatestData(Config.TIME_IRI);
		try {
			return LocalDate.parse(ts.getValuesAsString(Config.TIME_IRI).get(0));
		} catch (IndexOutOfBoundsException e) {
			LOGGER.error(e.getMessage());
			throw new RuntimeException(e);
		}
	}
	
	/**
	 * returns true if given date exists
	 * @param date
	 * @return
	 */
	boolean checkUpdateDateExists(TimeSeriesClient<Instant> tsClient, LocalDate date) {
		TimeSeries<Instant> ts = tsClient.getLatestData(Config.TIME_IRI);
		if (ts.getValues(Config.TIME_IRI).size() > 0) {
			LocalDate latestDate = LocalDate.parse(ts.getValuesAsString(Config.TIME_IRI).get(0));
			if(date.equals(latestDate)) {
				return true;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}
	
	/**
	 * returns a map of station iri to station object
	 */
	Map<String, Station> getStationsWithCoordinates(String southwest, String northeast) {
		Iri river_prop = iri("http://environment.data.gov.uk/flood-monitoring/def/core/riverName");
		Iri catchment_prop = iri("http://environment.data.gov.uk/flood-monitoring/def/core/catchmentName");
		Iri town_prop = iri("http://environment.data.gov.uk/flood-monitoring/def/core/town");
		Iri dateOpen_prop = iri("http://environment.data.gov.uk/flood-monitoring/def/core/dateOpened");
		
		SelectQuery query = Queries.SELECT();
		
		// station properties
		Variable lat = query.var();
		Variable lon = query.var();
		Variable station = query.var();
		Variable ref = query.var();
		Variable river = query.var();
		Variable catchment = query.var();
		Variable town = query.var();
		Variable dateOpened = query.var();
		Variable label = query.var();
		
		// measure properties
		Variable measure = query.var();
		// e.g. table name: Water Level (Tidal Level), param = Water Level,
    	// qual (param subtype) = Tidal Level
    	Variable param = query.var();
    	Variable qual = query.var();
    	Variable unit = query.var();
		
		GraphPattern queryPattern = GraphPatterns.and(station.has(lat_prop,lat)
				.andHas(lon_prop,lon).andHas(stationReference,ref).andHas(PropertyPaths.path(reports,hasValue), measure));
		
		GraphPattern stationProperties = GraphPatterns.and(station.has(iri(RDFS.LABEL), label).optional(),
				station.has(river_prop, river).optional(),
				station.has(catchment_prop, catchment).optional(),
				station.has(town_prop, town).optional(),
				station.has(dateOpen_prop, dateOpened).optional());
		
		GraphPattern measurePropertiesPattern = measure.has(parameterName,param).andHas(qualifier,qual).andHas(unitName, unit);
		
		// restrict query location
		if (southwest != null && northeast != null) {
			GraphPattern coordinatesPattern = GraphPatterns.and(station.has(p_geo.iri("search"), "inRectangle")
					.andHas(p_geo.iri("searchDatatype"),lat_lon)
					.andHas(p_geo.iri("predicate"), hasObservationLocation)
					.andHas(p_geo.iri("spatialRectangleSouthWest"), southwest)
					.andHas(p_geo.iri("spatialRectangleNorthEast"), northeast));

	    	GraphPattern geoPattern = new ServicePattern(p_geo.iri("search").getQueryString()).service(coordinatesPattern);
	    	query.where(queryPattern,geoPattern,stationProperties,measurePropertiesPattern).prefix(p_geo,p_ems,p_om);
		} else {
			query.where(queryPattern,stationProperties,measurePropertiesPattern).prefix(p_ems,p_om);
		}
		
		query.select(station,lat,lon,ref,river,catchment,town,dateOpened,label,measure,param,qual,unit);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		int visid = 0;
		Map<String, Station> station_map = new HashMap<>(); // iri to station object map
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
			String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
    		String measureName = queryResult.getJSONObject(i).getString(param.getQueryString().substring(1));
    		String subTypeName = queryResult.getJSONObject(i).getString(qual.getQueryString().substring(1));
    		String unitName = queryResult.getJSONObject(i).getString(unit.getQueryString().substring(1));
    		
    		Station stationObject;
    		if (station_map.containsKey(stationIri)) {
    			stationObject = station_map.get(stationIri);
    		} else {
    			stationObject = new Station(stationIri);
    			station_map.put(stationIri, stationObject);
    			
    			// station properties are unique, only need to set once
    			stationObject.setIdentifier(queryResult.getJSONObject(i).getString(ref.getQueryString().substring(1)));
    			stationObject.setLat(queryResult.getJSONObject(i).getDouble(lat.getQueryString().substring(1)));
    			stationObject.setLon(queryResult.getJSONObject(i).getDouble(lon.getQueryString().substring(1)));
				visid += 1;
    			stationObject.setVisId(visid);
    			
    			// optional station properties
    			if (queryResult.getJSONObject(i).has(river.getQueryString().substring(1))) {
    				stationObject.setRiver(queryResult.getJSONObject(i).getString(river.getQueryString().substring(1)));
    			}
    			if (queryResult.getJSONObject(i).has(catchment.getQueryString().substring(1))) {
    				stationObject.setCatchment(queryResult.getJSONObject(i).getString(catchment.getQueryString().substring(1)));
    			}
    			if (queryResult.getJSONObject(i).has(town.getQueryString().substring(1))) {
    				stationObject.setTown(queryResult.getJSONObject(i).getString(town.getQueryString().substring(1)));
    			}
    			if (queryResult.getJSONObject(i).has(dateOpened.getQueryString().substring(1))) {
    				stationObject.setDateOpened(queryResult.getJSONObject(i).getString(dateOpened.getQueryString().substring(1)));
    			}
    			if (queryResult.getJSONObject(i).has(label.getQueryString().substring(1))) {
    				stationObject.setLabel(queryResult.getJSONObject(i).getString(label.getQueryString().substring(1)));
    			}
    		}
			
			// measure properties
			// stations may measure more than 1 properties
			stationObject.setMeasureName(measureIri, measureName);
    		stationObject.setMeasureSubTypeName(measureIri, subTypeName);
    		stationObject.setMeasureUnit(measureIri, unitName);
		}
				
		return station_map;
	}
    
    boolean checkStationExists(String station) {
    	SelectQuery query = Queries.SELECT();
    	
    	GraphPattern queryPattern = iri(station).has(query.var(),query.var());
    	
    	query.where(queryPattern);
    	
	    if(storeClient.executeQuery(query.getQueryString()).length() > 1) {
	    	return true;
	    } else {
	    	return false;
	    }
    }

	/**
	 * datum is not within the original dataset, steps in this function
	 * 1) query stageScale for each station
	 * 2) download stageScale
	 */
	JSONObject downloadDatum(List<Station> stations) {
		List<Iri> station_iris = stations.stream().map(s -> iri(s.getIri())).collect(Collectors.toList());
		
		// query stagescale URL for each station
		SelectQuery query = Queries.SELECT();
		Variable stationvar = query.var();
		Variable stageScaleVar = query.var();
		ValuesPattern valuesPattern = new ValuesPattern(stationvar, station_iris);
		GraphPattern queryPattern = stationvar.has(stageScale, stageScaleVar);

		query.select(stationvar,stageScaleVar).where(valuesPattern, queryPattern).distinct();

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		JSONObject datumMap = new JSONObject();
		// download stageScale and append triple for each station
		for (int i = 0; i < queryResult.length(); i++) {
			String station_iri = queryResult.getJSONObject(i).getString(stationvar.getQueryString().substring(1));
			String stageScale_iri = queryResult.getJSONObject(i).getString(stageScaleVar.getQueryString().substring(1));

			APIConnector api = new APIConnector(stageScale_iri);
			try {
				HttpEntity result = api.getData();
				JSONObject result_jo = new JSONObject(EntityUtils.toString(result));
				double datum = result_jo.getJSONObject("items").getDouble("datum");

				datumMap.put(station_iri, datum);
			} catch (IOException | URISyntaxException e) {
				LOGGER.warn("Download from " + stageScale_iri + " failed");
			}
		}
	
		return datumMap;
	}

	void addDatum(JSONObject datum_json) {
		ModifyQuery modify = Queries.MODIFY();

		for (String station : datum_json.keySet()) {
			double datum = datum_json.getDouble(station);
			modify.insert(iri(station).has(hasObservationElevation, datum));
		}

		modify.prefix(p_ems);
		storeClient.executeUpdate(modify.getQueryString());
	}

	void postToRemoteStore(HttpEntity entity) throws ClientProtocolException, IOException {
		// use Blazegraph's REST API to upload RDF data to a SPARQL endpoint
		LOGGER.info("Posting data to " + storeClient.getUpdateEndpoint());
	        
		// tried a few methods to add credentials, this seems to be the only way that works
		// i.e. setting it manually in the header
		String auth = storeClient.getUser() + ":" + storeClient.getPassword();
		String encoded_auth = Base64.getEncoder().encodeToString(auth.getBytes()); 
		HttpPost postRequest = new HttpPost(storeClient.getUpdateEndpoint());
		postRequest.setHeader(HttpHeaders.AUTHORIZATION, "Basic " + encoded_auth);
		
		// add contents downloaded from the API to the post request 
		postRequest.setEntity(entity);
		// then send the post request
		CloseableHttpClient httpclient = HttpClients.createDefault();
		httpclient.execute(postRequest);
	}

	/**
	 * query ranges from <station> <stageScale> <stageScale>
	 * @param tsClient
	 * @param measureIRIs
	 */
	List<Measure> addRangeForStageScale(TimeSeriesClient<Instant> tsClient, List<String> measureIRIs) {
		ModifyQuery modify = Queries.MODIFY(); // to store triples to add at the end
		
		// match meaures with qualifier = Stage
		// first query the upper and lower bounds for each data
		SelectQuery query = Queries.SELECT();
		Variable station = query.var();
		Variable upperBoundVar = query.var();
		Variable lowerBoundVar = query.var();
		Variable measure = query.var();

		Variable oldrange = query.var(); // used in update query only

		GraphPattern gp1 = station.has(PropertyPaths.path(reports,hasValue), measure)
		.andHas(PropertyPaths.path(stageScale, typicalRangeLow), lowerBoundVar)
		.andHas(PropertyPaths.path(stageScale, typicalRangeHigh), upperBoundVar);

		GraphPattern measureGp = measure.has(qualifier, "Stage");

		ValuesPattern vp = new ValuesPattern(measure, measureIRIs.stream().map(s -> iri(s)).collect(Collectors.toList()));

		query.select(measure,station,upperBoundVar,lowerBoundVar).where(gp1,vp,measureGp).prefix(p_om,p_ems);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		List<String> measureRangesToDelete = new ArrayList<>();
		List<Measure> measureObjectList = new ArrayList<>();

		for (int i = 0; i < queryResult.length(); i++) {
			String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
			double upperbound = queryResult.getJSONObject(i).getDouble(upperBoundVar.getQueryString().substring(1));
			double lowerbound = queryResult.getJSONObject(i).getDouble(lowerBoundVar.getQueryString().substring(1));

			Measure measureObject = new Measure(measureIri);
			measureObject.setTypicalRangeHigh(upperbound);
			measureObject.setTypicalRangeLow(lowerbound);
			measureObjectList.add(measureObject);
			// query latest value
			TimeSeries<Instant> ts = tsClient.getLatestData(measureIri);
			List<Double> values = ts.getValuesAsDouble(measureIri);
			if (values.size() > 0) {
				double latestValue = values.get(values.size()-1);
				measureRangesToDelete.add(measureIri);

				// determine range
				if (latestValue < lowerbound) {
					modify.insert(iri(measureIri).has(hasCurrentRange, LowRange));
				} else if (latestValue > upperbound) {
					modify.insert(iri(measureIri).has(hasCurrentRange, HighRange));
				} else {
					modify.insert(iri(measureIri).has(hasCurrentRange, NormalRange));
				}
			}
		}

		// delete old ranges
		vp = new ValuesPattern(measure, measureRangesToDelete.stream().map(s -> iri(s)).collect(Collectors.toList()));
		modify.delete(measure.has(hasCurrentRange, oldrange)).where(measure.has(hasCurrentRange, oldrange), vp);

		modify.prefix(p_ems);
		storeClient.executeUpdate(modify.getQueryString());
		return measureObjectList;
	}

	/**
	 * similar to above but for downstage
	 * @param tsClient
	 * @param measureIRIs
	 */
	List<Measure> addRangeForDownstageScale(TimeSeriesClient<Instant> tsClient, List<String> measureIRIs) {
		ModifyQuery modify = Queries.MODIFY();

		// match meaures with qualifier = Downstream Stage
		// first query the upper and lower bounds for each data
		SelectQuery query = Queries.SELECT();
		Variable station = query.var();
		Variable upperBoundVar = query.var();
		Variable lowerBoundVar = query.var();
		Variable measure = query.var();

		Variable oldrange = query.var(); // used in update query

		GraphPattern gp1 = station.has(PropertyPaths.path(reports,hasValue), measure)
		.andHas(PropertyPaths.path(downstageScale, typicalRangeLow), lowerBoundVar)
		.andHas(PropertyPaths.path(downstageScale, typicalRangeHigh), upperBoundVar);

		GraphPattern measureGp = measure.has(qualifier, "Downstream Stage");

		ValuesPattern vp = new ValuesPattern(measure, measureIRIs.stream().map(s -> iri(s)).collect(Collectors.toList()));

		query.select(measure,station,upperBoundVar,lowerBoundVar).where(gp1,vp,measureGp).prefix(p_om,p_ems);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		List<String> measureRangesToDelete = new ArrayList<>();

		// to return
		List<Measure> measureObjectList = new ArrayList<>();

		for (int i = 0; i < queryResult.length(); i++) {
			String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
			double upperbound = queryResult.getJSONObject(i).getDouble(upperBoundVar.getQueryString().substring(1));
			double lowerbound = queryResult.getJSONObject(i).getDouble(lowerBoundVar.getQueryString().substring(1));

			// add to list and return later
			Measure measureObject = new Measure(measureIri);
			measureObject.setTypicalRangeHigh(upperbound);
			measureObject.setTypicalRangeLow(lowerbound);
			measureObjectList.add(measureObject);

			// query latest value
			TimeSeries<Instant> ts = tsClient.getLatestData(measureIri);
			List<Double> values = ts.getValuesAsDouble(measureIri);
			if (values.size() > 0) {
				double latestValue = values.get(values.size()-1);
				measureRangesToDelete.add(measureIri);

				// determine range
				if (latestValue < lowerbound) {
					modify.insert(iri(measureIri).has(hasCurrentRange, LowRange));
				} else if (latestValue > upperbound) {
					modify.insert(iri(measureIri).has(hasCurrentRange, HighRange));
				} else {
					modify.insert(iri(measureIri).has(hasCurrentRange, NormalRange));
				}
			}
		}
		// delete old ranges
		vp = new ValuesPattern(measure, measureRangesToDelete.stream().map(s -> iri(s)).collect(Collectors.toList()));
		modify.delete(measure.has(hasCurrentRange, oldrange)).where(measure.has(hasCurrentRange, oldrange), vp);

		modify.prefix(p_ems);
		storeClient.executeUpdate(modify.getQueryString());

		return measureObjectList;
	}

	void addConnections(File connectionsFile) {
		// this will be used to query the IRIs of the stations
		List<Integer> RLOIidList = new ArrayList<>();
		Map<Integer,Integer> upstreamToDownstreamMap = new HashMap<>();

		try (Reader reader = Files.newBufferedReader(connectionsFile.toPath())) {
			CSVReader csvReader = new CSVReader(reader);
			List<String[]> readAll = csvReader.readAll();

			// skip header
			for (int i = 1; i < readAll.size(); i++) {
				if (!readAll.get(i)[4].isEmpty()){
					int upstream = Integer.parseInt(readAll.get(i)[1]);
					int downstream = Integer.parseInt(readAll.get(i)[4]);
					
					if (upstreamToDownstreamMap.containsKey(upstream)) {
						LOGGER.warn("Duplicate connection?");
					}
					upstreamToDownstreamMap.put(upstream, downstream);

					if (!RLOIidList.contains(upstream)) {
						RLOIidList.add(upstream);
					}

					if (!RLOIidList.contains(downstream)) {
						RLOIidList.add(downstream);
					}
				}
			}
			csvReader.close();
		} catch (IOException e) {
			LOGGER.error("Possible error reading csv file");
			LOGGER.error(e.getMessage());
		}

		Map<Integer, String> RLOIidToIRIMap = new HashMap<>();
		if (RLOIidList.size() > 0) {
			// now query the station IRIs
			SelectQuery query = Queries.SELECT();
			Iri RLOIid = iri("http://environment.data.gov.uk/flood-monitoring/def/core/RLOIid");
			Variable station = query.var();
			Variable idVar = query.var();

			ValuesPattern valuesPattern = new ValuesPattern(idVar, RLOIidList);
			GraphPattern queryPattern = station.has(RLOIid, idVar);

			query.select(station,idVar).where(valuesPattern, queryPattern);

			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

			for (int i = 0; i < queryResult.length(); i++) {
				String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
				String id = queryResult.getJSONObject(i).getString(idVar.getQueryString().substring(1));
				RLOIidToIRIMap.put(Integer.parseInt(id), stationIri);
			}

			// now that we have the station IRIs, we can add the triples for each connection
			ModifyQuery modify = Queries.MODIFY();
			Iterator<Integer> upstreamID_iterator = upstreamToDownstreamMap.keySet().iterator();

			while (upstreamID_iterator.hasNext()) {
				int upstreamID = upstreamID_iterator.next();
				int downstreamID = upstreamToDownstreamMap.get(upstreamID);

				String upstreamIRI = RLOIidToIRIMap.get(upstreamID);
				String downstreamIRI = RLOIidToIRIMap.get(downstreamID);

				if (upstreamIRI != null && downstreamIRI != null) {
					modify.insert(iri(upstreamIRI).has(hasDownstreamStation, iri(downstreamIRI)));
				} else {
					LOGGER.warn("Null IRI detected, upstreamID and downstreamID = " + upstreamID + " " + downstreamID);
				}
				
			}
			modify.prefix(p_ems);
			storeClient.executeUpdate(modify.getQueryString());
		}
	}

	/**
	 * queries time series over the last 12 hours
	 * uses the difference between final and first value
	 * if the change is greater than 10% based on the typical range, it is marked as either rising or falling
	 */
	void addTrends(TimeSeriesClient<Instant> tsClient, List<Measure> measures, Instant lowerbound, Instant upperbound) {
		ModifyQuery modify = Queries.MODIFY();

		List<String> oldMeasureTrendToDelete = new ArrayList<>();
		for (Measure measure : measures) {
			TimeSeries<Instant> ts = tsClient.getTimeSeriesWithinBounds(Arrays.asList(measure.getIri()), lowerbound, upperbound);
			List<Double> values = ts.getValuesAsDouble(measure.getIri());

			if (values.size() > 0) {
				double fraction_difference = (values.get(values.size()-1) - values.get(0)) / (measure.getRangeHigh() - measure.getRangeLow());
				
				if (fraction_difference > 0.1) {
					modify.insert(iri(measure.getIri()).has(hasCurrentTrend, Rising));
				} else if (fraction_difference < -0.1) {
					modify.insert(iri(measure.getIri()).has(hasCurrentTrend, Falling));
				} else {
					modify.insert(iri(measure.getIri()).has(hasCurrentTrend, Steady));
				}

				oldMeasureTrendToDelete.add(measure.getIri());
			}
		}

		// delete old trends
		Variable oldtrend = SparqlBuilder.var("oldtrend");
		Variable measureVar = SparqlBuilder.var("measure");

		ValuesPattern vp = new ValuesPattern(measureVar, oldMeasureTrendToDelete.stream().map(s -> iri(s)).collect(Collectors.toList()));

		modify.delete(measureVar.has(hasCurrentTrend, oldtrend)).where(measureVar.has(hasCurrentTrend, oldtrend), vp);
		modify.prefix(p_ems);
		storeClient.executeUpdate(modify.getQueryString());
	}
}
