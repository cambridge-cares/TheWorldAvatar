package uk.ac.cam.cares.jps.agent.flood;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.sql.Connection;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.time.Instant;

import org.apache.http.HttpEntity;
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
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.agent.flood.objects.Station;
import uk.ac.cam.cares.jps.agent.flood.objects.StationConnection;
import uk.ac.cam.cares.jps.agent.flood.objects.Measure;
import uk.ac.cam.cares.jps.agent.flood.sparqlbuilder.ValuesPattern;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * contains a collection of methods to query and update the KG
 * @author Kok Foong Lee
 *
 */
public class FloodSparql {
    private RemoteStoreClient storeClient;
    
    // prefix
	private static final String ONTOEMS = "https://www.theworldavatar.com/kg/ontoems/";
    private static final Prefix P_EMS = SparqlBuilder.prefix("ems",iri(ONTOEMS));
	private static final Prefix P_OM = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    
    // classes
	private static final Iri REPORTING_STATION = P_EMS.iri("ReportingStation");
	private static final Iri WATER_LEVEL_REPORTING_STATION = P_EMS.iri("WaterLevelReportingStation");

	// subclass of Quantity
	private static final String RAINFALL_STRING = "Rainfall";
	private static final Iri WATER_LEVEL = P_EMS.iri("WaterLevel");
	private static final Iri RAINFALL = P_EMS.iri(RAINFALL_STRING);
	private static final Iri WATER_FLOW = P_EMS.iri("WaterFlow");
	private static final Iri AIR_TEMPERATURE = P_EMS.iri("AirTemperature");
	private static final Iri WIND_SPEED = P_EMS.iri("WindSpeed");
	private static final Iri WIND_DIRECTION = P_EMS.iri("WindDirection");
	private static final Iri WET_BULB_TEMPERATURE = P_EMS.iri("WetBulbTemperature");
	
	private static final Iri MEASURE = P_OM.iri("Measure");

	//ranges
	private static final Iri NORMAL_RANGE = P_EMS.iri("NormalRange");
	private static final Iri HIGH_RANGE = P_EMS.iri("HighRange");
	private static final Iri LOW_RANGE = P_EMS.iri("LowRange");
	private static final Iri UNAVAILABLE_RANGE = P_EMS.iri("UnavailableRange");

	// trends
	private static final Iri STEADY = P_EMS.iri("Steady");
	private static final Iri RISING = P_EMS.iri("Rising");
	private static final Iri FALLING = P_EMS.iri("Falling");
	private static final Iri UNAVAILABLE_TREND = P_EMS.iri("UnavailableTrend");

    // properties
	private static final Iri HAS_OBSERVATION_ELEVATION = P_EMS.iri("hasObservationElevation");
	private static final Iri HAS_CURRENT_RANGE = P_EMS.iri("hasCurrentRange");
	private static final Iri HAS_CURRENT_TREND = P_EMS.iri("hasCurrentTrend");
	private static final Iri DATA_SOURCE = P_EMS.iri("dataSource");
	private static final Iri HAS_DOWNSTREAM_STATION = P_EMS.iri("hasDownstreamStation");
	private static final Iri HAS_VALUE = P_OM.iri("hasValue");
	private static final Iri HAS_UNIT = P_OM.iri("hasUnit");
	private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri MEASURES = iri("http://environment.data.gov.uk/flood-monitoring/def/core/measures");
    private static final Iri STATION_REFERENCE = iri("http://environment.data.gov.uk/flood-monitoring/def/core/stationReference");
    
    private static final Iri UNIT_NAME = iri("http://environment.data.gov.uk/flood-monitoring/def/core/unitName");
    private static final Iri PARAMETER_NAME = iri("http://environment.data.gov.uk/flood-monitoring/def/core/parameterName");
	private static final Iri QUALIFIER = iri("http://environment.data.gov.uk/flood-monitoring/def/core/qualifier");

	private static final Iri LAT = iri("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
	private static final Iri LON = iri("http://www.w3.org/2003/01/geo/wgs84_pos#long");

	private static final Iri STAGE_SCALE = iri("http://environment.data.gov.uk/flood-monitoring/def/core/stageScale");
	private static final Iri DOWNSTAGE_SCALE = iri("http://environment.data.gov.uk/flood-monitoring/def/core/downstageScale");
	private static final Iri TYPICAL_RANGE_HIGH = iri("http://environment.data.gov.uk/flood-monitoring/def/core/typicalRangeHigh");
	private static final Iri TYPICAL_RANGE_LOW = iri("http://environment.data.gov.uk/flood-monitoring/def/core/typicalRangeLow");

	private static final String WATER_LEVEL_STRING = "Water Level";
	private static final String DOWNSTREAM_STAGE = "Downstream Stage";
	private static final String STAGE = "Stage";
    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(FloodSparql.class);
    
    static Map<String, Iri> unitMap = new HashMap<>();
	static {
		unitMap.put("mAOD", iri("https://www.theworldavatar.com/kg/ontouom/metreAOD"));
		unitMap.put("m", iri("https://www.theworldavatar.com/kg/ontouom/metreUnspecified"));
		unitMap.put("mASD", iri("https://www.theworldavatar.com/kg/ontouom/metreASD"));
		unitMap.put("mm", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/millimetre"));
		unitMap.put("mBDAT", iri("https://www.theworldavatar.com/kg/ontouom/metreBDAT"));
		unitMap.put("l/s", iri("https://www.theworldavatar.com/kg/ontouom/litrePerSecond"));
		unitMap.put("m3/s", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/cubicMetrePerSecond-Time"));
		unitMap.put("deg", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/degree"));
		unitMap.put("m/s", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/PrefixedMetrePerSecond-Time"));
		unitMap.put("Knots", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/knot-International"));
	}

	public FloodSparql(RemoteStoreClient storeClient) {
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
		Variable label = query.var();
		Variable ref = query.var();
		Variable measureVar = query.var();
		Variable param = query.var();
		Variable qual = query.var();
		Variable unit = query.var();
		Variable lat = query.var();
		Variable lon = query.var();
		Variable stageLowerBoundVar = query.var();
		Variable stageUpperBoundVar = query.var();
		Variable downstageLowerBoundVar = query.var();
		Variable downstageUpperBoundVar = query.var();
		
		GraphPattern stationPattern = query.var().has(RDF.FIRST, station);
		GraphPattern stationPropertiesPattern = station.has(MEASURES, measureVar).andHas(LAT,lat).andHas(LON,lon).andHas(STATION_REFERENCE,ref).andHas(iri(RDFS.LABEL),label);
		GraphPattern measurePropertiesPattern = measureVar.has(PARAMETER_NAME,param).andHas(QUALIFIER,qual).andHas(UNIT_NAME,unit);
		GraphPattern stageScalePattern = station.has(PropertyPaths.path(STAGE_SCALE, TYPICAL_RANGE_LOW), stageLowerBoundVar)
			.andHas(PropertyPaths.path(STAGE_SCALE, TYPICAL_RANGE_HIGH), stageUpperBoundVar).optional();
		GraphPattern downStageScalePattern = station.has(PropertyPaths.path(DOWNSTAGE_SCALE, TYPICAL_RANGE_LOW), downstageLowerBoundVar)
		.andHas(PropertyPaths.path(STAGE_SCALE, TYPICAL_RANGE_HIGH), downstageUpperBoundVar).optional();
		GraphPattern queryPattern = GraphPatterns.and(stationPattern, stationPropertiesPattern, measurePropertiesPattern,stageScalePattern,downStageScalePattern);

		query.where(queryPattern).distinct();
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		Map<String, Station> stationMap = new HashMap<>(); // iri to station object map
		List<Station> stations = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
			String measureIri = queryResult.getJSONObject(i).getString(measureVar.getQueryString().substring(1));
    		String measureName = queryResult.getJSONObject(i).getString(param.getQueryString().substring(1));
    		String subTypeName = queryResult.getJSONObject(i).getString(qual.getQueryString().substring(1));
			String unitNameQueried = queryResult.getJSONObject(i).getString(unit.getQueryString().substring(1));
			String latString = queryResult.getJSONObject(i).getString(lat.getQueryString().substring(1));
			String lonString = queryResult.getJSONObject(i).getString(lon.getQueryString().substring(1));
    		
    		Station stationObject;
    		if (stationMap.containsKey(stationIri)) {
    			stationObject = stationMap.get(stationIri);
    		} else {
    			stationObject = new Station(stationIri);
				stationMap.put(stationIri, stationObject);
				stations.add(stationObject);
			}

			stationObject.setIdentifier(queryResult.getJSONObject(i).getString(ref.getQueryString().substring(1)));
			stationObject.setLabel(queryResult.getJSONObject(i).getString(label.getQueryString().substring(1)));

			// create measure object and add to station
			Measure measure = new Measure(measureIri);
			measure.setParameterName(measureName);
			measure.setQualifier(subTypeName);
			measure.setUnit(unitNameQueried);

			stationObject.addMeasure(measure);
			stationObject.setLat(Double.parseDouble(latString));
			stationObject.setLon(Double.parseDouble(lonString));

			// optional stagescales. should be attached to measures instead, but not as straightforward as this
			// due to nature of query results
			if (queryResult.getJSONObject(i).has(stageLowerBoundVar.getQueryString().substring(1))) {
				stationObject.setStageLower(queryResult.getJSONObject(i).getDouble(stageLowerBoundVar.getQueryString().substring(1)));
			}
			if (queryResult.getJSONObject(i).has(stageUpperBoundVar.getQueryString().substring(1))) {
				stationObject.setStageUpper(queryResult.getJSONObject(i).getDouble(stageUpperBoundVar.getQueryString().substring(1)));
			}
			if (queryResult.getJSONObject(i).has(downstageUpperBoundVar.getQueryString().substring(1))) {
				stationObject.setDownstageUpper(queryResult.getJSONObject(i).getDouble(downstageUpperBoundVar.getQueryString().substring(1)));
			}
			if (queryResult.getJSONObject(i).has(downstageLowerBoundVar.getQueryString().substring(1))) {
				stationObject.setDownstageLower(queryResult.getJSONObject(i).getDouble(downstageLowerBoundVar.getQueryString().substring(1)));
			}
		}
	    return stations;
	}
	
	/**
	 * add triples related to OntoEMS
	 * @param stations
	 */
	void addStationTypeAndCoordinates(List<Station> stations) {
		ModifyQuery modify = Queries.MODIFY();

		// prepare geojson for gdal
		JSONObject geojson = new JSONObject();
		geojson.put("type","FeatureCollection");

		JSONArray features = new JSONArray();
		geojson.put("features", features);
		
		for (Station station : stations) {
			if (station.getMeasures().stream().anyMatch(m -> m.getParameterName().contentEquals(WATER_LEVEL_STRING))) {
				modify.insert(iri(station.getIri()).isA(WATER_LEVEL_REPORTING_STATION));
			} else {
				modify.insert(iri(station.getIri()).isA(REPORTING_STATION));
			}
			modify.insert(iri(station.getIri()).has(DATA_SOURCE, "Environment Agency Real Time flood-monitoring"));

			// geojson
			JSONObject feature = new JSONObject();
			feature.put("type", "Feature");

			JSONObject geometry = new JSONObject();
			geometry.put("coordinates", new JSONArray().put(station.getLon()).put(station.getLat()));
			geometry.put("type", "Point");
			feature.put("geometry",geometry);

			JSONObject properties = new JSONObject();
			properties.put("iri", station.getIri());
			properties.put("geom_iri", String.format("%s/geometry", station.getIri()));
			properties.put("name", String.format("Environment Agency: %s (%s)", station.getLabel(), station.getIdentifier()));
			properties.put("type", station.getIconImage());
			properties.put("endpoint", this.storeClient.getQueryEndpoint());
            feature.put("properties", properties);

			features.put(feature);
		}
		
		LOGGER.info("Adding class to each station in the triple-store");
		modify.prefix(P_EMS);
		storeClient.executeUpdate(modify.getQueryString());

		LOGGER.info("Uploading GeoJSON to PostGIS");
		GDALClient gdalclient = new GDALClient();
		gdalclient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, EnvConfig.LAYERNAME, geojson.toString(), new Ogr2OgrOptions(), true);
	}
	
	/**
	 * replace original triple <station> <measures> <measure> with
	 * <station> <reports> <quantity>, <quantity> <hasValue> <measure>
	 * also adds other OntoEMS concepts such as range, trend
	 */
    void addMeasuresConcepts(List<Station> stations) {
		// delete all <station> <measures> <measure> triple
		ModifyQuery modify = Queries.MODIFY();
		Variable stationvar = SparqlBuilder.var("x");
		Variable measurevar = SparqlBuilder.var("y");
		modify.delete(stationvar.has(MEASURES,measurevar)).where(stationvar.has(MEASURES,measurevar));

		// add the new ontoEMS triples
		for (Station station : stations) {
			for (Measure measure : station.getMeasures()) {
				Iri quantityIri = null;
				String paramName = measure.getParameterName();
				String qual = measure.getQualifier();
				// determine class of quantity
				switch (paramName) {
					case WATER_LEVEL_STRING:
						quantityIri = iri(station.getIri() + "/WaterLevel");
						modify.insert(quantityIri.isA(WATER_LEVEL));
						
						// only add trends/ranges for stations that have stage info
						if ((qual.contentEquals(DOWNSTREAM_STAGE) && station.getDownstageLower() != null && station.getDownstageUpper() != null) || 
							(qual.contentEquals(STAGE) && station.getStageLower() != null && station.getStageUpper() != null)) {
							// add dummy triple for range so that sparql update will work
							modify.insert(iri(measure.getIri()).has(HAS_CURRENT_RANGE, UNAVAILABLE_RANGE));
							modify.insert(iri(measure.getIri()).has(HAS_CURRENT_TREND, UNAVAILABLE_TREND));
						}
						break;
					case "Flow":
						quantityIri = iri(station.getIri() + "/Flow");
						modify.insert(quantityIri.isA(WATER_FLOW));
						break;
					case RAINFALL_STRING:
						quantityIri = iri(station.getIri() + "/Rainfall");
						modify.insert(quantityIri.isA(RAINFALL));
						break;
					case "Wind":
						if (qual.contentEquals("Direction")) {
							quantityIri = iri(station.getIri() + "/WindDirection");
							modify.insert(quantityIri.isA(WIND_DIRECTION));
						} else if (qual.contentEquals("Speed")) {
							quantityIri = iri(station.getIri() + "/WindSpeed");
							modify.insert(quantityIri.isA(WIND_SPEED));
						} else {
							LOGGER.warn(("Unknown qual for wind, {}"), qual);
							continue;
						}
						break;
					case "Temperature":
						if (qual.contentEquals("Wet Bulb")) {
							quantityIri = iri(station.getIri() + "/WetBulbTemperature");
							modify.insert(quantityIri.isA(WET_BULB_TEMPERATURE));
						} else {
							quantityIri = iri(station.getIri() + "/Temperature");
							modify.insert(quantityIri.isA(AIR_TEMPERATURE));
						}
						break;
					default:
					    LOGGER.warn(("Unknown parameter, paramName={}, qual={}"), paramName, qual);
					    continue;
				}
				modify.insert(iri(station.getIri()).has(REPORTS, quantityIri));
				modify.insert(quantityIri.has(HAS_VALUE, iri(measure.getIri())));
				
				modify.insert(iri(measure.getIri()).isA(MEASURE));

				// unit
				if (unitMap.containsKey(measure.getUnit())) {
					modify.insert(iri(measure.getIri()).has(HAS_UNIT, unitMap.get(measure.getUnit())));
				}
			}
		}
		modify.prefix(P_EMS,P_OM);
		storeClient.executeUpdate(modify.getQueryString());
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
				
		GraphPattern queryPattern = station.has(PropertyPaths.path(REPORTS,HAS_VALUE), measure);
		List<String> stationIriList = new ArrayList<>(stations.keySet());
		ValuesPattern stationPattern = new ValuesPattern(station, stationIriList.stream().map(Rdf::iri).collect(Collectors.toList()));
		
		query.select(measure,station).where(queryPattern, stationPattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		Map<String, List<String>> stationMeasureMap = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
			String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
			
			if (stationMeasureMap.containsKey(stationIri)) {
				stationMeasureMap.get(stationIri).add(measureIri);
			} else {
				List<String> newMeasureList = new ArrayList<>();
				newMeasureList.add(measureIri);
				stationMeasureMap.put(stationIri, newMeasureList);
			}
		}
	    
	    return stationMeasureMap;
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
			case WATER_LEVEL_STRING:
			    quantityIri = iri(station + "/WaterLevel");
				modify.insert(quantityIri.isA(WATER_LEVEL));

				if ((qual.contentEquals(STAGE) && stationHasStage(station)) || (qual.contentEquals(DOWNSTREAM_STAGE) && stationHasDownstage(station))) {
					// dummy range triple to modified in sparql update
					modify.insert(iri(measure).has(HAS_CURRENT_RANGE, UNAVAILABLE_RANGE));
					modify.insert(iri(measure).has(HAS_CURRENT_TREND, UNAVAILABLE_TREND));
				}

				// some stations did not have any measures when initialised and do not contain an rdf:type
				modify.insert(iri(station).isA(WATER_LEVEL_REPORTING_STATION));

				// the following triple may be added in the initialisation when the station measures
				// something else but not Water Level
				modify.delete(iri(station).isA(REPORTING_STATION));
				break;
			case "Flow":
			    quantityIri = iri(station + "/Flow");
				modify.insert(quantityIri.isA(WATER_FLOW));
				break;
			case RAINFALL_STRING:
			    quantityIri = iri(station + "/Rainfall");
				modify.insert(quantityIri.isA(RAINFALL));
				break;
			case "Wind":
				if (qual.contentEquals("Direction")) {
					quantityIri = iri(station + "/WindDirection");
					modify.insert(quantityIri.isA(WIND_DIRECTION));
				} else if (qual.contentEquals("Speed")) {
					quantityIri = iri(station + "/WindSpeed");
					modify.insert(quantityIri.isA(WIND_SPEED));
				} else {
					LOGGER.warn(("Unknown qual for wind, {}"), qual);
					return;
				}
				break;
			case "Temperature":
			    if (qual.contentEquals("Wet Bulb")) {
					quantityIri = iri(station + "/WetBulbTemperature");
					modify.insert(quantityIri.isA(WET_BULB_TEMPERATURE));
				} else {
					quantityIri = iri(station + "/Temperature");
					modify.insert(quantityIri.isA(AIR_TEMPERATURE));
				}
				break;
			default:
				LOGGER.warn(("Unknown parameter, paramName={}, qual={}"), paramName, qual);
				return;
		}

		modify.insert(iri(station).has(REPORTS,quantityIri));

		modify.insert(quantityIri.has(HAS_VALUE, iri(measure)));
		modify.insert(iri(measure).has(UNIT_NAME, unit)
				.andHas(PARAMETER_NAME, paramName)
				.andHas(QUALIFIER,qual)
				.andIsA(MEASURE));

		// add ems unit 
		if (unitMap.containsKey(unit)) {
			modify.insert(iri(measure).has(HAS_UNIT, unitMap.get(unit)));
		}

		modify.prefix(P_EMS,P_OM);
		storeClient.executeUpdate(modify.getQueryString());
	}

	boolean stationHasStage(String station) {
		boolean hasStage = false;
		SelectQuery query = Queries.SELECT();
		GraphPattern queryPattern = iri(station).has(STAGE_SCALE, query.var());
		query.where(queryPattern);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 0) {
			hasStage = true;
		}
		return hasStage;
	}

	boolean stationHasDownstage(String station) {
		boolean hasDownstage = false;
		SelectQuery query = Queries.SELECT();
		GraphPattern queryPattern = iri(station).has(DOWNSTAGE_SCALE, query.var());
		query.where(queryPattern);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 0) {
			hasDownstage = true;
		}
		return hasDownstage;
	}

	/** 
	 * performs a very simple check on whether stations are already initialised
	 * with time series
	 * @return
	 */
	boolean areStationsInitialised() {
		boolean initialised = false;
		SelectQuery query = Queries.SELECT();
		Variable station = query.var();
		
		GraphPattern queryPattern = station.isA(REPORTING_STATION);
		
		query.prefix(P_EMS).where(queryPattern).limit(10);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() >= 1) {
			initialised = true;
		}
		return initialised;
	}

	LocalDate getLatestUpdate(TimeSeriesClient<Instant> tsClient) {
		TimeSeries<Instant> ts = tsClient.getLatestData(EnvConfig.TIME_IRI);
		return LocalDate.parse(ts.getValuesAsString(EnvConfig.TIME_IRI).get(0));
	}
	
	/**
	 * returns true if given date exists
	 * @param date
	 * @return
	 */
	boolean checkUpdateDateExists(TimeSeriesClient<Instant> tsClient, LocalDate date) {
		boolean dataExists = false;
		TimeSeries<Instant> ts = tsClient.getLatestData(EnvConfig.TIME_IRI);
		if (!ts.getValues(EnvConfig.TIME_IRI).isEmpty()) {
			LocalDate latestDate = LocalDate.parse(ts.getValuesAsString(EnvConfig.TIME_IRI).get(0));
			if(date.equals(latestDate)) {
				dataExists = true;
			}
		}
		return dataExists;
	}
    
	/**
	 * stations list - stations we want to query
	 * station_map - contain all the stations, key = IRI
	 */
	void addStageScaleToStations(List<String> stations, Map<String,Station> stationMap) {
		SelectQuery query = Queries.SELECT();

		Variable station = query.var();
		Variable stageScaleVar = query.var();
		Variable upperBound = query.var();
		Variable lowerBound = query.var();
		
		ValuesPattern vp = new ValuesPattern(station, stations.stream().map(Rdf::iri).collect(Collectors.toList()));

		GraphPattern queryPattern = GraphPatterns.and(station.has(STAGE_SCALE, stageScaleVar),
			stageScaleVar.has(TYPICAL_RANGE_HIGH, upperBound),
			stageScaleVar.has(TYPICAL_RANGE_LOW, lowerBound));

		query.select(upperBound,lowerBound,station).where(queryPattern,vp);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		for (int i = 0; i < queryResult.length(); i++) {
			Station stationObject = stationMap.get(queryResult.getJSONObject(i).getString(station.getQueryString().substring(1)));
			stationObject.setStageLower(queryResult.getJSONObject(i).getDouble(lowerBound.getQueryString().substring(1)));
			stationObject.setStageUpper(queryResult.getJSONObject(i).getDouble(upperBound.getQueryString().substring(1)));
		}
	}

	/**
	 * stations list - stations we want to query
	 * station_map - contain all the stations, key = IRI
	 */
	void addDownstageScaleToStations(List<String> stations, Map<String,Station> stationMap) {
		SelectQuery query = Queries.SELECT();

		Variable station = query.var();
		Variable stageScaleVar = query.var();
		Variable upperBound = query.var();
		Variable lowerBound = query.var();
		
		ValuesPattern vp = new ValuesPattern(station, stations.stream().map(Rdf::iri).collect(Collectors.toList()));

		GraphPattern queryPattern = GraphPatterns.and(station.has(DOWNSTAGE_SCALE, stageScaleVar),
			stageScaleVar.has(TYPICAL_RANGE_HIGH, upperBound),
			stageScaleVar.has(TYPICAL_RANGE_LOW, lowerBound));

		query.select(upperBound,lowerBound,station).where(queryPattern,vp);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		for (int i = 0; i < queryResult.length(); i++) {
			Station stationObject = stationMap.get(queryResult.getJSONObject(i).getString(station.getQueryString().substring(1)));
			stationObject.setDownstageLower(queryResult.getJSONObject(i).getDouble(lowerBound.getQueryString().substring(1)));
			stationObject.setDownstageUpper(queryResult.getJSONObject(i).getDouble(upperBound.getQueryString().substring(1)));
		}
	}

	/**
	 * the station will be part of an rdf:collection, something like
	 * ?blankNode rdf:first <station>
	 * @param station
	 * @return
	 */
    boolean checkStationExists(String station) {
		boolean stationExists = false;
    	SelectQuery query = Queries.SELECT();
    	GraphPattern gp1 = query.var().has(query.var(), iri(station));
    	
    	query.where(gp1).prefix(P_EMS);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
	    if(queryResult.length() > 0) {
	    	stationExists = true;
	    }

		return stationExists;
    }

	/**
	 * datum is not within the original dataset, steps in this function
	 * 1) query stageScale for each station
	 * 2) download stageScale
	 * @throws URISyntaxException
	 */
	JSONObject downloadDatum(List<Station> stations) throws URISyntaxException {
		List<Iri> stationIris = stations.stream().map(s -> iri(s.getIri())).collect(Collectors.toList());
		
		// query stagescale URL for each station
		SelectQuery query = Queries.SELECT();
		Variable stationvar = query.var();
		Variable stageScaleVar = query.var();
		ValuesPattern valuesPattern = new ValuesPattern(stationvar, stationIris);
		GraphPattern queryPattern = stationvar.has(STAGE_SCALE, stageScaleVar);

		query.select(stationvar,stageScaleVar).where(valuesPattern, queryPattern).distinct();

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		JSONObject datumMap = new JSONObject();
		// download stageScale and append triple for each station
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(stationvar.getQueryString().substring(1));
			String stageScaleIri = queryResult.getJSONObject(i).getString(stageScaleVar.getQueryString().substring(1));

			APIConnector api = new APIConnector(stageScaleIri);
			try {
				CloseableHttpClient httpclient = HttpClients.createDefault();
				HttpEntity result = api.getData(httpclient);
				JSONObject resultJo = new JSONObject(EntityUtils.toString(result));
				double datum = resultJo.getJSONObject("items").getDouble("datum");

				datumMap.put(stationIri, datum);
			} catch (IOException | URISyntaxException e) {
				LOGGER.warn("Download from {} failed", stageScaleIri);
			}
		}
	
		return datumMap;
	}

	void addDatum(JSONObject datumJson) {
		ModifyQuery modify = Queries.MODIFY();

		for (String station : datumJson.keySet()) {
			double datum = datumJson.getDouble(station);
			modify.insert(iri(station).has(HAS_OBSERVATION_ELEVATION, datum));
		}

		modify.prefix(P_EMS);
		storeClient.executeUpdate(modify.getQueryString());
	}

	void postToRemoteStore(HttpEntity entity) throws IOException {
		// use Blazegraph's REST API to upload RDF data to a SPARQL endpoint
		LOGGER.info("Posting data to {}", storeClient.getUpdateEndpoint());
		HttpPost postRequest = new HttpPost(storeClient.getUpdateEndpoint());
		
		// add contents downloaded from the API to the post request 
		postRequest.setEntity(entity);
		// then send the post request
		CloseableHttpClient httpclient = HttpClients.createDefault();
		try {
			httpclient.execute(postRequest);
		} finally {
			httpclient.close();
		}
	}

	/**
	 * query ranges from <station> <stageScale> <stageScale>
	 * @param tsClient
	 * @param measureIRIs
	 */
	List<Measure> addRangeForStageScale(TimeSeriesClient<Instant> tsClient, List<String> measureIRIs, Connection conn) {
		ModifyQuery modify = Queries.MODIFY(); // to store triples to add at the end
		
		// match meaures with qualifier = Stage
		// first query the upper and lower bounds for each data
		SelectQuery query = Queries.SELECT();
		Variable station = query.var();
		Variable upperBoundVar = query.var();
		Variable lowerBoundVar = query.var();
		Variable measure = query.var();
		Variable quantity = query.var();

		Variable oldrange = query.var(); // used in update query only

		GraphPattern gp1 = station.has(REPORTS, quantity)
		.andHas(PropertyPaths.path(STAGE_SCALE, TYPICAL_RANGE_LOW), lowerBoundVar)
		.andHas(PropertyPaths.path(STAGE_SCALE, TYPICAL_RANGE_HIGH), upperBoundVar);

		GraphPattern gp2 = quantity.isA(WATER_LEVEL).andHas(HAS_VALUE, measure);

		GraphPattern measureGp = measure.has(QUALIFIER, STAGE);

		ValuesPattern vp = new ValuesPattern(measure, measureIRIs.stream().map(Rdf::iri).collect(Collectors.toList()));

		query.select(measure,station,upperBoundVar,lowerBoundVar).where(gp1,vp,measureGp,gp2).prefix(P_OM,P_EMS);

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
			TimeSeries<Instant> ts = tsClient.getLatestData(measureIri, conn);
			List<Double> values = ts.getValuesAsDouble(measureIri);
			if (!values.isEmpty()) {
				double latestValue = values.get(values.size()-1);
				measureRangesToDelete.add(measureIri);

				// determine range
				if (latestValue < lowerbound) {
					modify.insert(iri(measureIri).has(HAS_CURRENT_RANGE, LOW_RANGE));
				} else if (latestValue > upperbound) {
					modify.insert(iri(measureIri).has(HAS_CURRENT_RANGE, HIGH_RANGE));
				} else {
					modify.insert(iri(measureIri).has(HAS_CURRENT_RANGE, NORMAL_RANGE));
				}
			}
		}

		// delete old ranges
		vp = new ValuesPattern(measure, measureRangesToDelete.stream().map(Rdf::iri).collect(Collectors.toList()));
		modify.delete(measure.has(HAS_CURRENT_RANGE, oldrange)).where(measure.has(HAS_CURRENT_RANGE, oldrange), vp);

		modify.prefix(P_EMS);
		storeClient.executeUpdate(modify.getQueryString());
		return measureObjectList;
	}

	/**
	 * similar to above but for downstage
	 * @param tsClient
	 * @param measureIRIs
	 */
	List<Measure> addRangeForDownstageScale(TimeSeriesClient<Instant> tsClient, List<String> measureIRIs, Connection conn) {
		ModifyQuery modify = Queries.MODIFY();

		// match meaures with qualifier = Downstream Stage
		// first query the upper and lower bounds for each data
		SelectQuery query = Queries.SELECT();
		Variable station = query.var();
		Variable upperBoundVar = query.var();
		Variable lowerBoundVar = query.var();
		Variable measure = query.var();
		Variable quantity = query.var();

		Variable oldrange = query.var(); // used in update query

		GraphPattern gp1 = station.has(REPORTS, quantity)
		.andHas(PropertyPaths.path(DOWNSTAGE_SCALE, TYPICAL_RANGE_LOW), lowerBoundVar)
		.andHas(PropertyPaths.path(DOWNSTAGE_SCALE, TYPICAL_RANGE_HIGH), upperBoundVar);

		GraphPattern gp2 = quantity.isA(WATER_LEVEL).andHas(HAS_VALUE, measure);
		GraphPattern measureGp = measure.has(QUALIFIER, DOWNSTREAM_STAGE);

		ValuesPattern vp = new ValuesPattern(measure, measureIRIs.stream().map(Rdf::iri).collect(Collectors.toList()));

		query.select(measure,station,upperBoundVar,lowerBoundVar).where(gp1,vp,measureGp,gp2).prefix(P_OM,P_EMS);

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
			TimeSeries<Instant> ts = tsClient.getLatestData(measureIri, conn);
			List<Double> values = ts.getValuesAsDouble(measureIri);
			if (!values.isEmpty()) {
				double latestValue = values.get(values.size()-1);
				measureRangesToDelete.add(measureIri);

				// determine range
				if (latestValue < lowerbound) {
					modify.insert(iri(measureIri).has(HAS_CURRENT_RANGE, LOW_RANGE));
				} else if (latestValue > upperbound) {
					modify.insert(iri(measureIri).has(HAS_CURRENT_RANGE, HIGH_RANGE));
				} else {
					modify.insert(iri(measureIri).has(HAS_CURRENT_RANGE, NORMAL_RANGE));
				}
			}
		}
		// delete old ranges
		vp = new ValuesPattern(measure, measureRangesToDelete.stream().map(Rdf::iri).collect(Collectors.toList()));
		modify.delete(measure.has(HAS_CURRENT_RANGE, oldrange)).where(measure.has(HAS_CURRENT_RANGE, oldrange), vp);

		modify.prefix(P_EMS);
		storeClient.executeUpdate(modify.getQueryString());

		return measureObjectList;
	}

	void addConnections(File connectionsFile) {
		// this will be used to query the IRIs of the stations
		List<Integer> rLOIidList = new ArrayList<>();
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

					if (!rLOIidList.contains(upstream)) {
						rLOIidList.add(upstream);
					}

					if (!rLOIidList.contains(downstream)) {
						rLOIidList.add(downstream);
					}
				}
			}
			csvReader.close();
		} catch (IOException e) {
			LOGGER.error("Possible error reading csv file");
			LOGGER.error(e.getMessage());
		}

		Map<Integer, String> rLOIidToIRIMap = new HashMap<>();
		if (!rLOIidList.isEmpty()) {
			// now query the station IRIs
			SelectQuery query = Queries.SELECT();
			Iri rLOIid = iri("http://environment.data.gov.uk/flood-monitoring/def/core/RLOIid");
			Variable station = query.var();
			Variable idVar = query.var();

			ValuesPattern valuesPattern = new ValuesPattern(idVar, rLOIidList);
			GraphPattern queryPattern = station.has(rLOIid, idVar);

			query.select(station,idVar).where(valuesPattern, queryPattern);

			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

			for (int i = 0; i < queryResult.length(); i++) {
				String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
				String id = queryResult.getJSONObject(i).getString(idVar.getQueryString().substring(1));
				rLOIidToIRIMap.put(Integer.parseInt(id), stationIri);
			}

			// now that we have the station IRIs, we can add the triples for each connection
			ModifyQuery modify = Queries.MODIFY();
			Iterator<Integer> upstreamIDIterator = upstreamToDownstreamMap.keySet().iterator();

			while (upstreamIDIterator.hasNext()) {
				int upstreamID = upstreamIDIterator.next();
				int downstreamID = upstreamToDownstreamMap.get(upstreamID);

				String upstreamIRI = rLOIidToIRIMap.get(upstreamID);
				String downstreamIRI = rLOIidToIRIMap.get(downstreamID);

				if (upstreamIRI != null && downstreamIRI != null) {
					modify.insert(iri(upstreamIRI).has(HAS_DOWNSTREAM_STATION, iri(downstreamIRI)));
				} else {
					LOGGER.warn("Null IRI detected, upstreamID and downstreamID = {}, {}", upstreamID, downstreamID);
				}
				
			}
			modify.prefix(P_EMS);
			storeClient.executeUpdate(modify.getQueryString());
		}
	}

	/**
	 * queries time series over the last 12 hours
	 * uses the difference between final and first value
	 * if the change is greater than 10% based on the typical range, it is marked as either rising or falling
	 */
	void addTrends(TimeSeriesClient<Instant> tsClient, List<Measure> measures, Instant lowerbound, Instant upperbound, Connection conn) {
		ModifyQuery modify = Queries.MODIFY();

		List<String> oldMeasureTrendToDelete = new ArrayList<>();
		for (Measure measure : measures) {
			TimeSeries<Instant> ts = tsClient.getTimeSeriesWithinBounds(Arrays.asList(measure.getIri()), lowerbound, upperbound, conn);
			List<Double> values = ts.getValuesAsDouble(measure.getIri());

			if (!values.isEmpty()) {
				double fractionDifference = (values.get(values.size()-1) - values.get(0)) / (measure.getRangeHigh() - measure.getRangeLow());
				
				if (fractionDifference > 0.1) {
					modify.insert(iri(measure.getIri()).has(HAS_CURRENT_TREND, RISING));
				} else if (fractionDifference < -0.1) {
					modify.insert(iri(measure.getIri()).has(HAS_CURRENT_TREND, FALLING));
				} else {
					modify.insert(iri(measure.getIri()).has(HAS_CURRENT_TREND, STEADY));
				}

				oldMeasureTrendToDelete.add(measure.getIri());
			}
		}

		// delete old trends
		Variable oldtrend = SparqlBuilder.var("oldtrend");
		Variable measureVar = SparqlBuilder.var("measure");

		ValuesPattern vp = new ValuesPattern(measureVar, oldMeasureTrendToDelete.stream().map(Rdf::iri).collect(Collectors.toList()));

		modify.delete(measureVar.has(HAS_CURRENT_TREND, oldtrend)).where(measureVar.has(HAS_CURRENT_TREND, oldtrend), vp);
		modify.prefix(P_EMS);
		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * if the original station is a ReportingStation, change it to WaterLevelReportingStation
	 */
	void changeStationToWaterLevelReportingStation(String station) {
		ModifyQuery modify = Queries.MODIFY();

		modify.insert(iri(station).isA(WATER_LEVEL_REPORTING_STATION)).where(iri(station).isA(REPORTING_STATION)).delete(iri(station).isA(REPORTING_STATION)).prefix(P_EMS);

		storeClient.executeUpdate(modify.getQueryString());
	}

	List<String> getAllMeasuresWithTimeseries() {
		SelectQuery query = Queries.SELECT();

		Variable measure = query.var();
		Variable timeseries = query.var();

		GraphPattern gp = measure.has(iri("https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries"),timeseries);

		query.select(measure).where(gp);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		List<String> measuresList = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			measuresList.add(queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1)));
		}

		return measuresList;
	}

	void deleteMeasures(List<String> measures) {
		ModifyQuery modify = Queries.MODIFY();

		Variable measure = SparqlBuilder.var("measure");
		Variable a = SparqlBuilder.var("a");
		Variable b = SparqlBuilder.var("b");
		Variable c = SparqlBuilder.var("c");
		Variable d = SparqlBuilder.var("d");

		TriplePattern tp1 = measure.has(a,b);
		TriplePattern tp2 = c.has(d, measure);

		ValuesPattern vp = new ValuesPattern(measure, measures.stream().map(Rdf::iri).collect(Collectors.toList()));

		modify.delete(tp1,tp2).where(tp1,tp2,vp);

		storeClient.executeUpdate(modify.getQueryString());
	}

    List<StationConnection> getConnections(Map<String, Station> stationsMap) {
		SelectQuery query = Queries.SELECT();
		Variable upstream = query.var();
		Variable downstream = query.var();

		query.where(upstream.has(HAS_DOWNSTREAM_STATION, downstream)).prefix(P_EMS);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		List<StationConnection> connections = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String upstreamIri = queryResult.getJSONObject(i).getString(upstream.getQueryString().substring(1));
			String downstreamIri = queryResult.getJSONObject(i).getString(downstream.getQueryString().substring(1));

			if (stationsMap.containsKey(upstreamIri) && stationsMap.containsKey(downstreamIri)) {
				Station upstreamStation = stationsMap.get(upstreamIri);
				Station downstreamStation = stationsMap.get(downstreamIri);

				connections.add(new StationConnection(upstreamStation, downstreamStation));
			}
		}

		return connections;
	}
}
