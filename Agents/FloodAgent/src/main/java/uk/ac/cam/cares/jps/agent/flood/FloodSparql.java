package uk.ac.cam.cares.jps.agent.flood;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.file.Files;
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
import uk.ac.cam.cares.jps.agent.flood.objects.Connection;
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
    private static Prefix preEms = SparqlBuilder.prefix("ems",iri(ontoems));
    private static Prefix preGeo = SparqlBuilder.prefix("geo",iri("http://www.bigdata.com/rdf/geospatial#"));
	private static Prefix preOm = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    
    // classes
	private static Iri ReportingStation = preEms.iri("ReportingStation");
	private static Iri WaterLevelReportingStation = preEms.iri("WaterLevelReportingStation");

	// subclass of Quantity
	private static Iri WaterLevel = preEms.iri("WaterLevel");
	private static Iri Rainfall = preEms.iri("Rainfall");
	private static Iri WaterFlow = preEms.iri("WaterFlow");
	private static Iri AirTemperature = preEms.iri("AirTemperature");
	private static Iri WindSpeed = preEms.iri("WindSpeed");
	private static Iri WindDirection = preEms.iri("WindDirection");
	private static Iri WetBulbTemperature = preEms.iri("WetBulbTemperature");
	
	private static Iri Measure = preOm.iri("Measure");

	//ranges
	private static Iri NormalRange = preEms.iri("NormalRange");
	private static Iri HighRange = preEms.iri("HighRange");
	private static Iri LowRange = preEms.iri("LowRange");
	private static Iri UnavailableRange = preEms.iri("UnavailableRange");

	// trends
	private static Iri Steady = preEms.iri("Steady");
	private static Iri Rising = preEms.iri("Rising");
	private static Iri Falling = preEms.iri("Falling");
	private static Iri UnavailableTrend = preEms.iri("UnavailableTrend");

    // properties
	private static Iri hasObservationLocation = preEms.iri("hasObservationLocation");
	private static Iri hasObservationElevation = preEms.iri("hasObservationElevation");
	private static Iri hasCurrentRange = preEms.iri("hasCurrentRange");
	private static Iri hasCurrentTrend = preEms.iri("hasCurrentTrend");
	private static Iri dataSource = preEms.iri("dataSource");
	private static Iri hasDownstreamStation = preEms.iri("hasDownstreamStation");
	private static Iri hasValue = preOm.iri("hasValue");
	private static Iri hasUnit = preOm.iri("hasUnit");
	private static Iri reports = preEms.iri("reports");
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
    
    static Map<String, Iri> unitMap = new HashMap<String, Iri>() ;
	static {
		unitMap.put("mAOD", iri("http://theworldavatar.com/resource/ontouom/metreAOD"));
		unitMap.put("m", iri("http://theworldavatar.com/resource/ontouom/metreUnspecified"));
		unitMap.put("mASD", iri("http://theworldavatar.com/resource/ontouom/metreASD"));
		unitMap.put("mm", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/millimetre"));
		unitMap.put("mBDAT", iri("http://theworldavatar.com/resource/ontouom/metreBDAT"));
		unitMap.put("l/s", iri("http://theworldavatar.com/resource/ontouom/litrePerSecond"));
		unitMap.put("m3/s", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/cubicMetrePerSecond-Time"));
		unitMap.put("deg", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/degree"));
		unitMap.put("m/s", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/PrefixedMetrePerSecond-Time"));
		unitMap.put("Knots", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/knot-International"));
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
		GraphPattern stationPropertiesPattern = station.has(measures, measureVar).andHas(lat_prop,lat).andHas(lon_prop,lon).andHas(stationReference,ref).andHas(iri(RDFS.LABEL),label);
		GraphPattern measurePropertiesPattern = measureVar.has(parameterName,param).andHas(qualifier,qual).andHas(unitName,unit);
		GraphPattern stageScalePattern = station.has(PropertyPaths.path(stageScale, typicalRangeLow), stageLowerBoundVar)
			.andHas(PropertyPaths.path(stageScale, typicalRangeHigh), stageUpperBoundVar).optional();
		GraphPattern downStageScalePattern = station.has(PropertyPaths.path(downstageScale, typicalRangeLow), downstageLowerBoundVar)
		.andHas(PropertyPaths.path(stageScale, typicalRangeHigh), downstageUpperBoundVar).optional();
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
			if (station.getMeasures().stream().anyMatch(m -> m.getParameterName().contentEquals("Water Level"))) {
				modify.insert(iri(station.getIri()).isA(WaterLevelReportingStation));
			} else {
				modify.insert(iri(station.getIri()).isA(ReportingStation));
			}
			modify.insert(iri(station.getIri()).has(dataSource, "Environment Agency Real Time flood-monitoring"));

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
            feature.put("properties", properties);

			features.put(feature);
		}
		
		LOGGER.info("Adding class to each station in the triple-store");
		modify.prefix(preEms);
		storeClient.executeUpdate(modify.getQueryString());

		LOGGER.info("Uploading GeoJSON to PostGIS");
		GDALClient gdalclient = new GDALClient();
		gdalclient.uploadVectorStringToPostGIS(Config.DATABASE, Config.LAYERNAME, geojson.toString(), new Ogr2OgrOptions(), true);
	}
	
	/**
	 * replace original triple <station> <measures> <measure> with
	 * <station> <reports> <quantity>, <quantity> <hasValue> <measure>
	 * also adds other OntoEMS concepts such as range, trend
	 */
    void addMeasuresConcepts(List<Station> stations) {
		// delete all <station> <measures> <measure> triple
		ModifyQuery modify = Queries.MODIFY();
		Variable stationvar = SparqlBuilder.var("station");
		Variable measurevar = SparqlBuilder.var("measure");
		modify.delete(stationvar.has(measures,measurevar)).where(stationvar.has(measures,measurevar));

		// add the new ontoEMS triples
		for (Station station : stations) {
			for (Measure measure : station.getMeasures()) {
				Iri quantityIri = null;
				String paramName = measure.getParameterName();
				String qual = measure.getQualifier();
				// determine class of quantity
				switch (paramName) {
					case "Water Level":
						quantityIri = iri(station.getIri() + "/WaterLevel");
						modify.insert(quantityIri.isA(WaterLevel));
						
						// only add trends/ranges for stations that have stage info
						if ((qual.contentEquals("Downstream Stage") && station.getDownstageLower() != null && station.getDownstageUpper() != null) || 
							(qual.contentEquals("Stage") && station.getStageLower() != null && station.getStageUpper() != null)) {
							// add dummy triple for range so that sparql update will work
							modify.insert(iri(measure.getIri()).has(hasCurrentRange, UnavailableRange));
							modify.insert(iri(measure.getIri()).has(hasCurrentTrend, UnavailableTrend));
						}
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
						} else {
							LOGGER.warn(("Unknown qual for wind, {}"), qual);
							continue;
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
					default:
					    LOGGER.warn(("Unknown parameter, paramName={}, qual={}"), paramName, qual);
					    continue;
				}
				modify.insert(iri(station.getIri()).has(reports, quantityIri));
				modify.insert(quantityIri.has(hasValue, iri(measure.getIri())));
				
				modify.insert(iri(measure.getIri()).isA(Measure));

				// unit
				if (unitMap.containsKey(measure.getUnit())) {
					modify.insert(iri(measure.getIri()).has(hasUnit, unitMap.get(measure.getUnit())));
				}
			}
		}
		modify.prefix(preEms,preOm);
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
		
		query.select(measure).where(queryPattern).prefix(preEms,preOm);
		
	    @SuppressWarnings("unchecked")
		List<String> measureList = storeClient.executeQuery(query.getQueryString()).toList().stream()
	    .map(datairi -> ((HashMap<String,String>) datairi).get(measure.getQueryString().substring(1))).collect(Collectors.toList());
	    
	    return measureList;
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

				if ((qual.contentEquals("Stage") && stationHasStage(station)) || (qual.contentEquals("Downstream Stage") && stationHasDownstage(station))) {
					// dummy range triple to modified in sparql update
					modify.insert(iri(measure).has(hasCurrentRange, UnavailableRange));
					modify.insert(iri(measure).has(hasCurrentTrend, UnavailableTrend));
				}

				// some stations did not have any measures when initialised and do not contain an rdf:type
				modify.insert(iri(station).isA(WaterLevelReportingStation));

				// the following triple may be added in the initialisation when the station measures
				// something else but not Water Level
				modify.delete(iri(station).isA(ReportingStation));
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
				} else {
					LOGGER.warn(("Unknown qual for wind, {}"), qual);
					return;
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
			default:
				LOGGER.warn(("Unknown parameter, paramName={}, qual={}"), paramName, qual);
				return;
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

		modify.prefix(preEms,preOm);
		storeClient.executeUpdate(modify.getQueryString());
	}

	boolean stationHasStage(String station) {
		SelectQuery query = Queries.SELECT();
		GraphPattern queryPattern = iri(station).has(stageScale, query.var());
		query.where(queryPattern);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 0) {
			return true;
		} else {
			return false;
		}
	}

	boolean stationHasDownstage(String station) {
		SelectQuery query = Queries.SELECT();
		GraphPattern queryPattern = iri(station).has(downstageScale, query.var());
		query.where(queryPattern);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 0) {
			return true;
		} else {
			return false;
		}
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
		
		query.prefix(preEms).where(queryPattern).limit(10);
		
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
	Map<String,Station> getStationsWithCoordinates(String southwest, String northeast) {
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
		Variable measureVar = query.var();
		// e.g. table name: Water Level (Tidal Level), param = Water Level,
    	// qual (param subtype) = Tidal Level
    	Variable param = query.var();
    	Variable qual = query.var();
    	Variable unit = query.var();
		Variable trend = query.var();
		Variable range = query.var();
		
		GraphPattern queryPattern = GraphPatterns.and(station.has(lat_prop,lat)
				.andHas(lon_prop,lon).andHas(stationReference,ref).andHas(PropertyPaths.path(reports,hasValue), measureVar));
		
		GraphPattern stationProperties = GraphPatterns.and(station.has(iri(RDFS.LABEL), label).optional(),
				station.has(river_prop, river).optional(),
				station.has(catchment_prop, catchment).optional(),
				station.has(town_prop, town).optional(),
				station.has(dateOpen_prop, dateOpened).optional());
		
		GraphPattern measurePropertiesPattern = GraphPatterns.and(measureVar.has(parameterName,param).andHas(qualifier,qual).andHas(unitName, unit),
				(measureVar.has(hasCurrentRange, range).andHas(hasCurrentTrend, trend).optional()));
		
		// restrict query location
		if (southwest != null && northeast != null) {
			GraphPattern coordinatesPattern = GraphPatterns.and(station.has(preGeo.iri("search"), "inRectangle")
					.andHas(preGeo.iri("searchDatatype"),lat_lon)
					.andHas(preGeo.iri("predicate"), hasObservationLocation)
					.andHas(preGeo.iri("spatialRectangleSouthWest"), southwest)
					.andHas(preGeo.iri("spatialRectangleNorthEast"), northeast));

	    	GraphPattern geoPattern = new ServicePattern(preGeo.iri("search").getQueryString()).service(coordinatesPattern);
	    	query.where(queryPattern,geoPattern,stationProperties,measurePropertiesPattern).prefix(preGeo,preEms,preOm);
		} else {
			query.where(queryPattern,stationProperties,measurePropertiesPattern).prefix(preEms,preOm);
		}
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		int visid = 0;
		Map<String, Station> station_map = new HashMap<>(); // iri to station object map to check for duplicate
		List<Station> stations = new ArrayList<>();
		List<String> stations_to_query_for_stagescale = new ArrayList<>();
		List<String> stations_to_query_for_downstagescale = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
			String measureIri = queryResult.getJSONObject(i).getString(measureVar.getQueryString().substring(1));
    		String measureName = queryResult.getJSONObject(i).getString(param.getQueryString().substring(1));
    		String subTypeName = queryResult.getJSONObject(i).getString(qual.getQueryString().substring(1));
    		String unitName = queryResult.getJSONObject(i).getString(unit.getQueryString().substring(1));
    		
    		Station stationObject;
    		if (station_map.containsKey(stationIri)) {
    			stationObject = station_map.get(stationIri);
    		} else {
    			stationObject = new Station(stationIri);
    			station_map.put(stationIri, stationObject);
				stations.add(stationObject);
    			
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
			Measure measure = new Measure(measureIri);
			measure.setParameterName(measureName);
			measure.setQualifier(subTypeName);
			measure.setUnit(unitName);
			if (queryResult.getJSONObject(i).has(trend.getQueryString().substring(1))) {
				measure.setTrend(queryResult.getJSONObject(i).getString(trend.getQueryString().substring(1)));
			}
			if (queryResult.getJSONObject(i).has(range.getQueryString().substring(1))) {
				measure.setRange(queryResult.getJSONObject(i).getString(range.getQueryString().substring(1)));
			}

			if (subTypeName.contentEquals("Stage")) {
				stations_to_query_for_stagescale.add(stationIri);
			}
			if (subTypeName.contentEquals("Downstream Stage")) {
				stations_to_query_for_downstagescale.add(stationIri);
			}

			stationObject.addMeasure(measure);
		}

		addStageScaleToStations(stations_to_query_for_stagescale, station_map);
		addDownstageScaleToStations(stations_to_query_for_downstagescale, station_map);
				
		return station_map;
	}
    
	/**
	 * stations list - stations we want to query
	 * station_map - contain all the stations, key = IRI
	 */
	void addStageScaleToStations(List<String> stations, Map<String,Station> station_map) {
		SelectQuery query = Queries.SELECT();

		Variable station = query.var();
		Variable stageScaleVar = query.var();
		Variable upperBound = query.var();
		Variable lowerBound = query.var();;
		
		ValuesPattern vp = new ValuesPattern(station, stations.stream().map(s -> iri(s)).collect(Collectors.toList()));

		GraphPattern queryPattern = GraphPatterns.and(station.has(stageScale, stageScaleVar),
			stageScaleVar.has(typicalRangeHigh, upperBound),
			stageScaleVar.has(typicalRangeLow, lowerBound));

		query.select(upperBound,lowerBound,station).where(queryPattern,vp);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		for (int i = 0; i < queryResult.length(); i++) {
			Station stationObject = station_map.get(queryResult.getJSONObject(i).getString(station.getQueryString().substring(1)));
			stationObject.setStageLower(queryResult.getJSONObject(i).getDouble(lowerBound.getQueryString().substring(1)));
			stationObject.setStageUpper(queryResult.getJSONObject(i).getDouble(upperBound.getQueryString().substring(1)));
		}
	}

	/**
	 * stations list - stations we want to query
	 * station_map - contain all the stations, key = IRI
	 */
	void addDownstageScaleToStations(List<String> stations, Map<String,Station> station_map) {
		SelectQuery query = Queries.SELECT();

		Variable station = query.var();
		Variable stageScaleVar = query.var();
		Variable upperBound = query.var();
		Variable lowerBound = query.var();;
		
		ValuesPattern vp = new ValuesPattern(station, stations.stream().map(s -> iri(s)).collect(Collectors.toList()));

		GraphPattern queryPattern = GraphPatterns.and(station.has(downstageScale, stageScaleVar),
			stageScaleVar.has(typicalRangeHigh, upperBound),
			stageScaleVar.has(typicalRangeLow, lowerBound));

		query.select(upperBound,lowerBound,station).where(queryPattern,vp);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		for (int i = 0; i < queryResult.length(); i++) {
			Station stationObject = station_map.get(queryResult.getJSONObject(i).getString(station.getQueryString().substring(1)));
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
    	SelectQuery query = Queries.SELECT();
    	GraphPattern gp1 = query.var().has(query.var(), iri(station));
    	
    	query.where(gp1).prefix(preEms);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
	    if(queryResult.length() > 0) {
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
		List<Iri> stationIris = stations.stream().map(s -> iri(s.getIri())).collect(Collectors.toList());
		
		// query stagescale URL for each station
		SelectQuery query = Queries.SELECT();
		Variable stationvar = query.var();
		Variable stageScaleVar = query.var();
		ValuesPattern valuesPattern = new ValuesPattern(stationvar, stationIris);
		GraphPattern queryPattern = stationvar.has(stageScale, stageScaleVar);

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
			modify.insert(iri(station).has(hasObservationElevation, datum));
		}

		modify.prefix(preEms);
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
			// httpclient.close();
		}
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
		Variable quantity = query.var();

		Variable oldrange = query.var(); // used in update query only

		GraphPattern gp1 = station.has(reports, quantity)
		.andHas(PropertyPaths.path(stageScale, typicalRangeLow), lowerBoundVar)
		.andHas(PropertyPaths.path(stageScale, typicalRangeHigh), upperBoundVar);

		GraphPattern gp2 = quantity.isA(WaterLevel).andHas(hasValue, measure);

		GraphPattern measureGp = measure.has(qualifier, "Stage");

		ValuesPattern vp = new ValuesPattern(measure, measureIRIs.stream().map(s -> iri(s)).collect(Collectors.toList()));

		query.select(measure,station,upperBoundVar,lowerBoundVar).where(gp1,vp,measureGp,gp2).prefix(preOm,preEms);

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
			if (!values.isEmpty()) {
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
		vp = new ValuesPattern(measure, measureRangesToDelete.stream().map(Rdf::iri).collect(Collectors.toList()));
		modify.delete(measure.has(hasCurrentRange, oldrange)).where(measure.has(hasCurrentRange, oldrange), vp);

		modify.prefix(preEms);
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
		Variable quantity = query.var();

		Variable oldrange = query.var(); // used in update query

		GraphPattern gp1 = station.has(reports, quantity)
		.andHas(PropertyPaths.path(downstageScale, typicalRangeLow), lowerBoundVar)
		.andHas(PropertyPaths.path(downstageScale, typicalRangeHigh), upperBoundVar);

		GraphPattern gp2 = quantity.isA(WaterLevel).andHas(hasValue, measure);
		GraphPattern measureGp = measure.has(qualifier, "Downstream Stage");

		ValuesPattern vp = new ValuesPattern(measure, measureIRIs.stream().map(s -> iri(s)).collect(Collectors.toList()));

		query.select(measure,station,upperBoundVar,lowerBoundVar).where(gp1,vp,measureGp,gp2).prefix(preOm,preEms);

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

		modify.prefix(preEms);
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
					modify.insert(iri(upstreamIRI).has(hasDownstreamStation, iri(downstreamIRI)));
				} else {
					LOGGER.warn("Null IRI detected, upstreamID and downstreamID = {}, {}", upstreamID, downstreamID);
				}
				
			}
			modify.prefix(preEms);
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

			if (!values.isEmpty()) {
				double fractionDifference = (values.get(values.size()-1) - values.get(0)) / (measure.getRangeHigh() - measure.getRangeLow());
				
				if (fractionDifference > 0.1) {
					modify.insert(iri(measure.getIri()).has(hasCurrentTrend, Rising));
				} else if (fractionDifference < -0.1) {
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
		modify.prefix(preEms);
		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * if the original station is a ReportingStation, change it to WaterLevelReportingStation
	 */
	void changeStationToWaterLevelReportingStation(String station) {
		ModifyQuery modify = Queries.MODIFY();

		modify.insert(iri(station).isA(WaterLevelReportingStation)).where(iri(station).isA(ReportingStation)).delete(iri(station).isA(ReportingStation)).prefix(preEms);

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

		ValuesPattern vp = new ValuesPattern(measure, measures.stream().map(s -> iri(s)).collect(Collectors.toList()));

		modify.delete(tp1,tp2).where(tp1,tp2,vp);

		storeClient.executeUpdate(modify.getQueryString());
	}

    List<Connection> getConnections(Map<String, Station> stationsMap) {
		SelectQuery query = Queries.SELECT();
		Variable upstream = query.var();
		Variable downstream = query.var();

		query.where(upstream.has(hasDownstreamStation, downstream)).prefix(preEms);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		List<Connection> connections = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String upstreamIri = queryResult.getJSONObject(i).getString(upstream.getQueryString().substring(1));
			String downstreamIri = queryResult.getJSONObject(i).getString(downstream.getQueryString().substring(1));

			if (stationsMap.containsKey(upstreamIri) && stationsMap.containsKey(downstreamIri)) {
				Station upstreamStation = stationsMap.get(upstreamIri);
				Station downstreamStation = stationsMap.get(downstreamIri);

				connections.add(new Connection(upstreamStation, downstreamStation));
			}
		}

		return connections;
	}
}
