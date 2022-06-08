package uk.ac.cam.cares.jps.agent.flood;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.eclipse.rdf4j.sparqlbuilder.core.OrderCondition;
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

import uk.ac.cam.cares.jps.agent.flood.objects.Station;
import uk.ac.cam.cares.jps.agent.flood.sparqlbuilder.ServicePattern;
import uk.ac.cam.cares.jps.agent.flood.sparqlbuilder.ValuesPattern;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * contains a collection of methods to query and update the KG
 * @author Kok Foong Lee
 *
 */
public class FloodSparql {
    private StoreClientInterface storeClient;
    
    // prefix
	private static String ontoems = "http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl#";
    private static Prefix p_ems = SparqlBuilder.prefix("ems",iri(ontoems));
    private static Prefix p_time = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
    private static Prefix p_geo = SparqlBuilder.prefix("geo",iri("http://www.bigdata.com/rdf/geospatial#"));
	private static Prefix p_om = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    
    // classes
	private static Iri ReportingStation = p_ems.iri("ReportingStation");
    private static Iri Instant = p_time.iri("Instant");

	// subclass of Quantity
	private static Iri WaterLevel = p_ems.iri("WaterLevel");
	private static Iri Rainfall = p_ems.iri("Rainfall");
	private static Iri WaterFlow = p_ems.iri("WaterFlow");
	private static Iri AirTemperature = p_ems.iri("AirTemperature");
	private static Iri WindSpeed = p_ems.iri("WindSpeed");
	private static Iri WindDirection = p_ems.iri("WindDirection");
	private static Iri WetBulbTemperature = p_ems.iri("WetBulbTemperature");
	
	private static Iri Measure = p_om.iri("Measure");

    // properties
	private static Iri hasObservationLocation = p_ems.iri("hasObservationLocation");
	private static Iri hasObservationElevation = p_ems.iri("hasObservationElevation");
	private static Iri dataSource = p_ems.iri("dataSource");
	private static Iri hasValue = p_om.iri("hasValue");
	private static Iri reports = p_ems.iri("reports");
    private static Iri measures = iri("http://environment.data.gov.uk/flood-monitoring/def/core/measures");
    private static Iri hasTime = p_time.iri("hasTime");
    private static Iri inXSDDate = p_time.iri("inXSDDate");
    private static Iri stationReference = iri("http://environment.data.gov.uk/flood-monitoring/def/core/stationReference");
    private static Iri lat_lon = iri("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon");
    // made up by KFL, purely for mapbox requirement
    private static Iri hasVisID = iri("http://environment.data.gov.uk/flood-monitoring/def/core/visID"); 
    
    private static Iri unitName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/unitName");
    private static Iri parameterName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/parameterName");
	private static Iri qualifier = iri("http://environment.data.gov.uk/flood-monitoring/def/core/qualifier");

	private static Iri lat_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
	private static Iri lon_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#long");

    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(FloodSparql.class);
    
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
		Variable lat = query.var();
		Variable lon = query.var();
		
		GraphPattern stationPattern = query.var().has(RDF.FIRST, station);
		GraphPattern stationPropertiesPattern = station.has(measures, measure).andHas(lat_prop,lat).andHas(lon_prop,lon);
		GraphPattern measurePropertiesPattern = measure.has(parameterName,param).andHas(qualifier,qual);
		GraphPattern queryPattern = GraphPatterns.and(stationPattern, stationPropertiesPattern, measurePropertiesPattern);
		
		query.where(queryPattern).select(station,measure,param,qual,lat,lon).distinct();
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		Map<String, Station> station_map = new HashMap<>(); // iri to station object map
		List<Station> stations = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String stationIri = queryResult.getJSONObject(i).getString(station.getQueryString().substring(1));
			String measureIri = queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1));
    		String measureName = queryResult.getJSONObject(i).getString(param.getQueryString().substring(1));
    		String subTypeName = queryResult.getJSONObject(i).getString(qual.getQueryString().substring(1));
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
	 * replace original triple <station> <measures> <measure> with
	 * <station> <reports> <quantity>, <quantity> <hasValue> <measure>
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
		
		query.select(measure).where(queryPattern).prefix(p_ems);
		
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
	 * original data has lat and lon on different triples
	 * Blazegraph requires them to be in the form of lat#lon
	 * visID is purely for visualisation purpose
	 */
	void addBlazegraphCoordinatesAndVisID(List<Station> stations) {
		ModifyQuery modify = Queries.MODIFY();
		modify.prefix(p_ems);
		int visID = 0;
		// one triple per station
		for (Station station : stations) {
			// blazegraph's custom literal type
			String latlon = String.valueOf(station.getLat()) + "#" + String.valueOf(station.getLon());
			StringLiteral coordinatesLiteral = Rdf.literalOfType(latlon, lat_lon);
			modify.insert(iri(station.getIri()).has(hasObservationLocation,coordinatesLiteral));
			modify.insert(iri(station.getIri()).has(hasVisID,visID + 1));
		}
		
		storeClient.executeUpdate(modify.getQueryString());
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
			case "Flow":
			    quantityIri = iri(station + "/Flow");
				modify.insert(quantityIri.isA(WaterFlow));
			case "Rainfall":
			    quantityIri = iri(station + "/Rainfall");
				modify.insert(quantityIri.isA(Rainfall));
			case "Wind":
				if (qual.contentEquals("Direction")) {
					quantityIri = iri(station + "/WindDirection");
					modify.insert(quantityIri.isA(WindDirection));
				} else if (qual.contentEquals("Speed")) {
					quantityIri = iri(station + "/WindSpeed");
					modify.insert(quantityIri.isA(WindSpeed));
				}
			case "Temperature":
			    if (qual.contentEquals("Wet Bulb")) {
					quantityIri = iri(station + "/WetBulbTemperature");
					modify.insert(quantityIri.isA(WetBulbTemperature));
				} else {
					quantityIri = iri(station + "/Temperature");
					modify.insert(quantityIri.isA(AirTemperature));
				}
		}

		modify.insert(iri(station).has(reports,quantityIri));

		modify.insert(quantityIri.has(hasValue, iri(measure)));
		modify.insert(iri(measure).has(unitName, unit)
				.andHas(parameterName, paramName)
				.andHas(qualifier,qual)
				.andIsA(Measure));
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
	
	/**
	 * adds triples to say the database contains data of this date
	 * @param date
	 */
	void addUpdateDate(LocalDate date) {
		Iri stations = iri("http://environment.data.gov.uk/flood-monitoring/id/stations");
		Iri instant = iri("http://environment.data.gov.uk/flood-monitoring/id/stations/time");
		
		ModifyQuery modify = Queries.MODIFY();
		modify.insert(stations.has(hasTime,instant));
		modify.insert(instant.isA(Instant).andHas(inXSDDate, Rdf.literalOfType(date.toString(), XSD.DATE)));
		modify.prefix(p_time);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	LocalDate getLatestUpdate() {
		Iri stations = iri("http://environment.data.gov.uk/flood-monitoring/id/stations");
		SelectQuery query = Queries.SELECT();
        Variable instant = query.var();
        Variable date = query.var();
		
		GraphPattern queryPattern = GraphPatterns.and(stations.has(hasTime,instant),
				instant.has(inXSDDate, date));
		
		// descending date
		OrderCondition dateDesc = SparqlBuilder.desc(date);
		
		query.select(date).prefix(p_time).where(queryPattern).orderBy(dateDesc).limit(1);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		try {
			String latestDate = queryResult.getJSONObject(0).getString(date.getQueryString().substring(1));
			return LocalDate.parse(latestDate);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			LOGGER.error("Failed to query latest update date");
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * returns true if provided data on the given date exists
	 * @param date
	 * @return
	 */
	boolean checkUpdateDateExists(LocalDate date) {
		Iri stations = iri("http://environment.data.gov.uk/flood-monitoring/id/stations");
		SelectQuery query = Queries.SELECT();
		Variable instant = query.var();
		
		GraphPattern queryPattern = GraphPatterns.and(stations.has(hasTime,instant),
				instant.has(inXSDDate, Rdf.literalOfType(date.toString(), XSD.DATE)));
		
		query.prefix(p_time).where(queryPattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() == 1)
			return true;
		else {
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
		Variable id = query.var();
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
				.andHas(lon_prop,lon).andHas(stationReference,ref).andHas(hasVisID, id).andHas(PropertyPaths.path(reports,hasValue), measure));
		
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
	    	query.where(queryPattern,geoPattern,stationProperties,measurePropertiesPattern).prefix(p_geo,p_ems);
		} else {
			query.where(queryPattern,stationProperties,measurePropertiesPattern).prefix(p_ems);
		}
		
		query.select(station,lat,lon,ref,id,river,catchment,town,dateOpened,label,measure,param,qual,unit);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
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
    			stationObject.setVisId(queryResult.getJSONObject(i).getInt(id.getQueryString().substring(1)));
    			
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
    	
    	GraphPattern queryPattern = iri(station).isA(ReportingStation);
    	
    	query.prefix(p_ems).where(queryPattern);
    	
	    if(storeClient.executeQuery(query.getQueryString()).length() == 1) {
	    	return true;
	    } else {
	    	return false;
	    }
    }
    
    void addNewStation(String station, double lat, double lon, String name) {
    	ModifyQuery modify = Queries.MODIFY();
    	Iri station_iri = iri(station);
    	
    	// blazegraph coordinates
    	String blazegraph_latlon = String.valueOf(lat) + "#" + String.valueOf(lon);
    	StringLiteral coordinatesLiteral = Rdf.literalOfType(blazegraph_latlon, 
    			lat_lon);
    	modify.insert(station_iri.has(hasObservationLocation,coordinatesLiteral));
    	
    	modify.insert(station_iri.isA(ReportingStation));
    	modify.insert(station_iri.has(stationReference, name));
    	modify.insert(station_iri.has(hasVisID, getNumID()+1));
    	modify.prefix(p_ems);
    	
    	storeClient.executeUpdate(modify.getQueryString());
    }
    
    /**
     * returns number of IDs currently in the kg, to generate a unique ID
     */
    int getNumID() {
    	SelectQuery query = Queries.SELECT();
    	
    	GraphPattern queryPattern = query.var().has(hasVisID,query.var());
    	
    	query.where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	return queryResult.length();
    }
}
