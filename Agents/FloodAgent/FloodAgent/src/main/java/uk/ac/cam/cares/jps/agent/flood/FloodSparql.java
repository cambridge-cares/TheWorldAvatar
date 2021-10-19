package uk.ac.cam.cares.jps.agent.flood;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.XSD;
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
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

/**
 * contains a collection of methods to query and update the KG
 * @author Kok Foong Lee
 *
 */
public class FloodSparql {
    private StoreClientInterface storeClient;
    
    // prefix
 	private static String ontostation = "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontostation/OntoStation.owl#";
    private static Prefix p_station = SparqlBuilder.prefix("station",iri(ontostation));
    private static Prefix p_time = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
    
    // classes
    private static Iri Station = p_station.iri("Station");
    private static Iri Instant = p_time.iri("Instant");
	
    // properties
    private static Iri hasCoordinates = p_station.iri("hasCoordinates");
    private static Iri measures = iri("http://environment.data.gov.uk/flood-monitoring/def/core/measures");
    private static Iri hasTime = p_time.iri("hasTime");
    private static Iri inXSDDate = p_time.iri("inXSDDate");
    private static Iri stationReference = iri("http://environment.data.gov.uk/flood-monitoring/def/core/stationReference");
    // made up by KFL, purely for mapbox requirement
    private static Iri hasVisID = iri("http://environment.data.gov.uk/flood-monitoring/def/core/visID"); 
    
    private static Iri unitName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/unitName");
    private static Iri parameterName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/parameterName");
	private static Iri qualifier = iri("http://environment.data.gov.uk/flood-monitoring/def/core/qualifier");
    
    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(FloodSparql.class);
    
	public FloodSparql(StoreClientInterface storeClient) {
		this.storeClient = storeClient;
	}
	
	/**
	 * returns a list of stations
	 * it is assumed that there is only one RDF collection in the namespace
	 * a good illustration of how an RDF collection look like 
	 * http://www-kasm.nii.ac.jp/~koide/SWCLOS2/Manual/07RDFCollection.htm
	 * @return
	 */
	List<String> getStations() {
		SelectQuery query = Queries.SELECT();
		
		Variable station = query.var();
		
		GraphPattern queryPattern = query.var().has(RDF.FIRST, station);
		
		query.select(station).where(queryPattern);
		
	    @SuppressWarnings("unchecked")
		List<String> stations = storeClient.executeQuery(query.getQueryString()).toList().stream()
	    .map(stationiri -> ((HashMap<String,String>) stationiri).get(station.getQueryString().substring(1))).collect(Collectors.toList());
	    
	    return stations;
	}
	
	void addStationRdfType(List<String> stations) {
		ModifyQuery modify = Queries.MODIFY();
		
		for (String station : stations) {
			modify.insert(iri(station).isA(Station));
		}
		
		modify.prefix(p_station);
		
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
		
		Iri measures = iri("http://environment.data.gov.uk/flood-monitoring/def/core/measures");
		
		GraphPattern queryPattern = station.has(measures, measure)
				.andHas(hasCoordinates, coord);
		
		query.select(measure).where(queryPattern).prefix(p_station);
		
	    @SuppressWarnings("unchecked")
		List<String> stations = storeClient.executeQuery(query.getQueryString()).toList().stream()
	    .map(datairi -> ((HashMap<String,String>) datairi).get(measure.getQueryString().substring(1))).collect(Collectors.toList());
	    
	    return stations;
	}
	
	/**
	 * original data has lat and lon on different triples
	 * Blazegraph requires them to be in the form of lat#lon
	 * visID is purely for visualisation purpose
	 */
	void addBlazegraphCoordinatesAndVisID() {
		Iri lat_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
		Iri lon_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#long");
		
		// first query both lat and lon for each station
		SelectQuery query = Queries.SELECT();
		
		Variable station = query.var();
		Variable lat = query.var();
		Variable lon = query.var();
		
		GraphPattern queryPattern = GraphPatterns.and(station.has(lat_prop,lat)
				.andHas(lon_prop,lon));
		
		query.where(queryPattern).select(station,lat,lon);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		// then add the combined literal and upload it
		List<String> latlon = new ArrayList<>(queryResult.length());
		List<String> stations = new ArrayList<>(queryResult.length());
		List<Integer> visID = new ArrayList<>(queryResult.length());
		
		for (int i = 0; i < queryResult.length(); i++) {
			latlon.add(i,queryResult.getJSONObject(i).getString(lat.getQueryString().substring(1)) +
					"#" + queryResult.getJSONObject(i).getString(lon.getQueryString().substring(1)));
			stations.add(i, queryResult.getJSONObject(i).getString(station.getQueryString().substring(1)));
			visID.add(i,i);
		}
		
		ModifyQuery modify = Queries.MODIFY();
		modify.prefix(p_station);
		// one triple per station
		for (int i = 0; i < queryResult.length(); i++) {
			// blazegraph's custom literal type
			StringLiteral coordinatesLiteral = Rdf.literalOfType(latlon.get(i), iri("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon"));
			modify.insert(iri(stations.get(i)).has(hasCoordinates,coordinatesLiteral));
			modify.insert(iri(stations.get(i)).has(hasVisID,visID.get(i)));
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
		modify.insert(iri(station).has(measures,iri(measure)));
		modify.insert(iri(measure).has(unitName, unit)
				.andHas(parameterName, paramName)
				.andHas(qualifier,qual));
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
		Variable measure = query.var();
		Variable timeseries = query.var();
		
		Iri hasTimeSeries = iri(TimeSeriesSparql.ns_ontology + "hasTimeSeries");
		
		GraphPattern queryPattern = GraphPatterns.and(station.has(measures, measure).andIsA(Station),
				measure.has(hasTimeSeries,timeseries));
		
		query.prefix(p_station).where(queryPattern).limit(10);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() >= 1) {
			return true;
		} else {
			return false;
		}
	}
	
	/**
	 * Initialise last update date with an empty string
	 * @param date
	 */
	void addLastDate() {
		Iri stations = iri("http://environment.data.gov.uk/flood-monitoring/id/stations");
		Iri instant = iri("http://environment.data.gov.uk/flood-monitoring/id/stations/time");
		
		ModifyQuery modify = Queries.MODIFY();
		modify.insert(stations.has(hasTime,instant));
		// set an arbitrary old date (10 years ago)
		modify.insert(instant.isA(Instant).andHas(inXSDDate, Rdf.literalOfType(LocalDate.now().minusYears(10).toString(), XSD.DATE)));
		modify.prefix(p_time);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * update the last updated date
	 * @param newdate
	 */
	void updateLastDate(LocalDate newdate) {
		ModifyQuery modify = Queries.MODIFY();
		Variable instant = SparqlBuilder.var("instant");
		Variable olddate = SparqlBuilder.var("date");
		
		TriplePattern delete_tp = instant.has(inXSDDate,olddate);
		TriplePattern insert_tp = instant.has(inXSDDate, Rdf.literalOfType(newdate.toString(), XSD.DATE));
		
		modify.insert(insert_tp).delete(delete_tp).prefix(p_time).where(delete_tp);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * returns the last updated date for the data set
	 * @return
	 */
	LocalDate getLastDate() {
		SelectQuery query = Queries.SELECT();
		Variable instant = query.var();
		Variable date = query.var();
		
		GraphPattern queryPattern = instant.has(inXSDDate,date);
		
		query.prefix(p_time).select(date).where(queryPattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		LocalDate queriedDate = LocalDate.parse(queryResult.getJSONObject(0).getString(date.getQueryString().substring(1)));
		
		return queriedDate;
	}
	
	/**
	 * station with its lat/lon in 3 lists
	 * index 1 = station name (List<String>), 2 = lat (List<Double>), 3 = lon (List<Double>)
	 */
	List<List<?>> getStationsWithCoordinates() {
		Iri lat_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
		Iri lon_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#long");
		
		List<String> stations = new ArrayList<>();
		List<Double> latval = new ArrayList<>();
		List<Double> lonval = new ArrayList<>();
		
		SelectQuery query = Queries.SELECT();
		
		Variable lat = query.var();
		Variable lon = query.var();
		Variable station = query.var();
		Variable ref = query.var();
		
		GraphPattern queryPattern = GraphPatterns.and(station.has(lat_prop,lat)
				.andHas(lon_prop,lon),station.has(stationReference,ref).optional());
		
		query.where(queryPattern).select(lat,lon,ref);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		for (int i = 0; i < queryResult.length(); i++) {
			stations.add(queryResult.getJSONObject(i).getString(ref.getQueryString().substring(1)));
			latval.add(queryResult.getJSONObject(i).getDouble(lat.getQueryString().substring(1)));
			lonval.add(queryResult.getJSONObject(i).getDouble(lon.getQueryString().substring(1)));
		}
		
		List<List<?>> station_coord = Arrays.asList(stations,latval,lonval);
		
		return station_coord;
	}
    
    /**
     * list 1: data name, list 2: unit, list 3: vis ID
     * @param measures
     */
    List<List<?>> getMeasurePropertiesForVis(List<String> measure_list) {
    	
    	
    	List<String> measureName_list = Arrays.asList(new String[measure_list.size()]);
    	List<String> unit_list = Arrays.asList(new String[measure_list.size()]);
    	List<Integer> visID_list = Arrays.asList(new Integer[measure_list.size()]);
    	
    	for (int i = 0; i < measure_list.size(); i++) {
    		SelectQuery query = Queries.SELECT();
    		
    		// e.g. table name: Water Level (Tidal Level), param = Water Level,
        	// qual (param subtype) = Tidal Level
        	Variable param = query.var();
        	Variable qual = query.var();
        	
        	Variable unit = query.var();
        	Variable station = query.var();
        	Variable visID = query.var();
    		
    		Iri measureIRI = iri(measure_list.get(i));
    		
    		GraphPattern paramNamePattern = measureIRI.has(parameterName,param)
        			.andHas(qualifier,qual).optional();
    		
    		GraphPattern unitPattern = measureIRI.has(unitName, unit).optional();
    		
    		GraphPattern visIDPattern = GraphPatterns.and(station.has(measures, measureIRI).
    				andHas(hasVisID,visID)).optional();
    		
    		GraphPattern queryPattern = GraphPatterns.and(paramNamePattern, unitPattern, visIDPattern);
    		
    		query.select(param,qual,unit,visID).where(queryPattern);
    		
    		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    		
    		try {
    			String s_param = queryResult.getJSONObject(0).getString(param.getQueryString().substring(1));
            	String s_qual = queryResult.getJSONObject(0).getString(qual.getQueryString().substring(1));
        	    String measureName = s_param + " (" + s_qual + ")";
        	    
        	    String s_unit = queryResult.getJSONObject(0).getString(unit.getQueryString().substring(1));
        	    int id = queryResult.getJSONObject(0).getInt(visID.getQueryString().substring(1));
        	    
        	    // measures without these properties will be ignored
        	    // they are mostly stations without coordinates
        	    measureName_list.set(i, measureName);
    			unit_list.set(i, s_unit);
    			visID_list.set(i, id);
    		} catch (Exception e) {
    			LOGGER.error(e.getMessage());
    			LOGGER.error("Error in querying properties of " + measure_list.get(i));
    			LOGGER.error(queryResult);
    		}
    	}
    	
    	List<List<?>> combined_list = new ArrayList<>();
    	combined_list.add(measureName_list);
    	combined_list.add(unit_list);
    	combined_list.add(visID_list);
    	
    	return combined_list;
    }
    
    boolean checkStationExists(String station) {
    	SelectQuery query = Queries.SELECT();
    	
    	GraphPattern queryPattern = iri(station).isA(Station);
    	
    	query.prefix(p_station).where(queryPattern);
    	
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
    			iri("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon"));
    	modify.insert(station_iri.has(hasCoordinates,coordinatesLiteral));
    	
    	modify.insert(station_iri.isA(Station));
    	modify.insert(station_iri.has(stationReference, name));
    	modify.insert(station_iri.has(hasVisID, getNumID()+1));
    	modify.prefix(p_station);
    	
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
