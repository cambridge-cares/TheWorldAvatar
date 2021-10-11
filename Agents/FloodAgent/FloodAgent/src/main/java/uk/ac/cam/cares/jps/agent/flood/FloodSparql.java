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
	 * @return
	 */
	List<String> getMeasures() {
        SelectQuery query = Queries.SELECT();
		
		Variable measure = query.var();
		
		Iri measures = iri("http://environment.data.gov.uk/flood-monitoring/def/core/measures");
		
		GraphPattern queryPattern = query.var().has(measures, measure);
		
		query.select(measure).where(queryPattern);
		
	    @SuppressWarnings("unchecked")
		List<String> stations = storeClient.executeQuery(query.getQueryString()).toList().stream()
	    .map(datairi -> ((HashMap<String,String>) datairi).get(measure.getQueryString().substring(1))).collect(Collectors.toList());
	    
	    return stations;
	}
	
	/**
	 * original data has lat and lon on different triples
	 * Blazegraph requires them to be in the form of lat#lon
	 */
	void addBlazegraphCoordinates(List<String> stations) {
		Iri lat_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
		Iri lon_prop = iri("http://www.w3.org/2003/01/geo/wgs84_pos#long");
		
		int num_without_coordinates = 0;
		
		for (String station : stations) {
			// first query both lat and lon for each station
			SelectQuery query = Queries.SELECT();
			
			Variable lat = query.var();
			Variable lon = query.var();
			
			GraphPattern queryPattern = GraphPatterns.and(iri(station).has(lat_prop,lat)
					.andHas(lon_prop,lon));
			
			query.where(queryPattern).select(lat,lon);
			
			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
			
			// then add the combined literal and upload it
			String latlon;
			try { 
				// some stations don't have coordinates?
				latlon = queryResult.getJSONObject(0).getString(lat.getQueryString().substring(1)) +
						"#" + queryResult.getJSONObject(0).getString(lon.getQueryString().substring(1));
			} catch (Exception e) {
				num_without_coordinates += 1;
				LOGGER.error(e.getMessage());
				LOGGER.error("<" + station + "> does not have coordinates");
				continue;
			}
			
			ModifyQuery modify = Queries.MODIFY();
			
			// blazegraph's custom literal type
			StringLiteral coordinatesLiteral = Rdf.literalOfType(latlon, iri("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon"));
			TriplePattern insert_tp = iri(station).has(hasCoordinates,coordinatesLiteral);
			
			modify.insert(insert_tp).prefix(p_station);
			
			storeClient.executeUpdate(modify.getQueryString());
		}
		LOGGER.info(Integer.toString(num_without_coordinates) + " stations do not have coordinates");
	}
	
	/**
	 * add a measure that was not present in the initial RDF file, but present
	 * in the data downloaded later
	 * adds a triple <station> <measures> <measure>
	 * @param station
	 * @param measure
	 */
	void addMeasureToStation(String station, String measure) {
		ModifyQuery modify = Queries.MODIFY();
		modify.insert(iri(station).has(measures,iri(measure)));
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
	 * returns station name that measures this quantity
	 * @param measure
	 * @return
	 */
	String getStationNameFromMeasure(String measure) {
	    SelectQuery query = Queries.SELECT();
	    Variable station = query.var();
	    Variable stationRef = query.var();
	    
	    GraphPattern queryPattern = station.has(measures, iri(measure)).andHas(stationReference,stationRef);
	    
	    query.select(stationRef).where(queryPattern);
	    
	    return storeClient.executeQuery(query.getQueryString())
	    .getJSONObject(0).getString(stationRef.getQueryString().substring(1));
	}
	
	/**
	 * returns units of this measure
	 * @param measure
	 * @return
	 */
	String getUnitOfMeasure(String measure) {
		Iri unitName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/unitName");
		
		// construct query
		SelectQuery query = Queries.SELECT();
		Variable unit = query.var();
		query.select(unit).where(iri(measure).has(unitName,unit));
		
		String unitstring;
		try {
    		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    		unitstring = queryResult.getJSONObject(0)
    				.getString(unit.getQueryString().substring(1));
		} catch (Exception e) {
			unitstring = "";
		}
		
		return unitstring;
	}
	
    String getMeasureName(String measure) {
    	Iri parameterName = iri("http://environment.data.gov.uk/flood-monitoring/def/core/parameterName");
    	Iri qualifier = iri("http://environment.data.gov.uk/flood-monitoring/def/core/qualifier");
    	
    	// construct query
    	SelectQuery query = Queries.SELECT();
    	Variable param = query.var();
    	Variable qual = query.var();
    	GraphPattern queryPattern = iri(measure).has(parameterName,param)
    			.andHas(qualifier,qual);
    	
    	query.select(param,qual).where(queryPattern);
    	
    	String measureName;
    	
    	try {
    		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        	
        	String s_param = queryResult.getJSONObject(0).getString(param.getQueryString().substring(1));
        	String s_qual = queryResult.getJSONObject(0).getString(qual.getQueryString().substring(1));
    	    measureName = s_param + " (" + s_qual + ")";
    	} catch (Exception e) {
    	    measureName = measure;	
    	}
    	
    	return measureName ;
    }
}
