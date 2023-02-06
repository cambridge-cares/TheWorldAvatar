package uk.ac.cam.cares.jps.agent.weather;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

class MockWeatherQueryClient extends WeatherQueryClient{
	
	MockWeatherQueryClient(RemoteStoreClient kgClient, TimeSeriesClient<Instant> tsClient, RemoteStoreClient ontopClient) {
		super(kgClient, tsClient, ontopClient);
	}

	@Override 
	void updateStation(String stationIri, String timestamp) {
		// construct time series object
		// first query all data values
		SelectQuery query = Queries.SELECT();
		Variable measure = query.var();
		GraphPattern queryPattern = iri(stationIri).has(PropertyPaths.path(reports,hasValue),measure);
		query.select(measure).where(queryPattern).prefix(p_ems,p_om);
		
		JSONArray queryResult = this.kgClient.executeQuery(query.getQueryString());
		List<String> datavalue_list = queryResult.toList().stream()
				.map(iri -> ((HashMap<String,String>) iri).get(measure.getQueryString().substring(1)))
				.collect(Collectors.toList()); 
		
		// add dummy data
		List<List<?>> value_list = new ArrayList<>();
		for (int i = 0; i < datavalue_list.size(); i++) {
			value_list.add(Arrays.asList(0));
		}
		
		// append new values to time series table
		TimeSeries<Instant> ts = new TimeSeries<Instant>(Arrays.asList(Instant.now()), datavalue_list, value_list);
		tsClient.addTimeSeriesData(ts);
	}

}
