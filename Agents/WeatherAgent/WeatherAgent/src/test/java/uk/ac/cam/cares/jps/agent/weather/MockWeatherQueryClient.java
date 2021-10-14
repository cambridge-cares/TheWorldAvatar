package uk.ac.cam.cares.jps.agent.weather;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

class MockWeatherQueryClient extends WeatherQueryClient{

	MockWeatherQueryClient(StoreClientInterface storeClient) {
		super(storeClient);
	}
	
	MockWeatherQueryClient(StoreClientInterface storeClient, TimeSeriesClient<Long> tsClient) {
		super(storeClient, tsClient);
	}

	@Override 
	void updateStation(String station_iri) {
		// construct time series object
		// first query all data values
		SelectQuery query = Queries.SELECT();
		Variable datavalue = query.var();
		Iri[] predicates = {hasSubsystem,observes,hasValue};
		GraphPattern queryPattern = getQueryGraphPattern(query, predicates, iri(station_iri), datavalue);
		query.select(datavalue).where(queryPattern).prefix(p_ontosensor,p_system);
		
		JSONArray queryResult = this.storeClient.executeQuery(query.getQueryString());
		List<String> datavalue_list = queryResult.toList().stream()
				.map(iri -> ((HashMap<String,String>) iri).get(datavalue.getQueryString().substring(1)))
				.collect(Collectors.toList()); 
		
		// add dummy data
		List<List<?>> value_list = new ArrayList<>();
		for (int i = 0; i < datavalue_list.size(); i++) {
			value_list.add(Arrays.asList(0));
		}
		
		// append new values to time series table
		TimeSeries<Long> ts = new TimeSeries<Long>(Arrays.asList(Instant.now().getEpochSecond()), datavalue_list, value_list);
		tsClient.addTimeSeriesData(ts);
		
		// update last updated timestamp
		DerivationClient dClient = new DerivationClient(storeClient);
		dClient.updateTimestamp(station_iri);
	}

}
