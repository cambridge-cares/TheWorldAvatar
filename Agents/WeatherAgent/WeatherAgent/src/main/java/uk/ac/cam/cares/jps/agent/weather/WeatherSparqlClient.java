package uk.ac.cam.cares.jps.agent.weather;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.StringLiteral;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

class WeatherSparqlClient {
	// prefix
	private static Prefix p_geoliteral = SparqlBuilder.prefix(iri("http://www.bigdata.com/rdf/geospatial/literals/v1#"));
	private static String ontostation = "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontostation/OntoStation.owl#";
    private static Prefix p_station = SparqlBuilder.prefix("station",iri(ontostation));
    private static Prefix p_ontosensor = SparqlBuilder.prefix("sensor",iri("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#"));
    private static Prefix p_system = SparqlBuilder.prefix("system",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"));
    private static Prefix p_derived_SI_unit = SparqlBuilder.prefix("derived_SI_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#"));
    
    // classes
    private static Iri WeatherStation = p_station.iri("WeatherStation");
    private static Iri latlon = p_geoliteral.iri("lat-lon");
    private static Iri MeasuringInstrument = iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_system.owl#MeasuringInstrument");
    private static Iri ScalarValue = p_system.iri("ScalarValue");
    private static Iri OutsideAirCloudCover = p_ontosensor.iri("OutsideAirCloudCover");
    private static Iri OutsideAirPrecipitation = p_ontosensor.iri("OutsideAirPrecipitation");
    private static Iri OutsideAirPressure = p_ontosensor.iri("OutsideAirPressure");
    private static Iri OutsideAirTemperature = p_ontosensor.iri("OutsideAirTemperature");
    private static Iri OutsideAirRelativeHumidity = p_ontosensor.iri("OutsideAirRelativeHumidity");
    private static Iri OutsideWindSpeed = p_ontosensor.iri("OutsideWindSpeed");
    private static Iri OutsideWindDirection = p_ontosensor.iri("OutsideWindDirection");
    
    // properties
    private static Iri hasCoordinates = p_station.iri("hasCoordinates");
    private static Iri observes = p_ontosensor.iri("observes");
    private static Iri hasValue = p_system.iri("hasValue");
    private static Iri numericalValue = p_system.iri("numericalValue");
    private static Iri hasUnitOfMeasure = p_system.iri("hasUnitOfMeasure");
    
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
	
    WeatherSparqlClient(StoreClientInterface storeClient) {
    	this.storeClient = storeClient;
    }
    
    void createStation(double x, double y) {
    	ModifyQuery modify = Queries.MODIFY();
    	
    	Iri station = p_station.iri("weatherstation_" + UUID.randomUUID());
    	
    	// coordinates
    	StringLiteral coordinatesLiteral = Rdf.literalOfType(String.valueOf(y)+"#"+String.valueOf(y), latlon);
    	modify.insert(station.isA(WeatherStation).andHas(hasCoordinates,coordinatesLiteral));
    	
    	// 1 sensor per property
    	for (Iri weatherClass : weatherClasses) {
    		Iri sensor = p_station.iri("sensor_" + UUID.randomUUID());
    		Iri data = p_station.iri("data_" + UUID.randomUUID());
    		Iri datavalue = p_station.iri("datavalue_" + UUID.randomUUID());
    		modify.insert(sensor.isA(MeasuringInstrument).andHas(observes,data));
    		modify.insert(data.isA(weatherClass).andHas(hasValue,datavalue));
    		modify.insert(datavalue.isA(ScalarValue).andHas(numericalValue,0).andHas(hasUnitOfMeasure,unitMap.get(weatherClass)));
    	}
    	
    	modify.prefix(p_geoliteral,p_station,p_system,p_ontosensor,p_derived_SI_unit);
    	storeClient.executeUpdate(modify.getQueryString());
    }
}
