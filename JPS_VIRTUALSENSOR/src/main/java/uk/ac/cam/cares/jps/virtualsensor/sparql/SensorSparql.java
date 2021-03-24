package uk.ac.cam.cares.jps.virtualsensor.sparql;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Assignment;
import org.eclipse.rdf4j.sparqlbuilder.core.From;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatternNotTriples;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.objects.WeatherStation;

import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;

/**
 * This class deals with all the interactions with the triple-store for weather and air quality stations
 * @author Kok Foong Lee
 *
 */

public class SensorSparql {
    // weather station properties
    public static final String cloud = "OutsideAirCloudCover";
    public static final String precipitation = "OutsideAirPrecipitation";
    public static final String pressure = "OutsideAirPressure";
    public static final String temperature = "OutsideAirTemperature";
    public static final String humidity = "OutsideAirRelativeHumidity";
    public static final String windspeed = "OutsideWindSpeed";
    public static final String winddirection = "OutsideWindDirection";

    // air quality properties
    public static final String CO2 = "OutsideCO2Concentration";
    public static final String CO = "OutsideCOConcentration";
    public static final String HC = "OutsideHCConcentration";
    public static final String NO2 = "OutsideNO2Concentration";
    public static final String NO = "OutsideNOConcentration";
    public static final String NOx =  "OutsideNOxConcentration";
    public static final String O3 = "OutsideO3Concentration";
    public static final String PM1 = "OutsidePM1Concentration";
    public static final String PM25 = "OutsidePM2.5Concentration";
    public static final String PM10 = "OutsidePM10Concentration";
    public static final String SO2 = "OutsideSO2Concentration";

    // prefixes for SPARQL queries
    private static String ontostation = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#";
    private static Prefix p_station = SparqlBuilder.prefix("station",iri(ontostation));
    private static Prefix p_space_time_extended = SparqlBuilder.prefix("space_time_extended",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"));
    private static Prefix p_space_time = SparqlBuilder.prefix("space_time",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#"));
    private static Prefix p_system = SparqlBuilder.prefix("system",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"));
    private static Prefix p_SI_unit = SparqlBuilder.prefix("si_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#"));
    private static Prefix p_derived_SI_unit = SparqlBuilder.prefix("derived_SI_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#"));
    private static Prefix p_ontosensor = SparqlBuilder.prefix("sensor",iri("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#"));
    private static Prefix p_time = SparqlBuilder.prefix("time",iri("http://www.w3.org/2006/time#"));
    private static Prefix p_coordsys = SparqlBuilder.prefix("coordsys",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#"));
    private static Prefix p_instrument = SparqlBuilder.prefix("instrument", iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#"));
    
    // type
    private static Iri WeatherStation = p_station.iri("WeatherStation");
    
    // IRI of units used
    private static Iri unit_m = p_SI_unit.iri("m");
    private static Iri unit_mm = p_derived_SI_unit.iri("mm");
    private static Iri unit_degree = p_derived_SI_unit.iri("degree");
    private static Iri unit_mbar = p_derived_SI_unit.iri("mBar");
    private static Iri unit_celcius = p_derived_SI_unit.iri("Celsius");
    private static Iri unit_ms = p_derived_SI_unit.iri("m_per_s");
    private static Iri unit_fraction = p_derived_SI_unit.iri("fraction"); // made up, does not exist in ontology
    private static Iri unit_percentage = p_derived_SI_unit.iri("percentage"); // made up, does not exist in ontology
    private static Iri unit_ugm3 = p_derived_SI_unit.iri("ug_per_m.m.m");

    //endpoint
    private static Iri weather_graph = p_station.iri("WeatherStations");
    private static Iri airquality_graph = p_station.iri("AirQualityStations");

    private static Prefix [] getPrefix() {
        Prefix [] prefixes = {p_station,p_space_time_extended,p_space_time,p_system,p_SI_unit,p_derived_SI_unit,p_ontosensor,p_time,p_coordsys,p_instrument};
        return prefixes;
    }

    /**
     * @param station_name
     * @param xyz_coord = x y coordinates are in EPSG:4326, z is the height in m 
     */
    public static void createWeatherStation(int index, double [] xyz_coord) {
    	String station_name = "weatherstation" + String.valueOf(index);
        Iri weatherstation_iri = p_station.iri(station_name);
        Iri stationcoordinates_iri = p_station.iri(station_name+"_coordinates");
        Iri time_iri = p_station.iri(station_name+"_time");

        ModifyQuery modify = Queries.MODIFY();
        
        TriplePattern weatherstation_tp = weatherstation_iri.isA(WeatherStation)
        		.andHas(p_space_time_extended.iri("hasGISCoordinateSystem"),stationcoordinates_iri);

        InsertCoordinatesTP(modify,stationcoordinates_iri,station_name,xyz_coord);

        // time stamp is shared by all properties, in unix timestamp
        TriplePattern time_tp = time_iri.isA(p_time.iri("Instant")).andHas(p_time.iri("inTimePosition"),0);

        InsertWeatherSensorTP(modify,weatherstation_iri,p_station,station_name,cloud,unit_percentage,time_iri);
        InsertWeatherSensorTP(modify,weatherstation_iri,p_station,station_name,precipitation,unit_mm,time_iri);
        InsertWeatherSensorTP(modify,weatherstation_iri,p_station,station_name,pressure,unit_mbar,time_iri);
        InsertWeatherSensorTP(modify,weatherstation_iri,p_station,station_name,temperature,unit_celcius,time_iri);
        InsertWeatherSensorTP(modify,weatherstation_iri,p_station,station_name,humidity,unit_fraction,time_iri);
        InsertWeatherSensorTP(modify,weatherstation_iri,p_station,station_name,windspeed,unit_ms,time_iri);
        InsertWeatherSensorTP(modify,weatherstation_iri,p_station,station_name,winddirection,unit_degree,time_iri);
        
        Prefix [] prefix_list = getPrefix();
        
        modify.prefix(prefix_list).insert(weatherstation_tp,time_tp).where().with(weather_graph);
        SparqlGeneral.performUpdate(modify);
    }

    private static void InsertWeatherSensorTP(ModifyQuery modify, Iri station_iri, Prefix station_prefix, String station_name, String data, Iri unit, Iri time_iri) {
        Iri sensor_iri = station_prefix.iri(station_name+"_sensor"+data);
        Iri data_iri = station_prefix.iri(station_name+"_"+data);
        Iri datavalue_iri = station_prefix.iri(station_name+"_v"+data);
        
        TriplePattern station_tp = station_iri.has(p_system.iri("hasSubsystem"),sensor_iri);
        TriplePattern sensor_tp = sensor_iri.isA(p_instrument.iri("Q-Sensor")).andHas(p_ontosensor.iri("observes"),data_iri);
        TriplePattern data_tp = data_iri.isA(p_ontosensor.iri(data)).andHas(p_system.iri("hasValue"),datavalue_iri);
        
        TriplePattern datavalue_tp = datavalue_iri.isA(p_system.iri("ScalarValue"))
                .andHas(p_system.iri("numericalValue"), 0)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit)
                .andHas(p_time.iri("hasTime"), time_iri);
        
        modify.insert(station_tp,sensor_tp,data_tp,datavalue_tp);
    }

    private static void InsertCoordinatesTP(ModifyQuery modify,Iri stationcoordinates_iri, String station_name, double [] xyz) {
    	Iri xcoord = p_station.iri(station_name+"_xcoord");
        Iri ycoord = p_station.iri(station_name+"_ycoord");
        Iri zcoord = p_station.iri(station_name+"_zcoord");
        Iri vxcoord = p_station.iri(station_name+"_vxcoord");
        Iri vycoord = p_station.iri(station_name+"_vycoord");
        Iri vzcoord = p_station.iri(station_name+"_vzcoord");
    	
    	TriplePattern projected_gp = stationcoordinates_iri.isA(p_space_time_extended.iri("ProjectedCoordinateSystem"))
                .andHas(p_space_time_extended.iri("hasProjectedCoordinate_x"),xcoord)
                .andHas(p_space_time_extended.iri("hasProjectedCoordinate_y"),ycoord)
                .andHas(p_space_time_extended.iri("hasProjectedCoordinate_z"),zcoord);

        TriplePattern xcoord_tp = xcoord.isA(p_space_time.iri("AngularCoordinate")).andHas(p_system.iri("hasValue"),vxcoord);
        TriplePattern ycoord_tp = ycoord.isA(p_space_time.iri("AngularCoordinate")).andHas(p_system.iri("hasValue"),vycoord);
        TriplePattern zcoord_tp = zcoord.isA(p_space_time.iri("StraightCoordinate")).andHas(p_system.iri("hasValue"),vzcoord);

        TriplePattern vxcoord_tp  = vxcoord.isA(p_coordsys.iri("CoordinateValue"))
        		.andHas(p_system.iri("numericalValue"), xyz[0]).andHas(p_system.iri("hasUnitOfMeasure"), unit_degree);
        TriplePattern vycoord_tp = vycoord.isA(p_coordsys.iri("CoordinateValue"))
                .andHas(p_system.iri("numericalValue"), xyz[1]).andHas(p_system.iri("hasUnitOfMeasure"), unit_degree);
        TriplePattern vzcoord_tp = vzcoord.isA(p_coordsys.iri("CoordinateValue"))
                .andHas(p_system.iri("numericalValue"), xyz[2]).andHas(p_system.iri("hasUnitOfMeasure"), unit_m);

        modify.insert(projected_gp,xcoord_tp,ycoord_tp,zcoord_tp,vxcoord_tp,vycoord_tp,vzcoord_tp);
    }
    
    /** 
     * updates weather station with new values
     * @param stationiri
     * @param cloud
     * @param precip
     * @param pres
     * @param temp
     * @param humid
     * @param windspeed
     * @param winddirection
     */
    public static void updateWeatherStation(WeatherStation ws) {
    	Iri stationiri = iri(ws.getStationiri());
    	modifyTimeStamp(stationiri,ws.getTimestamp(),weather_graph);
    	modifyWeatherData(stationiri,cloud,ws.getCloudcover());
    	modifyWeatherData(stationiri,precipitation,ws.getPrecipitation());
    	modifyWeatherData(stationiri,pressure,ws.getPressure());
    	modifyWeatherData(stationiri,temperature,ws.getTemperature());
    	modifyWeatherData(stationiri,humidity,ws.getHumidity());
    	modifyWeatherData(stationiri,windspeed,ws.getWindspeed());
    	modifyWeatherData(stationiri,winddirection,ws.getWinddirection());
    }
    
    private static void modifyWeatherData(Iri stationiri,String datatype,double newvalue) {
    	Variable datavalue_iri = SparqlBuilder.var("datavalue_iri");
    	Variable oldvalue = SparqlBuilder.var("oldvalue");
    	
    	SubSelect sub = GraphPatterns.select();
    	Variable sensor_iri = sub.var();
    	Variable data_iri = sub.var();
    	
    	TriplePattern station_tp = stationiri.has(p_system.iri("hasSubsystem"),sensor_iri);
        TriplePattern sensor_tp = sensor_iri.has(p_ontosensor.iri("observes"),data_iri);
        TriplePattern data_tp = data_iri.isA(p_ontosensor.iri(datatype)).andHas(p_system.iri("hasValue"),datavalue_iri);
        TriplePattern olddatavalue_tp = datavalue_iri.has(p_system.iri("numericalValue"), oldvalue);
        
        GraphPattern weatherdata_gp = GraphPatterns.and(station_tp,sensor_tp,data_tp,olddatavalue_tp);
    	TriplePattern newvalue_tp = datavalue_iri.has(p_system.iri("numericalValue"), newvalue);
   
    	sub.select(datavalue_iri,oldvalue).where(weatherdata_gp).from(weather_graph);
    	ModifyQuery modify = Queries.MODIFY();
    	modify.prefix(p_system,p_ontosensor,p_station).delete(olddatavalue_tp).insert(newvalue_tp).where(sub).with(weather_graph);
    	SparqlGeneral.performUpdate(modify);
    }
    
    /**
     * can be used by both weather and air quality stations
     */
    private static void modifyTimeStamp(Iri stationiri,long timestamp, Iri graph) {
    	// all properties share the same timestamp, so we just have to match the pattern for one property
    	SubSelect sub = GraphPatterns.select();
    	Variable time_iri = SparqlBuilder.var("time_iri");
    	Variable oldvalue = SparqlBuilder.var("oldvalue");
    	Variable sensor_iri = sub.var();
    	Variable data_iri = sub.var();
    	Variable datavalue_iri = sub.var();
    	
    	TriplePattern station_tp = stationiri.has(p_system.iri("hasSubsystem"),sensor_iri);
        TriplePattern sensor_tp = sensor_iri.has(p_ontosensor.iri("observes"),data_iri);
        TriplePattern data_tp = data_iri.has(p_system.iri("hasValue"), datavalue_iri); 
        TriplePattern datavalue_tp = datavalue_iri.has(p_time.iri("hasTime"), time_iri);
        TriplePattern oldtime_tp = time_iri.has(p_time.iri("inTimePosition"), oldvalue);
        TriplePattern newtime_tp = time_iri.has(p_time.iri("inTimePosition"), timestamp);
        
        GraphPattern subquerypattern = GraphPatterns.and(station_tp,sensor_tp,data_tp,datavalue_tp,oldtime_tp);
        sub.select(time_iri,oldvalue).where(subquerypattern).distinct().from(graph);
        
        ModifyQuery modify = Queries.MODIFY();
        modify.prefix(p_system,p_ontosensor,p_time,p_station).delete(oldtime_tp).insert(newtime_tp).where(sub).with(weather_graph);
        SparqlGeneral.performUpdate(modify);
    }
    
    /**
     * returns all properties of a weather station
     * @param stationiri_string
     * @return
     */
    public static JSONObject queryWeatherStationProperties(String stationiri_string) {
    	SelectQuery query = Queries.SELECT();
    	
    	JSONObject result = new JSONObject();
    	Iri stationiri = iri(stationiri_string);
    	
    	Variable xval = SparqlBuilder.var("xval");
    	Variable yval = SparqlBuilder.var("yval");
    	Variable zval = SparqlBuilder.var("zval");
    	
    	Variable [] xyzval = {xval,yval,zval};
    	
    	Variable vcloud = SparqlBuilder.var("vcloud");
    	Variable vprecip = SparqlBuilder.var("vprecip");
    	Variable vpressure = SparqlBuilder.var("vpressure");
    	Variable vtemp = SparqlBuilder.var("vtemp");
    	Variable vhumidity = SparqlBuilder.var("vhumidity");
    	Variable vwindspeed = SparqlBuilder.var("vwindspeed");
    	Variable vwinddirection = SparqlBuilder.var("vwinddirection");
    	Variable vtime = SparqlBuilder.var("vtime");
    	
    	GraphPattern cloud_gp = getWeatherDataGP(stationiri,query,vcloud,cloud);
    	GraphPattern precip_gp = getWeatherDataGP(stationiri,query,vprecip,precipitation);
    	GraphPattern pressure_gp = getWeatherDataGP(stationiri,query,vpressure,pressure);
    	GraphPattern temp_gp = getWeatherDataGP(stationiri,query,vtemp,temperature);
    	GraphPattern humidity_gp = getWeatherDataGP(stationiri,query,vhumidity,humidity);
    	GraphPattern windspeed_gp = getWeatherDataGP(stationiri,query,vwindspeed,windspeed);
    	GraphPattern winddirection_gp = getWeatherDataGP(stationiri,query,vwinddirection,winddirection);
    	
    	GraphPattern data_gp = GraphPatterns.and(cloud_gp,precip_gp,pressure_gp,temp_gp,humidity_gp,windspeed_gp,winddirection_gp);
    	GraphPattern time_gp = getStationTimeGP(query,stationiri,vtime);
        GraphPattern coordinates_gp = getStationCoordinatesGP(query,stationiri,xyzval);
        
        From queryGraph = SparqlBuilder.from(weather_graph);
        
        GraphPattern querypattern = GraphPatterns.and(data_gp,coordinates_gp,time_gp);
        Variable [] queryvariables = {xval,yval,zval,vcloud,vprecip,vpressure,vtemp,vhumidity,vwindspeed,vwinddirection,vtime};
        // distinct used here purely because time triple is shared by all data, hence giving multiple results
        query.from(queryGraph).prefix(getPrefix()).select(queryvariables).where(querypattern).distinct();
        JSONArray queryresult = SparqlGeneral.performQuery(query);
        result = queryresult.getJSONObject(0);
    	return result;
    }
    
    private static GraphPattern getWeatherDataGP(Iri stationiri, SelectQuery query, Variable dataval, String datatype) {
    	Variable sensor_iri = query.var();
    	Variable data_iri = query.var();
    	Variable datavalue_iri = query.var();
    	
    	TriplePattern station_tp = stationiri.has(p_system.iri("hasSubsystem"),sensor_iri);
        TriplePattern sensor_tp = sensor_iri.has(p_ontosensor.iri("observes"),data_iri);
        TriplePattern data_tp = data_iri.isA(p_ontosensor.iri(datatype)).andHas(p_system.iri("hasValue"),datavalue_iri);
        TriplePattern datavalue_tp = datavalue_iri.has(p_system.iri("numericalValue"), dataval);
        
        GraphPattern weatherdata_gp = GraphPatterns.and(station_tp,sensor_tp,data_tp,datavalue_tp);
        return weatherdata_gp;
    }
    
    private static GraphPattern getStationCoordinatesGP(SelectQuery query, Iri stationiri, Variable [] xyzval) {
    	// properties we don't need
    	Variable coord = query.var();
        Variable xcoord = query.var();
        Variable ycoord = query.var();
        Variable zcoord = query.var();
        Variable vxcoord = query.var();
        Variable vycoord = query.var();
        Variable vzcoord = query.var();
    	
        GraphPattern station_gp = stationiri.has(p_space_time_extended.iri("hasGISCoordinateSystem"),coord);
        GraphPattern projected_gp = coord.has(p_space_time_extended.iri("hasProjectedCoordinate_x"),xcoord)
        		.andHas(p_space_time_extended.iri("hasProjectedCoordinate_y"),ycoord)
        		.andHas(p_space_time_extended.iri("hasProjectedCoordinate_z"),zcoord);
        GraphPattern coord_gp = GraphPatterns.and(xcoord.has(p_system.iri("hasValue"),vxcoord), 
        		ycoord.has(p_system.iri("hasValue"),vycoord),
        		zcoord.has(p_system.iri("hasValue"),vzcoord));
        GraphPattern vcoord_gp = GraphPatterns.and(vxcoord.has(p_system.iri("numericalValue"), xyzval[0]),
        		vycoord.has(p_system.iri("numericalValue"), xyzval[1]),
        		vzcoord.has(p_system.iri("numericalValue"), xyzval[2]));
    	
        GraphPattern coordinates_gp = GraphPatterns.and(station_gp,projected_gp,coord_gp,vcoord_gp);
        
        return coordinates_gp;
    }
    
    private static GraphPattern getStationTimeGP(SelectQuery query, Iri stationiri, Variable vtime) {
    	Variable time_iri = query.var();
    	Variable sensor_iri = query.var();
    	Variable data_iri = query.var();
    	Variable datavalue_iri = query.var();
    	
    	TriplePattern station_tp = stationiri.has(p_system.iri("hasSubsystem"),sensor_iri);
        TriplePattern sensor_tp = sensor_iri.has(p_ontosensor.iri("observes"),data_iri);
        // all data properties have the same time stamp, specifying cloud ensures that there's only 1 query result, making it less confusing
        TriplePattern data_tp = data_iri.has(p_system.iri("hasValue"), datavalue_iri); 
        TriplePattern datavalue_tp = datavalue_iri.has(p_time.iri("hasTime"), time_iri);
        TriplePattern vtime_tp = time_iri.has(p_time.iri("inTimePosition"), vtime);
        
        GraphPattern time_gp = GraphPatterns.and(station_tp,sensor_tp,data_tp,datavalue_tp,vtime_tp);
        return time_gp;
    }
    
    public static String createAirQualityStation(String station_name, double [] xyz_coord) {
    	Iri airqualitystation_iri = p_station.iri(station_name);
        Iri stationcoordinates_iri = p_station.iri(station_name+"_coordinates");

        ModifyQuery modify = Queries.MODIFY();
        
        TriplePattern airqualitystation_tp = airqualitystation_iri.isA(p_station.iri("AirQualityStation"))
        		.andHas(p_space_time_extended.iri("hasGISCoordinateSystem"),stationcoordinates_iri);

        InsertCoordinatesTP(modify,stationcoordinates_iri,station_name,xyz_coord);

        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,CO2,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,CO,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,HC,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,NO2,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,NO,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,NOx,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,O3,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,PM1,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,PM25,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,PM10,0.0);
        InsertAirQualitySensorTP(modify,airqualitystation_iri,p_station,station_name,SO2,0.0);
        
        Prefix [] prefix_list = getPrefix();
        
        modify.prefix(prefix_list).insert(airqualitystation_tp).where().with(airquality_graph);
        SparqlGeneral.performUpdate(modify);
        //airqualitystation_iri.toString() does not work, hence this awkward solution
        return ontostation + station_name;
    }
    
    public static void InsertAirQualitySensorTP(ModifyQuery modify, Iri station_iri, Prefix station_prefix, String station_name, String data, double value) {
    	Iri sensor_iri = station_prefix.iri(station_name+"_sensor"+data);
        Iri data_iri = station_prefix.iri(station_name+"_"+data);
        Iri datavalue_iri = station_prefix.iri(station_name+"_v"+data);
//        Iri time_iri = station_prefix.iri(station_name+"_time"+data);
        
        TriplePattern station_tp = station_iri.has(p_system.iri("hasSubsystem"),sensor_iri);
        TriplePattern sensor_tp = sensor_iri.isA(p_instrument.iri("Q-Sensor")).andHas(p_ontosensor.iri("observes"),data_iri);
        TriplePattern data_tp = data_iri.isA(p_ontosensor.iri(data)).andHas(p_system.iri("hasValue"),datavalue_iri);
        
        TriplePattern datavalue_tp = datavalue_iri.isA(p_system.iri("ScalarValue"))
                .andHas(p_ontosensor.iri("scaledNumValue"), Rdf.literalOf(value))
                .andHas(p_ontosensor.iri("prescaledNumValue"), Rdf.literalOf(value))
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_ugm3);
//                .andHas(p_time.iri("hasTime"), time_iri);

        TriplePattern protocol_tp;
        TriplePattern state_tp;
        if (data.contentEquals(PM1) || data.contentEquals(PM10) || data.contentEquals(PM25)) {
        	protocol_tp = sensor_iri.has(p_ontosensor.iri("particleProtocolVersion"), "V3.0");
        	state_tp = datavalue_iri.has(p_ontosensor.iri("particleState"),"OK");
        } else {
        	protocol_tp = sensor_iri.has(p_ontosensor.iri("gasProtocolVersion"), "V5.1");
        	state_tp = datavalue_iri.has(p_ontosensor.iri("gasState"),"Reading");
        }
        
//        TriplePattern datatime_tp = time_iri.isA(p_time.iri("Instant")).andHas(p_time.iri("inXSDDateTimeStamp"),0);
		
		modify.insert(station_tp,sensor_tp,data_tp,datavalue_tp,protocol_tp,state_tp);
    }

    /**
     * Get the number of points recorded for the type 'data'
     * @param station_iri_string
     * @param data
     * @return
     */
    private static JSONObject queryNumberAndDataIRI(String station_iri_string,String data) {
    	SelectQuery query = Queries.SELECT();
    	
    	// convert string to IRI
    	Iri station_iri = iri(station_iri_string);
    	
    	// variables we don't need
    	Variable sensor_iri = query.var();
    	Variable data_iri = SparqlBuilder.var("data_iri");
    	Variable datavalue_iri = SparqlBuilder.var("datavalue_iri"); // parameter we want to count
    	
    	TriplePattern station_tp = station_iri.has(p_system.iri("hasSubsystem"),sensor_iri);
    	TriplePattern sensor_tp = sensor_iri.has(p_ontosensor.iri("observes"),data_iri);
    	TriplePattern data_tp = data_iri.isA(p_ontosensor.iri(data)).andHas(p_system.iri("hasValue"),datavalue_iri);
    	GraphPattern querypattern = GraphPatterns.and(station_tp,sensor_tp,data_tp);
    	
    	From queryGraph = SparqlBuilder.from(airquality_graph);
    	
    	query.from(queryGraph).prefix(p_station,p_system,p_ontosensor).select(data_iri,datavalue_iri).where(querypattern);
    	JSONArray queryresult = SparqlGeneral.performQuery(query);

    	// there will be only one data_iri in each sensor
    	// data_iri can contain a number of values (time series data)
    	JSONObject result = new JSONObject();
    	result.put("numberPoints", queryresult.length());
    	result.put("data_iri", queryresult.getJSONObject(0).getString("data_iri"));
    	
    	return result;
    }
    
    public static void addSensorValue(String stationiri, String data, double value, boolean firstvalue) {
    	// first get the number of points, this will be used to create IRIs for the new triples
    	// the number is the same for all sensors
    	// data_iri is the top node of the triples being added here
    	JSONObject numberAndDataIRI = queryNumberAndDataIRI(stationiri, data); 
    	int numberDataPoints = numberAndDataIRI.getInt("numberPoints"); 
    	Iri data_iri = iri(numberAndDataIRI.getString("data_iri"));

    	Iri datavalue_iri;
    	Iri time_iri;
 
    	if ((numberDataPoints==1) && firstvalue) {
    	    datavalue_iri = iri(stationiri+"_v"+data);
    	    time_iri = iri(stationiri+"_time"+data);
    	} else {
    	    datavalue_iri = iri(stationiri+"_v"+data+Integer.toString(1+numberDataPoints));
    	    time_iri = iri(stationiri+"_time"+data+Integer.toString(1+numberDataPoints));
    	}

    	TriplePattern data_tp = data_iri.has(p_system.iri("hasValue"),datavalue_iri);
    	TriplePattern datavalue_tp = datavalue_iri.has(p_ontosensor.iri("scaledNumValue"), Rdf.literalOf(value))
            .andHas(p_ontosensor.iri("prescaledNumValue"), Rdf.literalOf(value));
//    	TriplePattern datatime_tp = time_iri.has(p_time.iri("inXSDDateTimeStamp"),timestamp);
    	
    	ModifyQuery modify = Queries.MODIFY();
    	if (firstvalue) {
    		// delete initial value
    		// It is necessary to use Rdf.literalOf(0.0) instead of 0.0, otherwise it won't match the triple
    		TriplePattern delete_tp = datavalue_iri.has(p_ontosensor.iri("scaledNumValue"), Rdf.literalOf(0.0))
    				.andHas(p_ontosensor.iri("prescaledNumValue"), Rdf.literalOf(0.0));
    		modify.delete(delete_tp);
    	}
        modify.with(airquality_graph).prefix(p_station,p_system,p_ontosensor,p_time).insert(data_tp,datavalue_tp).where();
        SparqlGeneral.performUpdate(modify);
    }
    
    public static JSONArray queryAirStationsWithinScope(Scope sc) {
    	JSONArray result = queryStationsWithinScope(sc,airquality_graph);
    	return result;
    }
    
    public static JSONArray queryWeatherStationsWithinScope(Scope sc) {
    	JSONArray result = queryStationsWithinScope(sc,weather_graph);
    	return result;
    }
    
    private static JSONArray queryStationsWithinScope(Scope sc, Iri graph) {
    	SelectQuery query = Queries.SELECT();
    	
    	// properties we want to query
    	Variable station = SparqlBuilder.var("stationiri");
    	Variable xvalue = SparqlBuilder.var("xvalue");
    	Variable yvalue = SparqlBuilder.var("yvalue");
    	
    	// properties we don't need
    	Variable coord = query.var();
        Variable xcoord = query.var();
        Variable ycoord = query.var();
        Variable vxcoord = query.var();
        Variable vycoord = query.var();
    	
        GraphPattern station_gp = station.has(p_space_time_extended.iri("hasGISCoordinateSystem"),coord);
        GraphPattern projected_gp = coord.has(p_space_time_extended.iri("hasProjectedCoordinate_x"),xcoord)
        		.andHas(p_space_time_extended.iri("hasProjectedCoordinate_y"),ycoord);
        GraphPattern coord_gp = GraphPatterns.and(xcoord.has(p_system.iri("hasValue"),vxcoord), 
        		ycoord.has(p_system.iri("hasValue"),vycoord));
        GraphPattern vcoord_gp = GraphPatterns.and(vxcoord.has(p_system.iri("numericalValue"), xvalue),
        		vycoord.has(p_system.iri("numericalValue"), yvalue));
        
    	// constraint to stations within scope
    	sc.transform("EPSG:4326");
        Expression<?> xconstraint = Expressions.and(Expressions.lt(xvalue, sc.getUpperx()),Expressions.gt(xvalue, sc.getLowerx()));
        Expression<?> yconstraint = Expressions.and(Expressions.lt(yvalue, sc.getUppery()),Expressions.gt(yvalue, sc.getLowery()));
        Expression<?> overallconstraint = Expressions.and(xconstraint,yconstraint);
        
        GraphPatternNotTriples querypattern = GraphPatterns.and(station_gp,projected_gp,coord_gp,vcoord_gp)
        		.filter(overallconstraint);
        
        From queryGraph = SparqlBuilder.from(graph);
        
    	query.from(queryGraph).prefix(p_station,p_space_time_extended, p_system).select(station,xvalue,yvalue).where(querypattern);

    	return SparqlGeneral.performQuery(query);
    }

    public static long queryWeatherStationTimeStamp(String stationiri_string) {
    	SelectQuery query = Queries.SELECT();
    	Iri stationiri = iri(stationiri_string);
    	Variable vtime = SparqlBuilder.var("vtime");
    	GraphPattern querypattern = getStationTimeGP(query,stationiri,vtime);
    	From queryGraph = SparqlBuilder.from(weather_graph);
    	query.from(queryGraph).prefix(p_station,p_ontosensor,p_time,p_system).select(vtime).where(querypattern).distinct();
    	JSONArray queryresult = SparqlGeneral.performQuery(query);
    	long timestamp = queryresult.getJSONObject(0).getLong("vtime");
    	return timestamp;
    }
    
    /**
     * Returns the IRIs of the air quality stations in the endpoint
     * @return
     */
    public static JSONArray queryAllAirStations() {
    	// IRI of station
    	Variable station = SparqlBuilder.var("station");
    	GraphPattern querypattern = station.isA(p_station.iri("AirQualityStation"));

    	SelectQuery query = Queries.SELECT();
    	From queryGraph = SparqlBuilder.from(airquality_graph);
    	query.from(queryGraph).prefix(p_station).select(station).where(querypattern);

    	return SparqlGeneral.performQuery(query);
    }

    /**
     * Returns the coordinates of the air quality station given the IRI
     * called by the SensorUpdaterAgent
     * @param station_iri_string
     * @return
     */
    public static double [] queryAirStationCoordinatesWithIRI(String station_iri_string) {
    	SelectQuery query = Queries.SELECT();
    	Iri station_iri = iri(station_iri_string);
    	
    	//coordinates we want
    	Variable xvalue = SparqlBuilder.var("xvalue");
    	Variable yvalue = SparqlBuilder.var("yvalue");
    	
    	// properties we don't need
    	Variable coord = query.var();
        Variable xcoord = query.var();
        Variable ycoord = query.var();
        Variable vxcoord = query.var();
        Variable vycoord = query.var();
    	
        GraphPattern station_gp = station_iri.has(p_space_time_extended.iri("hasGISCoordinateSystem"),coord);
        GraphPattern projected_gp = coord.has(p_space_time_extended.iri("hasProjectedCoordinate_x"),xcoord)
        		.andHas(p_space_time_extended.iri("hasProjectedCoordinate_y"),ycoord);
        GraphPattern coord_gp = GraphPatterns.and(xcoord.has(p_system.iri("hasValue"),vxcoord), 
        		ycoord.has(p_system.iri("hasValue"),vycoord));
        GraphPattern vcoord_gp = GraphPatterns.and(vxcoord.has(p_system.iri("numericalValue"), xvalue),
        		vycoord.has(p_system.iri("numericalValue"), yvalue));
        
        //assemble patterns
        GraphPattern querypattern = GraphPatterns.and(station_gp,projected_gp,coord_gp,vcoord_gp);
        
        From queryGraph = SparqlBuilder.from(airquality_graph);
        
        query.from(queryGraph).prefix(p_station,p_space_time_extended,p_system).select(xvalue,yvalue).where(querypattern);
        
        JSONArray queryresult = SparqlGeneral.performQuery(query);
        
        double [] coordinates_xy = {queryresult.getJSONObject(0).getDouble("xvalue"),
        		queryresult.getJSONObject(0).getDouble("yvalue")};

    	return coordinates_xy;
    }
    
    /**
     * Returns all the measured
     * @param station_iri_string
     * @return
     */
    public static JSONArray queryAirStationProperties(String station_iri_string) {
    	SelectQuery query = Queries.SELECT();
    	Iri station_iri = iri(station_iri_string);
    	
    	Variable sensor_iri = query.var();
    	Variable data_iri = query.var();
    	Variable data_type = SparqlBuilder.var("data_type");
    	Variable datavalue_iri = query.var();
    	Variable numvalue = SparqlBuilder.var("numvalue");
    	
    	TriplePattern station_tp = station_iri.has(p_system.iri("hasSubsystem"),sensor_iri);
        TriplePattern sensor_tp = sensor_iri.has(p_ontosensor.iri("observes"),data_iri);
        TriplePattern data_tp = data_iri.isA(data_type).andHas(p_system.iri("hasValue"),datavalue_iri);
    	
        //scaled and prescaled are the same for now
        TriplePattern datavalue_tp = datavalue_iri.has(p_ontosensor.iri("prescaledNumValue"), numvalue);
        
        GraphPattern querypattern = GraphPatterns.and(station_tp,sensor_tp,data_tp,datavalue_tp);
        From queryGraph = SparqlBuilder.from(airquality_graph);
        
        query.from(queryGraph).prefix(p_station,p_system,p_ontosensor).select(data_type,numvalue).where(querypattern);
    	return SparqlGeneral.performQuery(query);
    }
    
    public static int GetNumWeatherStation() {
    	SelectQuery query = Queries.SELECT();
    	
    	String queryKey = "numstn";
    	Variable numstn = SparqlBuilder.var(queryKey);
    	Variable stn = query.var();
    	
    	GraphPattern queryPattern = stn.isA(WeatherStation);
    	Assignment assign = Expressions.count(stn).as(numstn);
    	
    	From FromGraph = SparqlBuilder.from(weather_graph);
    	
    	query.prefix(p_station).from(FromGraph).select(assign).where(queryPattern);
    	
    	int result = SparqlGeneral.performQuery(query).getJSONObject(0).getInt(queryKey);
    	return result;
    }
}

