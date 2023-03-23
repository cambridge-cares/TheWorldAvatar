 package uk.ac.cam.cares.jps.agent;

 import org.apache.jena.arq.querybuilder.SelectBuilder;
 import org.apache.jena.arq.querybuilder.WhereBuilder;
 import org.apache.jena.graph.Node;
 import org.apache.jena.graph.NodeFactory;
 import org.apache.jena.sparql.core.Var;
 import org.apache.jena.sparql.lang.sparql_11.ParseException;
 import org.json.JSONArray;
 import org.openrdf.query.algebra.Str;
 import org.postgis.Point;
 import org.testng.annotations.Test;
 import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.StaticInstantiation;
 import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
 import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
 import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
 import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
 import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

 import java.sql.Connection;
 import java.time.OffsetDateTime;
 import java.util.*;

 public class QueryClient {




     private static final String dbURL = "jdbc:postgresql://localhost:5432/testingInstantiation";
     private static final String user = "postgres";
     private static final String password = "postgres";
     private static RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);




     private static ArrayList<OffsetDateTime> accel_tsList = new ArrayList<>();
     private static List<Double> accelList_x = new ArrayList<>();
     private static List<Double> accelList_y = new ArrayList<>();
     private static List<Double> accelList_z = new ArrayList<>();
     private static List<List<?>> accel_lolValues= Arrays.asList(accelList_x,accelList_y,accelList_z);

     //Magnetometer list
     private static ArrayList<OffsetDateTime> magnetometer_tsList = new ArrayList<>();
     private static List<Double> magnetometerList_x = new ArrayList<>();
     private static List<Double> magnetometerList_y = new ArrayList<>();
     private static List<Double> magnetometerList_z = new ArrayList<>();
     private static List<List<?>> magnetometer_lolValues= Arrays.asList(magnetometerList_x,magnetometerList_y,magnetometerList_z);

     //Gravity sensor list
     private static ArrayList<OffsetDateTime> gravity_tsList = new ArrayList<>();
     private static List<Double> gravityList_x = new ArrayList<>();
     private static List<Double> gravityList_y = new ArrayList<>();
     private static List<Double> gravityList_z = new ArrayList<>();
     private static List<List<?>> gravity_lolValues= Arrays.asList(gravityList_x,gravityList_y,gravityList_z);

     //Location list
     private static ArrayList<OffsetDateTime> location_tsList = new ArrayList<>();
     private static List<Double> bearingList = new ArrayList<>();
     private static List<Double> speedList = new ArrayList<>();
     private static List<Double> altitudeList = new ArrayList<>();
     private static List<Point> geomLocationList = new ArrayList<>();
     private static List<List<?>> location_lolValues= Arrays.asList(bearingList,speedList,altitudeList,geomLocationList);

     //Microphone lists
     private static ArrayList<OffsetDateTime> dBFS_tsList = new ArrayList<>();
     private static List<Double> dBFSList = new ArrayList<>();
     private static List<List<?>> dBFS_lolValues= Arrays.asList(dBFSList);

     //Light value lists
     private static ArrayList<OffsetDateTime> lightValue_tsList = new ArrayList<>();
     private static List<Double> lightValueList = new ArrayList<>();
     private static List<List<?>> lightValue_lolValues= Arrays.asList(lightValueList);

     private static ArrayList<OffsetDateTime> brightness_tsList = new ArrayList<>();
     private static List<Double> brightnessList = new ArrayList<>();
     private static List<List<?>> brightness_lolValues= Arrays.asList(brightnessList);

     private static int maxSize = accel_lolValues.size()+magnetometer_lolValues.size()+gravity_lolValues.size()+location_lolValues.size()+dBFS_lolValues.size()+lightValue_lolValues.size()+brightness_lolValues.size();

     private static ArrayList<OffsetDateTime> timesList;
     private static List<List<?>> lolvalues;


     private static String accel_xIRI;
     private static String accel_yIRI;
     private static String accel_zIRI;
     private static String gravity_xIRI;
     private static String gravity_yIRI;
     private static String gravity_zIRI;
     private static String magnetometer_xIRI;
     private static String magnetometer_yIRI;
     private static String magnetometer_zIRI;
     private static String bearingIRI;
     private static String speedIRI;
     private static String altitudeIRI;
     private static String pointIRI;
     private static String dbfsIRI;
     private static String relativeBrightnessIRI;
     private static String light_valueIRI;

     private static final String timeUnit = OffsetDateTime.class.getSimpleName();
     private HashMap iriHashmap = new HashMap();

     private static String deviceID ="605a09c9-d6c5-4ba7-bc28-fe595d698b41";

     @Test
     public void testQuery() throws ParseException {
         String smartphoneString="https://www.theworldavatar.com/kg/sensorloggerapp/smartphone_605a09c9-d6c5-4ba7-bc28-fe595d698b41";
         Node smartphoneIRI=NodeFactory.createURI(smartphoneString);
         getAccel_xIRIArray(smartphoneIRI);
         
         
     }




     public void initTimeseries() throws ParseException {
         String smartphoneString="https://www.theworldavatar.com/kg/sensorloggerapp/smartphone_605a09c9-d6c5-4ba7-bc28-fe595d698b41";
         Node smartphoneIRI=NodeFactory.createURI(smartphoneString);
         iriHashmap.put("DEVICEID",deviceID);
         List tsList = new ArrayList();

         //Accelerometer
         if(getAccel_xIRIArray(smartphoneIRI).isEmpty() && getAccel_yIRIArray(smartphoneIRI).isEmpty() && getAccel_zIRIArray(smartphoneIRI).isEmpty()) {
             List<String> dataIRIList = new ArrayList<>();
             accel_xIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_x_" + UUID.randomUUID();
             accel_yIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_y_" + UUID.randomUUID();
             accel_zIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_z_" + UUID.randomUUID();

             dataIRIList.add(accel_xIRI);
             dataIRIList.add(accel_yIRI);
             dataIRIList.add(accel_zIRI);
             iriHashmap.put("accel_x", accel_xIRI);
             iriHashmap.put("accel_y", accel_yIRI);
             iriHashmap.put("accel_z", accel_zIRI);

             List<Class> dataClass = Collections.nCopies(dataIRIList.size(), Double.class);
             initTimeSeries(dataIRIList, dataClass, timeUnit);
         }

         //GravitySensor
         if(getGravity_xIRIArray(smartphoneIRI).isEmpty() && getGravity_yIRIArray(smartphoneIRI).isEmpty() && getGravity_zIRIArray(smartphoneIRI).isEmpty())
         {
             List<String> dataIRIList = new ArrayList<>();
             gravity_xIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_x_"+ UUID.randomUUID();
             gravity_yIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_y_"+ UUID.randomUUID();
             gravity_zIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_z_"+ UUID.randomUUID();

             dataIRIList.add(gravity_xIRI);
             dataIRIList.add(gravity_yIRI);
             dataIRIList.add(gravity_zIRI);
             iriHashmap.put("gravity_x", gravity_xIRI);
             iriHashmap.put("gravity_y", gravity_yIRI);
             iriHashmap.put("gravity_z", gravity_zIRI);


             List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
             initTimeSeries(dataIRIList,dataClass,timeUnit);
         }

         //Magnetometer
         if(getMagnetometer_xIRIArray(smartphoneIRI).isEmpty() && getMagnetometer_yIRIArray(smartphoneIRI).isEmpty() && getMagnetometer_zIRIArray(smartphoneIRI).isEmpty())
         {
             List<String> dataIRIList = new ArrayList<>();
             magnetometer_xIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_x_"+ UUID.randomUUID();
             magnetometer_yIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_y_"+ UUID.randomUUID();
             magnetometer_zIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_z_"+ UUID.randomUUID();

             dataIRIList.add(magnetometer_xIRI);
             dataIRIList.add(magnetometer_yIRI);
             dataIRIList.add(magnetometer_zIRI);
             iriHashmap.put("magnetometer_x", magnetometer_xIRI);
             iriHashmap.put("magnetometer_y", magnetometer_yIRI);
             iriHashmap.put("magnetometer_z", magnetometer_zIRI);

             List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
             initTimeSeries(dataIRIList,dataClass,timeUnit);
         }

         //GPSDevice
         if(getBearingIRIArray(smartphoneIRI).isEmpty() && getSpeedIRIArray(smartphoneIRI).isEmpty() && getAltitudeIRIArray(smartphoneIRI).isEmpty() && getPointIRIArray(smartphoneIRI).isEmpty())
         {
             List<String> dataIRIList = new ArrayList<>();
             bearingIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_bearing_"+ UUID.randomUUID();
             speedIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_speed_"+ UUID.randomUUID();
             altitudeIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_altitude_"+ UUID.randomUUID();
             pointIRI="https://www.theworldavatar.com/kg/sensorloggerapp/point_"+ UUID.randomUUID();
             dataIRIList.add(bearingIRI);
             dataIRIList.add(speedIRI);
             dataIRIList.add(altitudeIRI);
             dataIRIList.add(pointIRI);
             iriHashmap.put("bearing", bearingIRI);
             iriHashmap.put("speed", speedIRI);
             iriHashmap.put("altitude", altitudeIRI);
             iriHashmap.put("point",pointIRI);

             List<Class> dataClass = Collections.nCopies(dataIRIList.size()-1,Double.class);
             List <Class> dataClassWithPoint= new ArrayList<>(dataClass);
             dataClassWithPoint.add(Point.class);
             initTimeSeries(dataIRIList,dataClassWithPoint,timeUnit);
         }

         //Microphone
         if(getSoundPressureLevelIRIArray(smartphoneIRI).isEmpty())
         {
             List<String> dataIRIList = new ArrayList<>();
             dbfsIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_dbfs_"+ UUID.randomUUID();
             dataIRIList.add(dbfsIRI);
             iriHashmap.put("dbfs", dbfsIRI);

             List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
             initTimeSeries(dataIRIList,dataClass,timeUnit);
         }

         //RelativeBrightness
         if(getRelativeBrightnessIRIArray(smartphoneIRI).isEmpty())
         {
             List<String> dataIRIList = new ArrayList<>();
             relativeBrightnessIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/relativeBrightness_"+ UUID.randomUUID();
             dataIRIList.add(relativeBrightnessIRI);
             iriHashmap.put("relativeBrightness", relativeBrightnessIRI);

             List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
             initTimeSeries(dataIRIList,dataClass,timeUnit);
         }

         //Camera
         if(getIlluminanceIRIArray(smartphoneIRI).isEmpty())
         {
             List<String> dataIRIList = new ArrayList<>();
             light_valueIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/light_value_"+ UUID.randomUUID();
             dataIRIList.add(light_valueIRI);
             iriHashmap.put("light_value", light_valueIRI);

             List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
             initTimeSeries(dataIRIList,dataClass,timeUnit);
         }

         StaticInstantiation.instantiationMethod(iriHashmap);
     }

     private void bulkAddTimeSeriesData(List<TimeSeries> tsList){
         try (Connection conn = rdbStoreClient.getConnection()) {
             TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
             tsClient.bulkaddTimeSeriesData(tsList, conn);
         } catch (Exception e) {
             e.printStackTrace();
             throw new JPSRuntimeException(e);
         }
     }

     private void initTimeSeries(List<String> dataIRIList, List dataClass, String timeUnit){

         try (Connection conn = rdbStoreClient.getConnection()) {
             TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
             tsClient.initTimeSeries(dataIRIList, dataClass, timeUnit, conn);
         } catch (Exception e) {
             e.printStackTrace();
             throw new JPSRuntimeException(e);
         }
     }




     // prefixes
     static final String ONTOSLMA = "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/";
     static final String SLMA = "https://www.theworldavatar.com/kg/sensorloggerapp/";
     static final String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
     static final String SF ="http://www.opengis.net/ont/sf#";
     static final String ONTODEVICE = "https://www.theworldavatar.com/kg/ontodevice/";
     static final String MON = "https://w3id.org/MON/person.owl";
     static final String SAREF="https://saref.etsi.org/core/";
     static final String RDF="http://www.w3.org/1999/02/22-rdf-syntax-ns#";
     final static String str_s = "s";
     final static Var VAR_S = Var.alloc(str_s);
     final static String str_o = "o";
     final static Var VAR_O = Var.alloc(str_o);


     RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/test/sparql", "http://127.0.0.1:9999/blazegraph/namespace/test/sparql");

    
    
     String getIRIfromJSONarray(JSONArray jsonArray){
         if(jsonArray.length()!=1){System.out.println("There is more than 1 data in this JSONArray, only the first one is returned");}
         return jsonArray.getJSONObject(0).get(str_o).toString();
     }
    
    
     JSONArray getPersonIRI(String smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addWhere(VAR_S, "slma:hasA", smartphoneIRI);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_S).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

     JSONArray getAccel_xIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?accelerometer")
                 .addWhere("?accelerometer","rdf:type","ontodevice:Accelerometer")
                 .addWhere("?accelerometer","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:AccelerationVector")
                 .addWhere("?vector", "ontoslma:hasXComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getAccel_yIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?accelerometer")
                 .addWhere("?accelerometer","rdf:type","ontodevice:Accelerometer")
                 .addWhere("?accelerometer","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:AccelerationVector")
                 .addWhere("?vector", "ontoslma:hasYComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getAccel_zIRIArray(Node smartphoneIRI) throws ParseException {
         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?accelerometer")
                 .addWhere("?accelerometer","rdf:type","ontodevice:Accelerometer")
                 .addWhere("?accelerometer","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:AccelerationVector")
                 .addWhere("?vector", "ontoslma:hasZComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }


     JSONArray getGravity_xIRIArray(Node smartphoneIRI) throws ParseException {


         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?gravitySensor")
                 .addWhere("?gravitySensor","rdf:type","ontodevice:GravitySensor")
                 .addWhere("?gravitySensor","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:GravityVector")
                 .addWhere("?vector", "ontoslma:hasXComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getGravity_yIRIArray(Node smartphoneIRI) throws ParseException {


         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?gravitySensor")
                 .addWhere("?gravitySensor","rdf:type","ontodevice:GravitySensor")
                 .addWhere("?gravitySensor","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:GravityVector")
                 .addWhere("?vector", "ontoslma:hasYComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);
         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getGravity_zIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?gravitySensor")
                 .addWhere("?gravitySensor","rdf:type","ontodevice:GravitySensor")
                 .addWhere("?gravitySensor","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:GravityVector")
                 .addWhere("?vector", "ontoslma:hasZComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

     JSONArray getMagnetometer_xIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?magnetometer")
                 .addWhere("?magnetometer","rdf:type","ontodevice:Magnetometer")
                 .addWhere("?magnetometer","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:MagneticFluxDensityVector")
                 .addWhere("?vector", "ontoslma:hasXComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);
         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getMagnetometer_yIRIArray(Node smartphoneIRI) throws ParseException {
         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?magnetometer")
                 .addWhere("?magnetometer","rdf:type","ontodevice:Magnetometer")
                 .addWhere("?magnetometer","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:MagneticFluxDensityVector")
                 .addWhere("?vector", "ontoslma:hasYComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getMagnetometer_zIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("ontoslma",ONTOSLMA)
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?magnetometer")
                 .addWhere("?magnetometer","rdf:type","ontodevice:Magnetometer")
                 .addWhere("?magnetometer","ontodevice:measures","?vector")
                 .addWhere("?vector","rdf:type","ontoslma:MagneticFluxDensityVector")
                 .addWhere("?vector", "ontoslma:hasZComponent", "?quantity")
                 .addWhere("?quantity", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

     JSONArray getBearingIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                 .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                 .addWhere("?GPSDevice","ontodevice:measures","?Bearing")
                 .addWhere("?Bearing","rdf:type","slma:Bearing")
                 .addWhere("?Bearing", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getAltitudeIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                 .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                 .addWhere("?GPSDevice","ontodevice:measures","?Altitude")
                 .addWhere("?Altitude","rdf:type","slma:Altitude")
                 .addWhere("?Altitude", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

     JSONArray getSpeedIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                 .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                 .addWhere("?GPSDevice","ontodevice:measures","?Speed")
                 .addWhere("?Speed","rdf:type","om:Speed")
                 .addWhere("?Speed", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }
     JSONArray getPointIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addPrefix("sf",SF)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                 .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                 .addWhere("?GPSDevice","ontodevice:hasGeoLocation",VAR_O)
                 .addWhere(VAR_O,"rdf:type","sf:Point");

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

     JSONArray getSoundPressureLevelIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?microphone")
                 .addWhere("?microphone","rdf:type","ontodevice:Microphone")
                 .addWhere("?microphone","ontodevice:measures","?om_soundPressureLevel")
                 .addWhere("?om_soundPressureLevel", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

     JSONArray getIlluminanceIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"saref:consistsOf","?camera")
                 .addWhere("?camera","rdf:type","ontodevice:Camera")
                 .addWhere("?camera","ontodevice:measures","?om_illuminance")
                 .addWhere("?om_illuminance", "om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

     JSONArray getRelativeBrightnessIRIArray(Node smartphoneIRI) throws ParseException {

         WhereBuilder wb = new WhereBuilder()
                 .addPrefix("slma",SLMA)
                 .addPrefix ("saref",SAREF)
                 .addPrefix("ontodevice", ONTODEVICE)
                 .addPrefix("rdf", RDF)
                 .addPrefix("om",OM)
                 .addWhere(smartphoneIRI,"ontodevice:hasScreenBrightness","?relativeBrightness")
                 .addWhere("?relativeBrightness","rdf:type","ontodevice:RelativeBrightness")
                 .addWhere("?relativeBrightness","rdf:type","?om_ratio")
                 .addWhere("?om_ratio", "rdf:type", "om:Ratio")
                 .addWhere("?om_ratio","om:hasValue", VAR_O);

         SelectBuilder sb = new SelectBuilder()
                 .addVar(VAR_O).addWhere(wb);

         JSONArray queryResult=storeClient.executeQuery(sb.buildString());
         return queryResult;
     }

 }
