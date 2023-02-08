package uk.ac.cam.cares.jps.agent.mobileappagent;

import lombok.Getter;
import lombok.Setter;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelContext;

import java.time.OffsetDateTime;
import java.util.*;


public class InstantiationClient {
    public static void main(String[] args) throws ParseException {
        instantiationMethod();

    }

    public static void instantiationMethod() {

        // Create context to work in, and also clear any old existing data
        ModelContext context = new ModelContext("http://localhost:48888/test");
        context.update("CLEAR ALL");


        HashMap dataIRI = dataIRIHashMapGenerator();


        /**
         * Creating instances and randomUUID.
         */

        //Person
        Person person = context.createNewModel(Person.class, "https://w3id.org/MON/person.owl/"+UUID.randomUUID());

        //Smartphone
        Smartphone smartphone = context.createNewModel(Smartphone.class, "https://www.theworldavatar.com/kg/ontodevice/smartphone_"+UUID.randomUUID());

        //Ontodevic:Sensors
        Accelerometer accelerometer= context.createNewModel(Accelerometer.class, "https://www.theworldavatar.com/kg/ontodevice/accelerometer_"+UUID.randomUUID());
        Camera camera = context.createNewModel(Camera.class, "https://www.theworldavatar.com/kg/ontodevice/camera_"+UUID.randomUUID());
        Magnetometer magnetometer = context.createNewModel(Magnetometer.class, "https://www.theworldavatar.com/kg/ontodevice/magnetometer_"+UUID.randomUUID());
        GravitySensor gravitySensor = context.createNewModel(GravitySensor.class, "https://www.theworldavatar.com/kg/ontodevice/gravitySensor_"+UUID.randomUUID());
        GPSSensor gpsSensor = context.createNewModel(GPSSensor.class, "https://www.theworldavatar.com/kg/ontodevice/gpsSensor_"+UUID.randomUUID());
        Microphone microphone = context.createNewModel(Microphone.class, "https://www.theworldavatar.com/kg/ontodevice/microphone_"+UUID.randomUUID());

        //Measured classes
        Accel_x accel_x = context.createNewModel(Accel_x.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/accel_x_"+UUID.randomUUID());
        Accel_y accel_y = context.createNewModel(Accel_y.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/accel_y_"+UUID.randomUUID());
        Accel_z accel_z = context.createNewModel(Accel_z.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/accel_z_"+UUID.randomUUID());
        Gravity_x gravity_x = context.createNewModel(Gravity_x.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/gravity_x_"+UUID.randomUUID());
        Gravity_y gravity_y = context.createNewModel(Gravity_y.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/gravity_y_"+UUID.randomUUID());
        Gravity_z gravity_z = context.createNewModel(Gravity_z.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/gravity_z_"+UUID.randomUUID());
        Magnetometer_x magnetometer_x = context.createNewModel(Magnetometer_x.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/magnetometer_x_"+UUID.randomUUID());
        Magnetometer_y magnetometer_y = context.createNewModel(Magnetometer_y.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/magnetometer_y_"+UUID.randomUUID());
        Magnetometer_z magnetometer_z = context.createNewModel(Magnetometer_z.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/magnetometer_z_"+UUID.randomUUID());
        Acceleration acceleration = context.createNewModel(Acceleration.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Acceleration_"+UUID.randomUUID());
        MagneticFluxDensity magneticFluxDensity = context.createNewModel(MagneticFluxDensity.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/MagneticFluxDensity_"+UUID.randomUUID());
        Speed speed = context.createNewModel(Speed.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Speed_"+UUID.randomUUID());
        Bearing bearing = context.createNewModel(Bearing.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/bearing_"+UUID.randomUUID());
        Altitude altitude = context.createNewModel(Altitude.class, "https://www.theworldavatar.com/kg/ontoGeo/altitude_"+UUID.randomUUID());
        GeomLocation geomLocation = context.createNewModel(GeomLocation.class, "https://www.theworldavatar.com/kg/ontoGeo/geomlocation_"+UUID.randomUUID());
        Noise noise = context.createNewModel(Noise.class, "https://www.theworldavatar.com/kg/ontouom/Noise_"+UUID.randomUUID());
        SoundPressureLevel soundPressureLevel = context.createNewModel(SoundPressureLevel.class, "https://www.theworldavatar.com/kg/ontouom/SoundPressureLevel_"+UUID.randomUUID());
        Illuminance illuminance = context.createNewModel(Illuminance.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Illuminance_"+UUID.randomUUID());

        /**
         * Input hashmap of instantiated measureIRIList for the measureclass
         */

        Measure measure_accel_x = context.createNewModel(Measure.class, dataIRI.get("measure_accel_x").toString());
        Measure measure_accel_y = context.createNewModel(Measure.class, dataIRI.get("measure_accel_y").toString());
        Measure measure_accel_z = context.createNewModel(Measure.class, dataIRI.get("measure_accel_z").toString());

        Measure measure_gravity_x = context.createNewModel(Measure.class, dataIRI.get("measure_gravity_x").toString());
        Measure measure_gravity_y = context.createNewModel(Measure.class, dataIRI.get("measure_gravity_y").toString());
        Measure measure_gravity_z = context.createNewModel(Measure.class, dataIRI.get("measure_gravity_z").toString());

        Measure measure_magnetometer_x = context.createNewModel(Measure.class, dataIRI.get("measure_magnetometer_x").toString());
        Measure measure_magnetometer_y = context.createNewModel(Measure.class, dataIRI.get("measure_magnetometer_y").toString());
        Measure measure_magnetometer_z = context.createNewModel(Measure.class, dataIRI.get("measure_magnetometer_z").toString());

        Measure measure_speed = context.createNewModel(Measure.class, dataIRI.get("measure_speed").toString());
        Measure measure_geom_location = context.createNewModel(Measure.class, dataIRI.get("measure_geom_location").toString());
        Measure measure_bearing = context.createNewModel(Measure.class, dataIRI.get("measure_bearing").toString());
        Measure measure_altitude = context.createNewModel(Measure.class, dataIRI.get("measure_altitude").toString());

        Measure measure_dbfs = context.createNewModel(Measure.class, dataIRI.get("measure_dbfs").toString());
        Measure measure_light_value = context.createNewModel(Measure.class, dataIRI.get("measure_light_value").toString());

        /**
         * Creating context for unitclasses
         */
        Unit metrePerSecond_TimeSquared = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/metrePerSecond_TimeSquared");
        Unit metre = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/metre");
        Unit lux = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/lux");
        Unit microtesla = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/microtesla");
        Unit metrePerSecond_Time = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/om:metrePerSecond-Time");
        Unit dBFS = context.createNewModel(Unit.class, "https://www.theworldavatar.com/kg/ontouom/dBFS");
        Unit degree= context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/degree");



        /**
         * Person class
         */
        person.smartphone=smartphone;

        /**
         * Smartphone class
         */
        smartphone.person=person;
        smartphone.devices.add(camera);
        smartphone.devices.add(accelerometer);
        smartphone.devices.add(magnetometer);
        smartphone.devices.add(gpsSensor);
        smartphone.devices.add(microphone);
        smartphone.devices.add(gravitySensor);

        /**
         * Accelerometer class
         */
        accel_x.accelerometer=accelerometer;
        accel_y.accelerometer=accelerometer;
        accel_z.accelerometer=accelerometer;

        acceleration.accelerationVariables.add(accel_x);
        acceleration.accelerationVariables.add(accel_y);
        acceleration.accelerationVariables.add(accel_z);

        /**
         * GravitySensor class
         */
        gravity_x.gravitysensor=gravitySensor;
        gravity_y.gravitysensor=gravitySensor;
        gravity_z.gravitysensor=gravitySensor;


        acceleration.accelerationVariables.add(gravity_x);
        acceleration.accelerationVariables.add(gravity_y);
        acceleration.accelerationVariables.add(gravity_z);
        /**
         * Magnetometer class
         */
        magnetometer_x.magnetometer=magnetometer;
        magnetometer_y.magnetometer=magnetometer;
        magnetometer_z.magnetometer=magnetometer;

        magneticFluxDensity.magneticFluxDensityVariables.add(magnetometer_x);
        magneticFluxDensity.magneticFluxDensityVariables.add(magnetometer_y);
        magneticFluxDensity.magneticFluxDensityVariables.add(magnetometer_z);

        /**
         * GPSSensor
         */
        gpsSensor.gpsSensorVariables.add(speed);
        gpsSensor.gpsSensorVariables.add(altitude);
        gpsSensor.gpsSensorVariables.add(geomLocation);
        gpsSensor.gpsSensorVariables.add(bearing);

        /**
         * Camera class
         */
        camera.illuminance=illuminance;
        illuminance.camera=camera;

        /**
         * Microphone class
         */
        microphone.soundPressureLevel=soundPressureLevel;
        noise.soundPressureLevel=soundPressureLevel;
        soundPressureLevel.noise=noise;

        /**
         * Measure class
         */
        measure_accel_x.measures.add(accel_x);
        measure_accel_y.measures.add(accel_y);
        measure_accel_z.measures.add(accel_z);
        measure_gravity_x.measures.add(gravity_x);
        measure_gravity_y.measures.add(gravity_y);
        measure_gravity_z.measures.add(gravity_z);
        measure_magnetometer_x.measures.add(magnetometer_x);
        measure_magnetometer_y.measures.add(magnetometer_y);
        measure_magnetometer_z.measures.add(magnetometer_z);

        measure_speed.measures.add(speed);
        measure_geom_location.measures.add(geomLocation);
        measure_bearing.measures.add(bearing);
        measure_altitude.measures.add(altitude);

        measure_dbfs.measures.add(soundPressureLevel);
        measure_light_value.measures.add(illuminance);

        /**
         * Units class
         */
        metrePerSecond_TimeSquared.unitsInstant.add(measure_accel_x);
        metrePerSecond_TimeSquared.unitsInstant.add(measure_accel_y);
        metrePerSecond_TimeSquared.unitsInstant.add(measure_accel_z);
        metrePerSecond_TimeSquared.unitsInstant.add(measure_gravity_x);
        metrePerSecond_TimeSquared.unitsInstant.add(measure_gravity_y);
        metrePerSecond_TimeSquared.unitsInstant.add(measure_gravity_z);
        microtesla.unitsInstant.add(measure_magnetometer_x);
        microtesla.unitsInstant.add(measure_magnetometer_y);
        microtesla.unitsInstant.add(measure_magnetometer_z);
        lux.unitsInstant.add(measure_light_value);
        dBFS.unitsInstant.add(measure_dbfs);
        metrePerSecond_Time.unitsInstant.add(measure_speed);
        degree.unitsInstant.add(measure_bearing);
        metre.unitsInstant.add(measure_altitude);

        context.pushAllChanges();
    }


    /**
     * Defining model
     */
    private static class OntoDeviceModel extends Model {}
    private static class SensorLoggerModel extends Model {}

    /**
     * Person branch
     */
    public static class Person extends Model {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/hasA")
        protected InstantiationClient.Smartphone smartphone;
    }

    /**
     * Smartphone branch
     */
    public static class Smartphone extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/hasA", backward = true)
        protected InstantiationClient.Person person;
        @Getter @Setter @FieldAnnotation(value = "https://saref.etsi.org/core/consistsOf", innerType = OntoDeviceModel.class)
        protected ArrayList<OntoDeviceModel> devices;
    }
    /**
     * Accelerometer branch
     */
    public static class Accelerometer extends OntoDeviceModel{}
    public static class Accel_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }

    public static class Accel_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }

    public static class Accel_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }

    /**
     * Gravity Branch
     */
    public static class GravitySensor extends OntoDeviceModel{}
    public static class Gravity_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }

    public static class Gravity_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }

    public static class Gravity_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }


    /**
     * Magnetometer Branch
     */

    public static class Magnetometer extends OntoDeviceModel{}

    public static class Magnetometer_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel magnetometer;
    }

    public static class Magnetometer_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel magnetometer;
    }

    public static class Magnetometer_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel magnetometer;
    }

    /**
     * Camera branch
     */
    public static class Camera extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures")
        protected Illuminance illuminance;
    }

    /**
     * GPSSensor branch
     */


    public static class GPSSensor extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> gpsSensorVariables;

    }

    /**
     * Microphone branch
     */

    public static class Microphone extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures")
        protected SoundPressureLevel soundPressureLevel;
    }



    public static class Acceleration extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",innerType = SensorLoggerModel.class,backward = true)
        protected ArrayList<SensorLoggerModel> accelerationVariables;

    }
    public static class MagneticFluxDensity extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",innerType = SensorLoggerModel.class,backward = true)
        protected ArrayList<SensorLoggerModel> magneticFluxDensityVariables;


    }
    public static class Speed extends SensorLoggerModel{}
    public static class Bearing extends SensorLoggerModel{}

    //Temporary assumptions that GeomLocation and Altitude extends SensorLoggerModel
    public static class GeomLocation extends SensorLoggerModel{}
    public static class Altitude extends SensorLoggerModel{}
    public static class SoundPressureLevel extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#", backward = true)
        protected Noise noise;
    }
    public static class Noise extends SoundPressureLevel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        protected SoundPressureLevel soundPressureLevel;
    }
    public static class Illuminance extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected Camera camera;
    }


    /**
     * Measure
     */
    public static class Measure extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue",backward = true, innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> measures;
    }

    /**
     *Unit
     */
    public static class Unit extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",backward = true, innerType = Measure.class)
        protected ArrayList<Measure> unitsInstant;
    }










































    public static final String accelerometer = "accelerometer";
    public static final String gravity = "gravity";
    public static final String light = "light";
    public static final String location = "location";
    public static final String magnetometer = "magnetometer";
    public static final String microphone = "microphone";

    //Declare table header as string.
    public static final String timestamp = "timestamp";
    public static final String accel_x = "accel_x";
    public static final String accel_y = "accel_y";
    public static final String accel_z = "accel_z";
    public static final String gravity_x = "gravity_x";
    public static final String gravity_y = "gravity_y";
    public static final String gravity_z = "gravity_z";
    public static final String light_value = "light_value";
    public static final String bearing = "bearing";
    public static final String speed = "speed";
    public static final String altitude = "altitude";
    public static final String geom_location = "geom_location";
    public static final String magnetometer_x = "magnetometer_x";
    public static final String magnetometer_y = "magnetometer_y";
    public static final String magnetometer_z = "magnetometer_z";
    public static final String dbfs = "dbfs";

    //Declare tableHeader as list of strings
    public static List<String> accelerometerHeader = Arrays.asList(timestamp, accel_x, accel_y, accel_z);
    public static List<String> gravityHeader = Arrays.asList(timestamp, gravity_x, gravity_y, gravity_z);
    public static List<String> lightHeader = Arrays.asList(timestamp, light_value);
    public static List<String> locationHeader = Arrays.asList(timestamp, bearing, speed, altitude, geom_location);
    public static List<String> magnetometerHeader = Arrays.asList(timestamp, magnetometer_x, magnetometer_y, magnetometer_z);
    public static List<String> microphoneHeader = Arrays.asList(timestamp, dbfs);
    public static List<List<String>> tableHeaderList= Arrays.asList(accelerometerHeader,gravityHeader,lightHeader,locationHeader,magnetometerHeader,microphoneHeader);
    public static List<String> tableList = Arrays.asList(accelerometer, gravity,light, location,magnetometer,microphone);
    private static final String dbURL = "jdbc:postgresql://localhost:5432/develop";
    private static final String user = "postgres";
    private static final String password = "postgres";
    private static RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
    private static RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/develop/sparql", "http://127.0.0.1:9999/blazegraph/namespace/develop/sparql");
    private static TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
    private JSONArray dataArray;
    private String Query;
    private static final String BASEURI = "https://www.theworldavatar.com/kg/measure_";




    public static HashMap dataIRIHashMapGenerator() {

        HashMap hashMap = new HashMap();

        //Loop through each table
        for (int i = 0; i < tableList.size(); i++) {
            List tableHeader= tableHeaderList.get(i);
            List<String> dataIRIList = new ArrayList<>();;
            for (int sensorVariable = 1; sensorVariable < tableHeader.size() ;sensorVariable++){
                String dataIRIName =BASEURI+ tableHeader.get(sensorVariable)+ "_"+ UUID.randomUUID();

                String key = "measure_"+tableHeader.get(sensorVariable);
                dataIRIList.add(dataIRIName);
                hashMap.put(key,dataIRIName);
            }
        }
        return hashMap;
    }

}
