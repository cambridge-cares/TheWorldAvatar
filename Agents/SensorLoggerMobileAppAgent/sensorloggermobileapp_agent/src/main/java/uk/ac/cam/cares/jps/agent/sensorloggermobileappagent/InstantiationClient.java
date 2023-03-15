package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelContext;

import java.util.*;

//Accessagent needs to be running with proper JSON Routing
public class InstantiationClient {
    public static void instantiationMethod(HashMap IRI) {


//        /**
//         * For stack
//         */
//        // Create context to work in, and also clear any old existing data
//        ModelContext context = new ModelContext("http://test-access-agent:8080/test");

        /**
         * For local
         */
        ModelContext context = new ModelContext("http://localhost:48888/test");

        /**
         * Creating instances and randomUUID.
         */

        //Person
        Person person = context.createNewModel(Person.class, "https://www.theworldavatar.com/kg/sensorloggerapp/person_"+UUID.randomUUID());

        //Smartphone
        Smartphone smartphone = context.createNewModel(Smartphone.class, IRI.get("DEVICEID").toString());

        //Ontodevice:Sensors
        Accelerometer accelerometer= context.createNewModel(Accelerometer.class, "https://www.theworldavatar.com/kg/sensorloggerapp/accelerometer_"+UUID.randomUUID());
        Camera camera = context.createNewModel(Camera.class, "https://www.theworldavatar.com/kg/sensorloggerapp/camera_"+UUID.randomUUID());
        Magnetometer magnetometer = context.createNewModel(Magnetometer.class, "https://www.theworldavatar.com/kg/sensorloggerapp/magnetometer_"+UUID.randomUUID());
        GravitySensor gravitySensor = context.createNewModel(GravitySensor.class, "https://www.theworldavatar.com/kg/sensorloggerapp/gravitySensor_"+UUID.randomUUID());
        GPSSensor gpsSensor = context.createNewModel(GPSSensor.class, "https://www.theworldavatar.com/kg/sensorloggerapp/gpsDevice_"+UUID.randomUUID());
        Microphone microphone = context.createNewModel(Microphone.class, "https://www.theworldavatar.com/kg/sensorloggerapp/microphone_"+UUID.randomUUID());

        
        //Measured classes instances
        Accel_x accel_x = context.createNewModel(Accel_x.class, "https://www.theworldavatar.com/kg/sensorloggerapp/accel_x_"+UUID.randomUUID());
        Accel_y accel_y = context.createNewModel(Accel_y.class, "https://www.theworldavatar.com/kg/sensorloggerapp/accel_y_"+UUID.randomUUID());
        Accel_z accel_z = context.createNewModel(Accel_z.class, "https://www.theworldavatar.com/kg/sensorloggerapp/accel_z_"+UUID.randomUUID());
        Gravity_x gravity_x = context.createNewModel(Gravity_x.class, "https://www.theworldavatar.com/kg/sensorloggerapp/gravity_x_"+UUID.randomUUID());
        Gravity_y gravity_y = context.createNewModel(Gravity_y.class, "https://www.theworldavatar.com/kg/sensorloggerapp/gravity_y_"+UUID.randomUUID());
        Gravity_z gravity_z = context.createNewModel(Gravity_z.class, "https://www.theworldavatar.com/kg/sensorloggerapp/gravity_z_"+UUID.randomUUID());
        Magnetometer_x magnetometer_x = context.createNewModel(Magnetometer_x.class, "https://www.theworldavatar.com/kg/sensorloggerapp/magnetometer_x_"+UUID.randomUUID());
        Magnetometer_y magnetometer_y = context.createNewModel(Magnetometer_y.class, "https://www.theworldavatar.com/kg/sensorloggerapp/magnetometer_y_"+UUID.randomUUID());
        Magnetometer_z magnetometer_z = context.createNewModel(Magnetometer_z.class, "https://www.theworldavatar.com/kg/sensorloggerapp/magnetometer_z_"+UUID.randomUUID());
        Acceleration acceleration = context.createNewModel(Acceleration.class, "https://www.theworldavatar.com/kg/sensorloggerapp/acceleration_"+UUID.randomUUID());
        MagneticFluxDensity magneticFluxDensity = context.createNewModel(MagneticFluxDensity.class, "https://www.theworldavatar.com/kg/sensorloggerapp/MagneticFluxDensity_"+UUID.randomUUID());
        Speed speed = context.createNewModel(Speed.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Speed_"+UUID.randomUUID());
        Bearing bearing = context.createNewModel(Bearing.class, "https://www.theworldavatar.com/kg/sensorloggerapp/bearing_"+UUID.randomUUID());
        Altitude altitude = context.createNewModel(Altitude.class, "https://www.theworldavatar.com/kg/sensorloggerapp/altitude_"+UUID.randomUUID());

        SoundPressureLevel soundPressureLevel = context.createNewModel(SoundPressureLevel.class, "https://www.theworldavatar.com/kg/sensorloggerapp/SoundPressureLevel_"+UUID.randomUUID());
        Illuminance illuminance = context.createNewModel(Illuminance.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Illuminance_"+UUID.randomUUID());

        /**
         * Input hashmap of instantiated measureIRIList for the measureclass
         */

        Measure measure_accel_x = context.createNewModel(Measure.class, IRI.get("measure_accel_x").toString());
        Measure measure_accel_y = context.createNewModel(Measure.class, IRI.get("measure_accel_y").toString());
        Measure measure_accel_z = context.createNewModel(Measure.class, IRI.get("measure_accel_z").toString());

        Measure measure_gravity_x = context.createNewModel(Measure.class, IRI.get("measure_gravity_x").toString());
        Measure measure_gravity_y = context.createNewModel(Measure.class, IRI.get("measure_gravity_y").toString());
        Measure measure_gravity_z = context.createNewModel(Measure.class, IRI.get("measure_gravity_z").toString());

        Measure measure_magnetometer_x = context.createNewModel(Measure.class, IRI.get("measure_magnetometer_x").toString());
        Measure measure_magnetometer_y = context.createNewModel(Measure.class, IRI.get("measure_magnetometer_y").toString());
        Measure measure_magnetometer_z = context.createNewModel(Measure.class, IRI.get("measure_magnetometer_z").toString());

        Measure measure_speed = context.createNewModel(Measure.class, IRI.get("measure_speed").toString());

        Point point = context.createNewModel(Point.class, "http://www.opengis.net/ont/sf#point_"+IRI.get("measure_geom_location").toString());

        Measure measure_bearing = context.createNewModel(Measure.class, IRI.get("measure_bearing").toString());
        Measure measure_altitude = context.createNewModel(Measure.class, IRI.get("measure_altitude").toString());

        Measure measure_dbfs = context.createNewModel(Measure.class, IRI.get("measure_dbfs").toString());
        Measure measure_light_value = context.createNewModel(Measure.class, IRI.get("measure_light_value").toString());

        /**
         * Creating context for rdfType
         */
        PersonRDF personRDF = context.createNewModel(PersonRDF.class, "https://w3id.org/MON/person.owl/");

        //Smartphone
        SmartphoneRDF smartphoneRDF = context.createNewModel(SmartphoneRDF.class,"https://theworldavatar.com/kg/ontodevice/Smartphone");

        //Ontodevice:Sensors
        AccelerometerRDF accelerometerRDF= context.createNewModel(AccelerometerRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Accelerometer");
        CameraRDF cameraRDF = context.createNewModel(CameraRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Camera");
        MagnetometerRDF magnetometerRDF = context.createNewModel(MagnetometerRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Magnetometer");
        GravitySensorRDF gravitySensorRDF = context.createNewModel(GravitySensorRDF.class, "https://www.theworldavatar.com/kg/ontodevice/GravitySensor");
        GPSSensorRDF gpsSensorRDF = context.createNewModel(GPSSensorRDF.class, "https://www.theworldavatar.com/kg/ontodevice/GPSDevice");
        MicrophoneRDF microphoneRDF = context.createNewModel(MicrophoneRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Microphone");

        //Measured classes instances
        Accel_xRDF accel_xRDF = context.createNewModel(Accel_xRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Accel_x");
        Accel_yRDF accel_yRDF = context.createNewModel(Accel_yRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Accel_y");
        Accel_zRDF accel_zRDF = context.createNewModel(Accel_zRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Accel_z");
        Gravity_xRDF gravity_xRDF = context.createNewModel(Gravity_xRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Gravity_x");
        Gravity_yRDF gravity_yRDF = context.createNewModel(Gravity_yRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Gravity_y");
        Gravity_zRDF gravity_zRDF = context.createNewModel(Gravity_zRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Gravity_z");
        Magnetometer_xRDF magnetometer_xRDF = context.createNewModel(Magnetometer_xRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Magnetometer_x");
        Magnetometer_yRDF magnetometer_yRDF = context.createNewModel(Magnetometer_yRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Magnetometer_y");
        Magnetometer_zRDF magnetometer_zRDF = context.createNewModel(Magnetometer_zRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Magnetometer_z");
        AccelerationRDF accelerationRDF = context.createNewModel(AccelerationRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Acceleration");
        MagneticFluxDensityRDF magneticFluxDensityRDF = context.createNewModel(MagneticFluxDensityRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/MagneticFluxDensity");
        SpeedRDF speedRDF = context.createNewModel(SpeedRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Speed");
        BearingRDF bearingRDF = context.createNewModel(BearingRDF.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/Bearing");
        AltitudeRDF altitudeRDF = context.createNewModel(AltitudeRDF.class, "https://www.theworldavatar.com/kg/ontoGeo/Altitude");
        PointRDF pointRDF = context.createNewModel(PointRDF.class, "http://www.opengis.net/ont/sf#Point");
        SoundPressureLevelRDF soundPressureLevelRDF = context.createNewModel(SoundPressureLevelRDF.class, "https://www.theworldavatar.com/kg/ontouom/SoundPressureLevel");
        IlluminanceRDF illuminanceRDF = context.createNewModel(IlluminanceRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Illuminance");


        /**
         * Creating context for unitclasses
         */
        
        Unit metrePerSecond_TimeSquared = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/metrePerSecond_TimeSquared");
        Unit metre = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/metre");
        Unit lux = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/lux");
        Unit microtesla = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/microtesla");
        Unit metrePerSecond_Time = context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/om:metrePerSecond-Time");
        Unit decibelsRelativeToFullScale = context.createNewModel(Unit.class, "https://www.theworldavatar.com/kg/ontouom/decibelsRelativeToFullScale");
        Unit degree= context.createNewModel(Unit.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/degree");

        /**
         * Person class
         */
        person.smartphone=smartphone;
        personRDF.person=person;

        /**
         * Smartphone class
         */
        smartphone.person=person;
        smartphoneRDF.smartphone=smartphone;

        smartphone.devices.add(camera);
        cameraRDF.camera=camera;

        smartphone.devices.add(accelerometer);
        accelerometerRDF.accelerometer=accelerometer;

        smartphone.devices.add(magnetometer);
        magnetometerRDF.magnetometer=magnetometer;

        smartphone.devices.add(gpsSensor);
        gpsSensorRDF.gpsSensor=gpsSensor;

        smartphone.devices.add(microphone);
        microphoneRDF.microphone=microphone;

        smartphone.devices.add(gravitySensor);
        gravitySensorRDF.gravitySensor=gravitySensor;

        /**
         * Accelerometer class
         */
        accel_x.accelerometer=accelerometer;
        accel_y.accelerometer=accelerometer;
        accel_z.accelerometer=accelerometer;
        accel_xRDF.accel_x=accel_x;
        accel_yRDF.accel_y=accel_y;
        accel_zRDF.accel_z=accel_z;

        acceleration.accelerationVariables.add(accel_x);
        acceleration.accelerationVariables.add(accel_y);
        acceleration.accelerationVariables.add(accel_z);

        accelerationRDF.acceleration=acceleration;

        /**
         * GravitySensor class
         */
        gravity_x.gravitysensor=gravitySensor;
        gravity_y.gravitysensor=gravitySensor;
        gravity_z.gravitysensor=gravitySensor;
        gravity_xRDF.gravity_x=gravity_x;
        gravity_yRDF.gravity_y=gravity_y;
        gravity_zRDF.gravity_z=gravity_z;

        acceleration.accelerationVariables.add(gravity_x);
        acceleration.accelerationVariables.add(gravity_y);
        acceleration.accelerationVariables.add(gravity_z);
        /**
         * Magnetometer class
         */
        magnetometer_x.magnetometer=magnetometer;
        magnetometer_y.magnetometer=magnetometer;
        magnetometer_z.magnetometer=magnetometer;
        magnetometer_xRDF.magnetometer_x=magnetometer_x;
        magnetometer_yRDF.magnetometer_y=magnetometer_y;
        magnetometer_zRDF.magnetometer_z=magnetometer_z;

        magneticFluxDensity.magneticFluxDensityVariables.add(magnetometer_x);
        magneticFluxDensity.magneticFluxDensityVariables.add(magnetometer_y);
        magneticFluxDensity.magneticFluxDensityVariables.add(magnetometer_z);
        magneticFluxDensityRDF.magneticFluxDensity=magneticFluxDensity;

        /**
         * GPSSensor
         */
        gpsSensor.gpsSensorVariables.add(speed);
        speedRDF.speed=speed;
        gpsSensor.point.add(point);
        pointRDF.point=point;

        gpsSensor.gpsSensorVariables.add(altitude);
        altitudeRDF.altitude=altitude;
        gpsSensor.gpsSensorVariables.add(bearing);
        bearingRDF.bearing=bearing;

        /**
         * Camera class
         */
        camera.illuminance=illuminance;
        illuminance.camera=camera;
        illuminanceRDF.illuminance=illuminance;


        /**
         * Microphone class
         */
        soundPressureLevel.microphone=microphone;
        microphone.soundPressureLevel=soundPressureLevel;
        soundPressureLevelRDF.soundPressureLevel=soundPressureLevel;

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
        decibelsRelativeToFullScale.unitsInstant.add(measure_dbfs);
        metrePerSecond_Time.unitsInstant.add(measure_speed);
        degree.unitsInstant.add(measure_bearing);
        metre.unitsInstant.add(measure_altitude);

        metrePerSecond_TimeSquared.symbolInstant = "m/s2";
        microtesla.symbolInstant = "&#x00B5;T";
        lux.symbolInstant = "lx";
        decibelsRelativeToFullScale.symbolInstant = "dBFS";
        metrePerSecond_Time.symbolInstant = "m/s";
        degree.symbolInstant = "&#x00B0;";
        metre.symbolInstant = "m";

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
    public static class Person extends SensorLoggerModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/hasA")
        protected InstantiationClient.Smartphone smartphone;
    }
    public static class PersonRDF extends SensorLoggerModel {
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Person person;
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
    public static class SmartphoneRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Smartphone smartphone;
    }




    /**
     * Accelerometer branch
     */
    public static class Accelerometer extends OntoDeviceModel{}
    public static class AccelerometerRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Accelerometer accelerometer;
    }
    
    public static class Accel_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }
    public static class Accel_xRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Accel_x accel_x;
    }

    public static class Accel_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }
    public static class Accel_yRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Accel_y accel_y;
    }
    public static class Accel_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }
    public static class Accel_zRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Accel_z accel_z;
    }


    /**
     * Gravity Branch
     */
    public static class GravitySensor extends OntoDeviceModel{}
    public static class GravitySensorRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.GravitySensor gravitySensor;
    }
    public static class Gravity_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }
    public static class Gravity_xRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Gravity_x gravity_x;
    }

    public static class Gravity_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }
    public static class Gravity_yRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Gravity_y gravity_y;
    }
    
    public static class Gravity_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }
    public static class Gravity_zRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Gravity_z gravity_z;
    }


    /**
     * Magnetometer Branch
     */

    public static class Magnetometer extends OntoDeviceModel{}
    public static class MagnetometerRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Magnetometer magnetometer;
    }
    public static class Magnetometer_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel magnetometer;
    }
    public static class Magnetometer_xRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Magnetometer_x magnetometer_x;
    }
    
    public static class Magnetometer_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel magnetometer;
    }
    public static class Magnetometer_yRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Magnetometer_y magnetometer_y;
    }

    public static class Magnetometer_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel magnetometer;
    }
    public static class Magnetometer_zRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Magnetometer_z magnetometer_z;
    }

    /**
     * Camera branch
     */
    public static class Camera extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures")
        protected Illuminance illuminance;
    }
    public static class CameraRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Camera camera;
    }


    /**
     * GPSSensor branch
     */


    public static class GPSSensor extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> gpsSensorVariables;

        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/hasGeoLocation", innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> point;

    }
    public static class GPSSensorRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.GPSSensor gpsSensor;
    }

    /**
     * Microphone branch
     */

    public static class Microphone extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures")
        protected SoundPressureLevel soundPressureLevel;
    }

    public static class MicrophoneRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Microphone microphone;
    }




    public static class Acceleration extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",innerType = SensorLoggerModel.class,backward = true)
        protected ArrayList<SensorLoggerModel> accelerationVariables;
    }

    public static class AccelerationRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Acceleration acceleration;
    }




    public static class MagneticFluxDensity extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",innerType = SensorLoggerModel.class,backward = true)
        protected ArrayList<SensorLoggerModel> magneticFluxDensityVariables;
    }
    public static class MagneticFluxDensityRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.MagneticFluxDensity magneticFluxDensity;
    }

    public static class Speed extends SensorLoggerModel{}
    public static class SpeedRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Speed speed;
    }
    public static class Bearing extends SensorLoggerModel{}
    public static class BearingRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Bearing bearing;
    }

    public static class Point extends SensorLoggerModel{}
    public static class PointRDF extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Point point;
    }

    public static class Altitude extends SensorLoggerModel{}
    public static class AltitudeRDF extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected InstantiationClient.Altitude altitude;
    }
    public static class SoundPressureLevel extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures",backward = true)
        protected Microphone microphone;
    }
    public static class SoundPressureLevelRDF extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",backward = true)
        protected SoundPressureLevel soundPressureLevel;
    }


    public static class Illuminance extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected Camera camera;
    }
    public static class IlluminanceRDF extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",backward = true)
        protected Illuminance illuminance;
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
        @Getter @Setter @FieldAnnotation(value = "http://www.ontology-of-units-of-measure.org/resource/om-2/symbol")
        protected String symbolInstant;
    }
}
