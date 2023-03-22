package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelContext;

import java.util.*;

//Accessagent needs to be running with proper JSON Routing
public class StaticInstantiation {
    public static void instantiationMethod(HashMap IRI) {


        String BASEURI = "https://www.theworldavatar.com/kg/sensorloggerapp/";
        String DEVICEID=IRI.get("deviceID").toString();


       /**
        * For stack
        */
       // Create context to work in, and also clear any old existing data
       ModelContext context = new ModelContext("http://test-access-agent:8080/test");

        // /**
        //  * For local
        //  */
        // ModelContext context = new ModelContext("http://localhost:48888/test");

        /**
         * Creating instances and randomUUID.
         */

        //Person
        Person person = context.createNewModel(Person.class, "https://www.theworldavatar.com/kg/sensorloggerapp/person_"+UUID.randomUUID());

        //Smartphone
        Smartphone smartphone = context.createNewModel(Smartphone.class,BASEURI+"Smartphone_"+DEVICEID);

        //Ontodevice:Sensors
        Accelerometer accelerometer= context.createNewModel(Accelerometer.class, "https://www.theworldavatar.com/kg/sensorloggerapp/accelerometer_"+UUID.randomUUID());
        Camera camera = context.createNewModel(Camera.class, "https://www.theworldavatar.com/kg/sensorloggerapp/camera_"+UUID.randomUUID());
        Magnetometer magnetometer = context.createNewModel(Magnetometer.class, "https://www.theworldavatar.com/kg/sensorloggerapp/magnetometer_"+UUID.randomUUID());
        GravitySensor gravitySensor = context.createNewModel(GravitySensor.class, "https://www.theworldavatar.com/kg/sensorloggerapp/gravitySensor_"+UUID.randomUUID());
        GPSDevice gpsDevice = context.createNewModel(GPSDevice.class, "https://www.theworldavatar.com/kg/sensorloggerapp/gpsDevice_"+UUID.randomUUID());
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
        MagneticFluxDensity magneticFluxDensity = context.createNewModel(MagneticFluxDensity.class, "https://www.theworldavatar.com/kg/sensorloggerapp/magneticFluxDensity_"+UUID.randomUUID());
        Speed speed = context.createNewModel(Speed.class, "https://www.theworldavatar.com/kg/sensorloggerapp/speed_"+UUID.randomUUID());
        Bearing bearing = context.createNewModel(Bearing.class, "https://www.theworldavatar.com/kg/sensorloggerapp/bearing_"+UUID.randomUUID());
        Altitude altitude = context.createNewModel(Altitude.class, "https://www.theworldavatar.com/kg/sensorloggerapp/altitude_"+UUID.randomUUID());
        RelativeBrightness relativeBrightness = context.createNewModel(RelativeBrightness.class, "https://www.theworldavatar.com/kg/sensorloggerapp/relativeBrightness_"+UUID.randomUUID());
        
        SoundPressureLevel soundPressureLevel = context.createNewModel(SoundPressureLevel.class, "https://www.theworldavatar.com/kg/sensorloggerapp/soundPressureLevel_"+UUID.randomUUID());
        Illuminance illuminance = context.createNewModel(Illuminance.class, "https://www.theworldavatar.com/kg/sensorloggerapp/illuminance_"+UUID.randomUUID());
        Ratio ratio = context.createNewModel(Ratio.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Ratio_"+UUID.randomUUID());
        
        /**
         * Input hashmap of instantiated measureIRIList for the measureclass
         */

        Measure measure_accel_x = context.createNewModel(Measure.class, IRI.get("accel_x").toString());
        Measure measure_accel_y = context.createNewModel(Measure.class, IRI.get("accel_y").toString());
        Measure measure_accel_z = context.createNewModel(Measure.class, IRI.get("accel_z").toString());

        Measure measure_gravity_x = context.createNewModel(Measure.class, IRI.get("gravity_x").toString());
        Measure measure_gravity_y = context.createNewModel(Measure.class, IRI.get("gravity_y").toString());
        Measure measure_gravity_z = context.createNewModel(Measure.class, IRI.get("gravity_z").toString());

        Measure measure_magnetometer_x = context.createNewModel(Measure.class, IRI.get("magnetometer_x").toString());
        Measure measure_magnetometer_y = context.createNewModel(Measure.class, IRI.get("magnetometer_y").toString());
        Measure measure_magnetometer_z = context.createNewModel(Measure.class, IRI.get("magnetometer_z").toString());

        Measure measure_speed = context.createNewModel(Measure.class, IRI.get("speed").toString());

        Point point = context.createNewModel(Point.class, IRI.get("point").toString());

        Measure measure_bearing = context.createNewModel(Measure.class, IRI.get("bearing").toString());
        Measure measure_altitude = context.createNewModel(Measure.class, IRI.get("altitude").toString());

        Measure measure_dbfs = context.createNewModel(Measure.class, IRI.get("dbfs").toString());
        Measure measure_light_value = context.createNewModel(Measure.class, IRI.get("light_value").toString());
        Measure measure_ratioRB = context.createNewModel(Measure.class, IRI.get("relativeBrightness").toString());

        /**
         * Creating context for rdfType
         */
        PersonRDF personRDF = context.createNewModel(PersonRDF.class, "https://w3id.org/MON/person.owl/");

        //Smartphone
        SmartphoneRDF smartphoneRDF = context.createNewModel(SmartphoneRDF.class,"https://www.theworldavatar.com/kg/ontodevice/Smartphone");

        //Ontodevice:Sensors
        AccelerometerRDF accelerometerRDF= context.createNewModel(AccelerometerRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Accelerometer");
        CameraRDF cameraRDF = context.createNewModel(CameraRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Camera");
        MagnetometerRDF magnetometerRDF = context.createNewModel(MagnetometerRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Magnetometer");
        GravitySensorRDF gravitySensorRDF = context.createNewModel(GravitySensorRDF.class, "https://www.theworldavatar.com/kg/ontodevice/GravitySensor");
        GPSDeviceRDF gpsDeviceRDF = context.createNewModel(GPSDeviceRDF.class, "https://www.theworldavatar.com/kg/ontodevice/GPSDevice");
        MicrophoneRDF microphoneRDF = context.createNewModel(MicrophoneRDF.class, "https://www.theworldavatar.com/kg/ontodevice/Microphone");
        RelativeBrightnessRDF relativeBrightnessRDF = context.createNewModel(RelativeBrightnessRDF.class, "https://www.theworldavatar.com/kg/ontodevice/RelativeBrightness");

        //Measured classes instances
        Accel_xRDF accel_xRDF = context.createNewModel(Accel_xRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Accel_x");
        Accel_yRDF accel_yRDF = context.createNewModel(Accel_yRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Accel_y");
        Accel_zRDF accel_zRDF = context.createNewModel(Accel_zRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Accel_z");
        Gravity_xRDF gravity_xRDF = context.createNewModel(Gravity_xRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Gravity_x");
        Gravity_yRDF gravity_yRDF = context.createNewModel(Gravity_yRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Gravity_y");
        Gravity_zRDF gravity_zRDF = context.createNewModel(Gravity_zRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Gravity_z");
        Magnetometer_xRDF magnetometer_xRDF = context.createNewModel(Magnetometer_xRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Magnetometer_x");
        Magnetometer_yRDF magnetometer_yRDF = context.createNewModel(Magnetometer_yRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Magnetometer_y");
        Magnetometer_zRDF magnetometer_zRDF = context.createNewModel(Magnetometer_zRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Magnetometer_z");
        AccelerationRDF accelerationRDF = context.createNewModel(AccelerationRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Acceleration");
        MagneticFluxDensityRDF magneticFluxDensityRDF = context.createNewModel(MagneticFluxDensityRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/MagneticFluxDensity");
        SpeedRDF speedRDF = context.createNewModel(SpeedRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Speed");
        BearingRDF bearingRDF = context.createNewModel(BearingRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Bearing");
        AltitudeRDF altitudeRDF = context.createNewModel(AltitudeRDF.class, "https://www.theworldavatar.com/kg/sensorloggerapp/Altitude");
        PointRDF pointRDF = context.createNewModel(PointRDF.class, "http://www.opengis.net/ont/sf#Point");
        SoundPressureLevelRDF soundPressureLevelRDF = context.createNewModel(SoundPressureLevelRDF.class, "https://www.theworldavatar.com/kg/ontouom/SoundPressureLevel");
        IlluminanceRDF illuminanceRDF = context.createNewModel(IlluminanceRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Illuminance");
        RatioRDF ratioRDF = context.createNewModel(RatioRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Ratio");
        MeasureRDF measureRDF = context.createNewModel(MeasureRDF.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");


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

        smartphone.DeviceIDString=DEVICEID;

        smartphone.devices.add(camera);
        cameraRDF.camera=camera;

        smartphone.devices.add(accelerometer);
        accelerometerRDF.accelerometer=accelerometer;

        smartphone.devices.add(magnetometer);
        magnetometerRDF.magnetometer=magnetometer;

        smartphone.devices.add(gpsDevice);
        gpsDeviceRDF.gpsDevice=gpsDevice;

        smartphone.devices.add(microphone);
        microphoneRDF.microphone=microphone;

        smartphone.devices.add(gravitySensor);
        gravitySensorRDF.gravitySensor=gravitySensor;

        smartphone.relativeBrightness=relativeBrightness;
        relativeBrightnessRDF.relativeBrightness=relativeBrightness;

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
         * GPSDevice
         */
        gpsDevice.gpsDeviceVariables.add(speed);
        speedRDF.speed=speed;
        gpsDevice.point.add(point);
        pointRDF.point=point;

        gpsDevice.gpsDeviceVariables.add(altitude);
        altitudeRDF.altitude=altitude;
        gpsDevice.gpsDeviceVariables.add(bearing);
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
         * RelativeBrightness class
         */
        relativeBrightness.ratio=ratio;
        ratioRDF.ratio=ratio;


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
        measure_ratioRB.measures.add(ratio);
        measure_light_value.measures.add(illuminance);


        //Link to measureRDF
        measureRDF.measuresRDF.add(measure_accel_x);
        measureRDF.measuresRDF.add(measure_accel_y);
        measureRDF.measuresRDF.add(measure_accel_z);
        measureRDF.measuresRDF.add(measure_gravity_x);
        measureRDF.measuresRDF.add(measure_gravity_y);
        measureRDF.measuresRDF.add(measure_gravity_z);
        measureRDF.measuresRDF.add(measure_magnetometer_x);
        measureRDF.measuresRDF.add(measure_magnetometer_y);
        measureRDF.measuresRDF.add(measure_magnetometer_z);
        measureRDF.measuresRDF.add(measure_speed);
        measureRDF.measuresRDF.add(measure_bearing);
        measureRDF.measuresRDF.add(measure_altitude);
        measureRDF.measuresRDF.add(measure_dbfs);
        measureRDF.measuresRDF.add(measure_ratioRB);
        measureRDF.measuresRDF.add(measure_ratioRB);

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
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/sensorloggerapp/hasA")
        protected StaticInstantiation.Smartphone smartphone;
    }
    public static class PersonRDF extends SensorLoggerModel {
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Person person;
    }

    
    /**
     * Smartphone branch
     */
    public static class Smartphone extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/sensorloggerapp/hasA", backward = true)
        protected StaticInstantiation.Person person;
        @Getter @Setter @FieldAnnotation(value = "https://saref.etsi.org/core/consistsOf", innerType = OntoDeviceModel.class)
        protected ArrayList<OntoDeviceModel> devices;

        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/hasScreenBrightness")
        protected StaticInstantiation.RelativeBrightness relativeBrightness;

        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/hasDeviceID")
        protected String DeviceIDString;
    }
    public static class SmartphoneRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Smartphone smartphone;
    }




    /**
     * Accelerometer branch
     */
    public static class Accelerometer extends OntoDeviceModel{}
    public static class AccelerometerRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Accelerometer accelerometer;
    }
    
    public static class Accel_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel accelerometer;
    }
    public static class Accel_xRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Accel_x accel_x;
    }

    public static class Accel_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel accelerometer;
    }
    public static class Accel_yRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Accel_y accel_y;
    }
    public static class Accel_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel accelerometer;
    }
    public static class Accel_zRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Accel_z accel_z;
    }


    /**
     * Gravity Branch
     */
    public static class GravitySensor extends OntoDeviceModel{}
    public static class GravitySensorRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.GravitySensor gravitySensor;
    }
    public static class Gravity_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel gravitysensor;
    }
    public static class Gravity_xRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Gravity_x gravity_x;
    }

    public static class Gravity_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel gravitysensor;
    }
    public static class Gravity_yRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Gravity_y gravity_y;
    }
    
    public static class Gravity_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel gravitysensor;
    }
    public static class Gravity_zRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Gravity_z gravity_z;
    }


    /**
     * Magnetometer Branch
     */

    public static class Magnetometer extends OntoDeviceModel{}
    public static class MagnetometerRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Magnetometer magnetometer;
    }
    public static class Magnetometer_x extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel magnetometer;
    }
    public static class Magnetometer_xRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Magnetometer_x magnetometer_x;
    }
    
    public static class Magnetometer_y extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel magnetometer;
    }
    public static class Magnetometer_yRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Magnetometer_y magnetometer_y;
    }

    public static class Magnetometer_z extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected StaticInstantiation.OntoDeviceModel magnetometer;
    }
    public static class Magnetometer_zRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Magnetometer_z magnetometer_z;
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
        protected StaticInstantiation.Camera camera;
    }


    /**
     * GPSDevice branch
     */


    public static class GPSDevice extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> gpsDeviceVariables;

        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/hasGeoLocation", innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> point;

    }
    public static class GPSDeviceRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.GPSDevice gpsDevice;
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
        protected StaticInstantiation.Microphone microphone;
    }

    /**
     * RelativeBrightness branch
     */

    public static class RelativeBrightness extends OntoDeviceModel {
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        protected Ratio ratio;
    }

    public static class RelativeBrightnessRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.RelativeBrightness relativeBrightness;
    }





    public static class Acceleration extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",innerType = SensorLoggerModel.class,backward = true)
        protected ArrayList<SensorLoggerModel> accelerationVariables;
    }

    public static class AccelerationRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Acceleration acceleration;
    }




    public static class MagneticFluxDensity extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",innerType = SensorLoggerModel.class,backward = true)
        protected ArrayList<SensorLoggerModel> magneticFluxDensityVariables;
    }
    public static class MagneticFluxDensityRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.MagneticFluxDensity magneticFluxDensity;
    }

    public static class Speed extends SensorLoggerModel{}
    public static class SpeedRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Speed speed;
    }
    public static class Bearing extends SensorLoggerModel{}
    public static class BearingRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Bearing bearing;
    }

    public static class Point extends SensorLoggerModel{}
    public static class PointRDF extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Point point;
    }

    public static class Altitude extends SensorLoggerModel{}
    public static class AltitudeRDF extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Altitude altitude;
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
    public static class Ratio extends SensorLoggerModel{}
    public static class RatioRDF extends OntoDeviceModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", backward = true)
        protected StaticInstantiation.Ratio ratio;
    }

    /**
     * Measure
     */
    public static class Measure extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue",backward = true, innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> measures;
    }

    public static class MeasureRDF extends SensorLoggerModel{
        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",backward = true, innerType = SensorLoggerModel.class)
        protected ArrayList<SensorLoggerModel> measuresRDF;
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
