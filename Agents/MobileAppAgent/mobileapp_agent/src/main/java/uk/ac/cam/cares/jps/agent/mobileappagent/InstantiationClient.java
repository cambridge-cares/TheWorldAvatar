package uk.ac.cam.cares.jps.agent.mobileappagent;

import lombok.Getter;
import lombok.Setter;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelContext;

import java.util.ArrayList;
import java.util.UUID;


public class InstantiationClient {

    public static void main(String[] args) throws ParseException {
        foundCompany();
    }

    public static void foundCompany() {

        // Create context to work in, and also clear any old existing data
        ModelContext context = new ModelContext("http://localhost:48888/test");
        context.update("CLEAR ALL");

        Smartphone smartphone = context.createNewModel(Smartphone.class, "https://www.theworldavatar.com/kg/ontodevice/smartphone_"+UUID.randomUUID());

        Person person = context.createNewModel(Person.class, "https://w3id.org/MON/person.owl/"+UUID.randomUUID());

        Accelerometer accelerometer= context.createNewModel(Accelerometer.class, "https://www.theworldavatar.com/kg/ontodevice/accelerometer_"+UUID.randomUUID());

        Camera camera = context.createNewModel(Camera.class, "https://www.theworldavatar.com/kg/ontodevice/camera_"+UUID.randomUUID());
        Magnetometer magnetometer = context.createNewModel(Magnetometer.class, "https://www.theworldavatar.com/kg/ontodevice/magnetometer_"+UUID.randomUUID());
        GravitySensor gravitySensor = context.createNewModel(GravitySensor.class, "https://www.theworldavatar.com/kg/ontodevice/gravitySensor_"+UUID.randomUUID());
        GPSSensor gpsSensor = context.createNewModel(GPSSensor.class, "https://www.theworldavatar.com/kg/ontodevice/gpsSensor_"+UUID.randomUUID());
        Microphone microphone = context.createNewModel(Microphone.class, "https://www.theworldavatar.com/kg/ontodevice/microphone_"+UUID.randomUUID());

        Accel_x accel_x = context.createNewModel(Accel_x.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/accel_x_"+UUID.randomUUID());
        Accel_y accel_y = context.createNewModel(Accel_y.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/accel_y_"+UUID.randomUUID());
        Accel_z accel_z = context.createNewModel(Accel_z.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/accel_z_"+UUID.randomUUID());
        Gravity_x gravity_x = context.createNewModel(Gravity_x.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/gravity_x_"+UUID.randomUUID());
        Gravity_y gravity_y = context.createNewModel(Gravity_y.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/gravity_y_"+UUID.randomUUID());
        Gravity_z gravity_z = context.createNewModel(Gravity_z.class, "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/gravity_z_"+UUID.randomUUID());



        Quantity quantity = context.createNewModel(Quantity.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Quantity_"+UUID.randomUUID());

        Acceleration acceleration = context.createNewModel(Acceleration.class, "http://www.ontology-of-units-of-measure.org/resource/om-2/Acceleration_"+UUID.randomUUID());

        

        person.smartphone=smartphone;
        smartphone.person=person;
        smartphone.devices.add(camera);
        smartphone.devices.add(accelerometer);
        smartphone.devices.add(magnetometer);
        smartphone.devices.add(gpsSensor);
        smartphone.devices.add(microphone);
        smartphone.devices.add(gravitySensor);

        accel_x.accelerometer=accelerometer;
        accel_y.accelerometer=accelerometer;
        accel_z.accelerometer=accelerometer;

        gravity_x.gravitysensor=gravitySensor;
        gravity_y.gravitysensor=gravitySensor;
        gravity_z.gravitysensor=gravitySensor;

        quantity.quantities.add(acceleration);



        context.pushAllChanges();
    }

    private static class OntoDeviceModel extends Model {

    }

    private static class OMModel extends Model {

    }

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
    public static class Accelerometer extends OntoDeviceModel{
    }
    public static class Accel_x extends Model{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }

    public static class Accel_y extends Model{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }

    public static class Accel_z extends Model{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel accelerometer;
    }

    public static class GravitySensor extends OntoDeviceModel {
    }

    public static class Gravity_x extends Model{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }

    public static class Gravity_y extends Model{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }

    public static class Gravity_z extends Model{
        @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontodevice/measures", backward = true)
        protected InstantiationClient.OntoDeviceModel gravitysensor;
    }

    public static class Quantity extends OMModel{

        @Getter @Setter @FieldAnnotation(value = "http://www.w3.org/1999/02/22-rdf-syntax-ns#", innerType = OMModel.class)
        protected ArrayList<OMModel> quantities;




    }



    public static class MagneticFluxDensity extends Quantity{
    }

    public static class Speed extends Quantity{
    }

    public static class SoundPressureLevel extends Quantity{
    }

    public static class Bearing extends Quantity{
    }







    





    public static class Camera extends OntoDeviceModel {
    }

    public static class Magnetometer extends OntoDeviceModel{
    }



    public static class GPSSensor extends OntoDeviceModel {
    }

    public static class Microphone extends OntoDeviceModel {

    }
    



}
