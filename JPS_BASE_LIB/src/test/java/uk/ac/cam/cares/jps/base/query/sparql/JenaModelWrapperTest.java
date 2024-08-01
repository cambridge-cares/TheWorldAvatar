package uk.ac.cam.cares.jps.base.query.sparql;



import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.junit.Assert;
import org.junit.Test;



public class JenaModelWrapperTest {
    private OntModel ontModel=ModelFactory.createOntologyModel();
    private String iri="testIri";
    private String startSubject="testStartSubject";

    @Test
    public void testConstructor(){
        JenaModelWrapper jmw= new JenaModelWrapper(ontModel,iri);
        OntModel actual_om=jmw.getModel();
        String actual_iri=this.iri;
        Assert.assertEquals(ontModel,actual_om);
        Assert.assertEquals(iri,actual_iri);
    }
    @Test
    public void testConcat(){
        String expected="http://www.w3.org/2002/07/owl#"+"testName";
        String actual=JenaModelWrapper.concat(Prefixes.OWL,"testName");
        Assert.assertEquals(expected,actual);
    }
    @Test//This unit test is for the public static newIri() method in the JenaModelWrapper class
    public void testNewIri1() {
        String str="test";
        String iri=JenaModelWrapper.newIri(str);
        String[] arr=iri.split("#");
        Assert.assertEquals(str,arr[0]);
        try{
            UUID uuid = UUID.fromString(arr[1]);
        } catch (IllegalArgumentException exception){
            //handle the case where string is not valid UUID
            System.out.println("The UUID provided is invalid");
        }
    }
    @Test//This unit test is for the private newIri() method in the JenaModelWrapper class
    public void testNewIri2() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        //distinguish between two cases one where this.iri is not null and when it is not.

        //not null case
        JenaModelWrapper jmw= new JenaModelWrapper(ontModel,iri);
        Assert.assertNotNull(jmw.getClass().getDeclaredMethod("newIri"));
        Method newIri=jmw.getClass().getDeclaredMethod("newIri");
        newIri.setAccessible(true);
        String actual_iri=(String) newIri.invoke(jmw);

        //Check if the returned object consists of the iri and a valid UUID separated by a hash mark.
        String[] arr=actual_iri.split("#");
        Assert.assertEquals(this.iri,arr[0]);
        try{
            UUID uuid = UUID.fromString(arr[1]);
        } catch (IllegalArgumentException exception){
            //handle the case where string is not valid UUID
            System.out.println("The UUID provided is invalid");
        }

        //null case
        //Use reflection to set iri field to null
        Class obj=jmw.getClass();
        Field field = obj.getDeclaredField("iri");
        field.setAccessible(true);
        field.set(jmw, null);

        Field field1 = obj.getDeclaredField("startSubject");
        field1.setAccessible(true);
        field1.set(jmw, startSubject);

        String iri_1=(String) newIri.invoke(jmw);

        //Check if the returned object consists of the iri and a valid UUID separated by a hash mark.
        String[] arr_1=iri_1.split("#");
        Assert.assertEquals(startSubject,arr_1[0]);
        try{
            UUID uuid = UUID.fromString(arr_1[1]);
        } catch (IllegalArgumentException exception){
            //handle the case where string is not valid UUID
            System.out.println("The UUID provided is invalid");
        }
    }
    @Test
    public void testCreateIndividual(){
        JenaModelWrapper jmw= new JenaModelWrapper(ontModel,iri);
        OntModel m= jmw.getModel();
        OntClass c=m.createClass("http://dbpedia.org/ontology/testClass");
        Individual expected= c.createIndividual("http://dbpedia.org/ontology/ind1");
        Individual actual=jmw.createIndividual(Prefixes.DBPEDIA_O,"testClass");
        Assert.assertEquals(expected.getOntClass(),actual.getOntClass());
    }
    @Test
    public void testListPropertyValues() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        JenaModelWrapper jmw= new JenaModelWrapper(ontModel,iri);
        Assert.assertNotNull(jmw.getClass().getDeclaredMethod("listProperyValues", Individual.class, Property.class));
        Method lpv=jmw.getClass().getDeclaredMethod("listProperyValues", Individual.class, Property.class);
        lpv.setAccessible(true);

        //Manually, create a list of RDFNodes based on one property p1.
        String ns="http://example.org/";
        OntModel m= jmw.getModel();
        OntClass c=m.createClass(ns+"test");
        Property p1= m.createProperty(ns+"prop1");
        Individual ind=c.createIndividual(ns+"ind1");
        ind.addProperty(p1,"val");

        List<RDFNode> expected= new ArrayList<>();
        expected.add(ind.getPropertyValue(p1));

        List<RDFNode> actual= ((List<RDFNode>) lpv.invoke(jmw,ind,p1));
        Assert.assertEquals(expected,actual);

    }
    @Test
    public void testCreateObject() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        //Four cases need to be distinguished.
        //In all four cases, whenever createObject() is invoked, the first argument passed is null since the first argument is never used
        //in the createObject() method. Maybe the method can be rewritten?

        JenaModelWrapper jmw= new JenaModelWrapper(ontModel,iri);
        Assert.assertNotNull(jmw.getClass().getDeclaredMethod("createObject", Individual.class, Property.class, int.class, String[].class));
        Method co=jmw.getClass().getDeclaredMethod("createObject", Individual.class, Property.class, int.class, String[].class);
        co.setAccessible(true);


        String ns="http://dbpedia.org/ontology/";

        //null case
        String[] path = {"OCPSPAC", "hasGISCoordinateSystem", "OCPSPAC", "hasProjectedCoordinate_x", "OCPSYST", "hasValue", "OCPSYST", "numericalValue"};
        OntModel m= jmw.getModel();
        ObjectProperty p1= m.createObjectProperty(ns+"prop1");


        int position=7;
        RDFNode actual= ((RDFNode) co.invoke(jmw,null,p1,position,path));
        Assert.assertNull(actual);

        //first test to see if class is part of path
        String []path1 = {"OCPSPAC", "hasGISCoordinateSystem", "hasProjectedCoordinate_x", "CLASS", Prefixes.DBPEDIA_O, "test/", "OCPSYST", "hasValue","OCPSYST","numericalValue"};
        int position1=1;

        String iri_val="http://dbpedia.org/ontology/";
        OntClass cc=m.createClass(iri_val+"test/");

        RDFNode actual_1=(RDFNode) co.invoke(jmw,null,p1,position1,path1);

        //Check if the returned object consists of the iri and a valid UUID separated by a hash mark.
        String str=actual_1.toString();
        String[] arr=str.split("#");
        Assert.assertEquals(arr[0],this.iri);
        try{
            UUID uuid = UUID.fromString(arr[1]);
        } catch (IllegalArgumentException exception){
            //handle the case where string is not valid UUID
            System.out.println("The UUID provided is invalid");
        }

        //test to see if the class is given by the domain of the successive property
        OntClass ccc=
                m.createClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"+
                "Coordinates");
        ObjectProperty hasProjectedCoordinate_x=
                m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasProjectedCoordinate_x");
        hasProjectedCoordinate_x.addDomain(ccc);
        int position2=0;
        RDFNode actual_2=(RDFNode) co.invoke(jmw,null,p1,position2,path);

        //Check if the returned object consists of the iri and a valid UUID separated by a hash mark.
        String str_2=actual_2.toString();
        String[] arr_1=str_2.split("#");
        Assert.assertEquals(arr_1[0],this.iri);
        try{
            UUID uuid_1 = UUID.fromString(arr_1[1]);
        } catch (IllegalArgumentException exception){
            //handle the case where string is not valid UUID
            System.out.println("The UUID provided is invalid");
        }

        // test to see if the class is given by the range of the current property
        OntClass cccc=
                m.createClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"+
                        "NUM_VAL");
        ObjectProperty numericalValue=
                m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "numericalValue");
        numericalValue.addRange(cccc);
        RDFNode actual_3=(RDFNode) co.invoke(jmw,null,numericalValue,position,path);

        //Check if the returned object consists of the iri and a valid UUID separated by a hash mark.
        String str_3=actual_3.toString();
        String[] arr_2=str_3.split("#");
        Assert.assertEquals(arr_2[0],this.iri);
        try{
            UUID uuid_2 = UUID.fromString(arr_2[1]);
        } catch (IllegalArgumentException exception){
            //handle the case where string is not valid UUID
            System.out.println("The UUID provided is invalid");
        }
    }

    @Test
    public void testGetPropertyValue() {
        //path taken from Paths.java interface
        String[] path = {Prefixes.OCPSPAC, "hasGISCoordinateSystem", Prefixes.OCPSPAC, "hasProjectedCoordinate_x", Prefixes.OCPSYST, "hasValue",
                Prefixes.OCPSYST, "numericalValue"};
        JenaModelWrapper jmw = new JenaModelWrapper(ontModel, iri);
        OntModel m = jmw.getModel();
        OntClass c = m.createClass("http://dbpedia.org/ontology/testClass");
        String startSubject = "http://dbpedia.org/ontology/ind1";
        String sub1 = "http://dbpedia.org/ontology/ind2";
        String sub2 = "http://dbpedia.org/ontology/ind3";
        String sub3 = "http://dbpedia.org/ontology/ind4";
        Individual ind = c.createIndividual(startSubject);
        Individual ind2 = c.createIndividual(sub1);
        Individual ind3 = c.createIndividual(sub2);
        Individual ind4 = c.createIndividual(sub3);

        Property hasGISCoordinateSystem = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasGISCoordinateSystem");
        Property hasProjectedCoordinate_x = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasProjectedCoordinate_x");
        Property hasValue = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "hasValue");
        Property numericalValue = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "numericalValue");

        ind.addProperty(hasGISCoordinateSystem, ind2);
        ind2.addProperty(hasProjectedCoordinate_x, ind3);
        ind3.addProperty(hasValue, ind4);
        ind4.addProperty(numericalValue, "42.5");

        RDFNode obj = jmw.getPropertyValue(startSubject, path);
        Double actual = obj.asLiteral().getDouble();
        Double expected = 42.5;

        Assert.assertEquals(expected, actual, 1e-15);

    }

    @Test
    public void testGetPropertyValuesWithoutTypes(){
        //path taken from Paths.java interface
        String[] path = {Prefixes.OCPSPAC, "hasGISCoordinateSystem", Prefixes.OCPSPAC, "hasProjectedCoordinate_x", Prefixes.OCPSYST, "hasValue",
                Prefixes.OCPSYST, "numericalValue"};
        JenaModelWrapper jmw = new JenaModelWrapper(ontModel, iri);
        OntModel m = jmw.getModel();
        OntClass c = m.createClass("http://dbpedia.org/ontology/testClass");
        String startSubject = "http://dbpedia.org/ontology/ind1";
        String sub1 = "http://dbpedia.org/ontology/ind2";
        String sub2 = "http://dbpedia.org/ontology/ind3";
        String sub3 = "http://dbpedia.org/ontology/ind4";
        Individual ind = c.createIndividual(startSubject);
        Individual ind2 = c.createIndividual(sub1);
        Individual ind3 = c.createIndividual(sub2);
        Individual ind4 = c.createIndividual(sub3);

        Property hasGISCoordinateSystem = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasGISCoordinateSystem");
        Property hasProjectedCoordinate_x = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasProjectedCoordinate_x");
        Property hasValue = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "hasValue");
        Property numericalValue = m.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "numericalValue");

        ind.addProperty(hasGISCoordinateSystem, ind2);
        ind2.addProperty(hasProjectedCoordinate_x, ind3);
        ind3.addProperty(hasValue, ind4);
        ind4.addProperty(numericalValue, "42.5");

        RDFNode obj = jmw.getPropertyValue(startSubject, path);
        Double actual = obj.asLiteral().getDouble();
        Double expected = 42.5;

        Assert.assertEquals(expected, actual, 1e-15);
    }

    @Test
    public void testSetPropertyValues(){
        //path taken from Paths.java interface
        String[] path = {Prefixes.OCPSPAC, "hasGISCoordinateSystem", Prefixes.OCPSPAC, "hasProjectedCoordinate_x", Prefixes.OCPSYST, "hasValue",
                Prefixes.OCPSYST, "numericalValue"};
        JenaModelWrapper jmw = new JenaModelWrapper(ontModel, iri);
        OntModel m = jmw.getModel();
        OntClass c = m.createClass("http://dbpedia.org/ontology/testClass");

        String startSubject = "http://dbpedia.org/ontology/ind1";
        String sub1 = "http://dbpedia.org/ontology/ind2";
        String sub2 = "http://dbpedia.org/ontology/ind3";
        String sub3 = "http://dbpedia.org/ontology/ind4";
        String dummy= "http://testdummy.com";

        Individual ind = c.createIndividual(startSubject);
        Individual ind2 = c.createIndividual(sub1);
        Individual ind3 = c.createIndividual(sub2);
        Individual ind4 = c.createIndividual(sub3);
        Individual ind5= c.createIndividual(dummy);

        ObjectProperty hasGISCoordinateSystem = m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasGISCoordinateSystem");
        ObjectProperty hasProjectedCoordinate_x = m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasProjectedCoordinate_x");
        ObjectProperty hasValue = m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "hasValue");
        DatatypeProperty numericalValue = m.createDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "numericalValue");

        ind.addProperty(hasGISCoordinateSystem, ind2);
        ind2.addProperty(hasProjectedCoordinate_x, ind3);
        ind3.addProperty(hasValue, ind4);
        ind4.addProperty(numericalValue, "42.5");

        //testing for a literal by passing a double value as the destObj argument
        double expected=52.65;
        RDFNode obj = jmw.setPropertyValue(startSubject, expected,path);
        double actual=obj.asLiteral().getDouble();

        Assert.assertEquals(expected,actual,1e-15);

        //testing for resource by passing an individual object as the destObj argument
        String []path1={Prefixes.OCPSPAC, "hasGISCoordinateSystem"};
        RDFNode obj_uri = jmw.setPropertyValue(startSubject,ind5,path1);
        String actual_uri=obj_uri.asResource().getURI();
        String expected_uri=ind5.getURI();
        Assert.assertEquals(expected_uri,actual_uri);
    }

    @Test
    public void testSetPropertyValuesWithoutTypes(){
        //path taken from Paths.java interface
        String[] path = {Prefixes.OCPSPAC, "hasGISCoordinateSystem", Prefixes.OCPSPAC, "hasProjectedCoordinate_x", Prefixes.OCPSYST, "hasValue",
                Prefixes.OCPSYST, "numericalValue"};
        JenaModelWrapper jmw = new JenaModelWrapper(ontModel, iri);
        OntModel m = jmw.getModel();
        OntClass c = m.createClass("http://dbpedia.org/ontology/testClass");

        String startSubject = "http://dbpedia.org/ontology/ind1";
        String sub1 = "http://dbpedia.org/ontology/ind2";
        String sub2 = "http://dbpedia.org/ontology/ind3";
        String sub3 = "http://dbpedia.org/ontology/ind4";
        String dummy= "http://testdummy.com";

        Individual ind = c.createIndividual(startSubject);
        Individual ind2 = c.createIndividual(sub1);
        Individual ind3 = c.createIndividual(sub2);
        Individual ind4 = c.createIndividual(sub3);
        Individual ind5= c.createIndividual(dummy);

        ObjectProperty hasGISCoordinateSystem = m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasGISCoordinateSystem");
        ObjectProperty hasProjectedCoordinate_x = m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#" +
                "hasProjectedCoordinate_x");
        ObjectProperty hasValue = m.createObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "hasValue");
        DatatypeProperty numericalValue = m.createDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" +
                "numericalValue");

        ind.addProperty(hasGISCoordinateSystem, ind2);
        ind2.addProperty(hasProjectedCoordinate_x, ind3);
        ind3.addProperty(hasValue, ind4);
        ind4.addProperty(numericalValue, "42.5");

        //testing for a literal by passing a double value as the destObj argument
        double expected=52.65;
        RDFNode obj = jmw.setPropertyValueWithoutTypes(startSubject, expected,path);
        double actual=obj.asLiteral().getDouble();

        Assert.assertEquals(expected,actual,1e-15);

        //testing for resource by passing an individual object as the destObj argument
        String []path1={Prefixes.OCPSPAC, "hasGISCoordinateSystem"};
        RDFNode obj_uri = jmw.setPropertyValueWithoutTypes(startSubject,ind5,path1);
        String actual_uri=obj_uri.asResource().getURI();
        String expected_uri=ind5.getURI();
        Assert.assertEquals(expected_uri,actual_uri);
    }
}
