package uk.ac.cam.cares.ogm.models.test;

import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldInterface;
import uk.ac.cam.cares.ogm.models.ModelContext;

import java.io.InvalidClassException;
import java.math.BigInteger;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.*;

public class FieldInterfaceTest{

  TestModel model1 = new ModelContext("", "").createNewModel(TestModel.class, "http://m1");
  TestModel model2 = new ModelContext("", "").createNewModel(TestModel.class, "http://m2");

  @Test
  public void testIntegerInterface() throws InvalidClassException, NoSuchFieldException, NoSuchMethodException {
    // Data to write
    this.testScalarInterface("intProp", TestModel::setIntProp, TestModel::getIntProp,
        "1", "http://www.w3.org/2001/XMLSchema#int", 1, 2,
        NodeFactory.createLiteral(String.valueOf(2), XSDDatatype.XSDint), 2);
  }

  @Test
  public void testBigIntegerInterface() throws InvalidClassException, NoSuchFieldException, NoSuchMethodException {
    // Data to write
    this.testScalarInterface("bigIntProp", TestModel::setBigIntProp, TestModel::getBigIntProp,
        "1", "http://www.w3.org/2001/XMLSchema#integer", BigInteger.valueOf(1), BigInteger.valueOf(2),
        NodeFactory.createLiteral(String.valueOf(2), XSDDatatype.XSDinteger), BigInteger.valueOf(2));
  }

  @Test
  public void testDoubleInterface() throws InvalidClassException, NoSuchFieldException, NoSuchMethodException {
    // Data to write
    this.testScalarInterface("doubleProp", TestModel::setDoubleProp, TestModel::getDoubleProp,
        "3.14", "http://www.w3.org/2001/XMLSchema#double", 3.14, 5.1167,
        NodeFactory.createLiteral("5.1167", XSDDatatype.XSDdouble), 5.1167);
  }

  @Test
  public void testStringInterface() throws InvalidClassException, NoSuchFieldException, NoSuchMethodException {
    // Data to write
    this.testScalarInterface("stringProp", TestModel::setStringProp, TestModel::getStringProp,
        "teststring", "http://www.w3.org/2001/XMLSchema#string", "teststring", "test2",
        NodeFactory.createLiteral("test2", XSDDatatype.XSDstring), "test2");
  }

  @Test
  public void testUriInterface() throws InvalidClassException, NoSuchFieldException, NoSuchMethodException {
    // Data to write
    this.testScalarInterface("uriProp", TestModel::setUriProp, TestModel::getUriProp,
        "https://example.com/testuri", "",
        URI.create("https://example.com/testuri"), URI.create("http://example.com/uri2"),
        NodeFactory.createURI("http://example.com/uri2"), "http://example.com/uri2");
  }

  @Test
  public void testModelInterface() throws InvalidClassException, NoSuchFieldException, NoSuchMethodException {
    // Data to write
    TestModel dataModel = new ModelContext("", "").createNewModel(TestModel.class, "http://example.com/testmodel");
    TestModel secondModel = new ModelContext("", "").createNewModel(TestModel.class, "http://example.com/model2");
    this.testScalarInterface("modelProp", TestModel::setModelProp, TestModel::getModelProp,
        "http://example.com/testmodel", "", dataModel, secondModel,
        NodeFactory.createURI("http://example.com/model2"), "http://example.com/model2");
  }

  @Test
  private <T> void testScalarInterface(
      String fieldName, BiConsumer<TestModel, T> directSetter, Function<TestModel, T> directGetter,
      String valueString, String datatypeString, T dataValue, T secondValue, Node secondValueNode, Object secondValueMinimised)
      throws NoSuchFieldException, InvalidClassException, NoSuchMethodException {
    FieldInterface field = new FieldInterface(TestModel.class.getDeclaredField(fieldName), 0);
    // Test putting
    field.put(model1, valueString, datatypeString);
    assertEquals(dataValue, directGetter.apply(model1));
    // Test equality checks
    assertFalse(field.equals(model1, model2)); // a != null
    assertFalse(field.equals(model2, model1)); // null != a
    directSetter.accept(model2, dataValue);
    assertTrue(field.equals(model1, model2)); // a == a
    directSetter.accept(model2, secondValue);
    assertFalse(field.equals(model1, model2)); // a != b
    directSetter.accept(model1, null);
    directSetter.accept(model2, null);
    assertTrue(field.equals(model1, model2)); // null == null
    // Test clear
    field.clear(model2);
    assertNull(directGetter.apply(model2));
    // Test getNode
    directSetter.accept(model2, secondValue);
    assertEquals(secondValueNode, field.getNodes(model2)[0]);
    assertTrue(field.getNodes(model1)[0].isBlank());
    // Test minimisation
    assertEquals(secondValueMinimised, field.getMinimised(model2));
    assertNull(field.getMinimised(model1));
  }

  @Test
  public void testVectorInterface() throws NoSuchFieldException, InvalidClassException, NoSuchMethodException {
    FieldInterface field = new FieldInterface(TestModel.class.getDeclaredField("forwardVector"), 0);
    // Test putting
    assertEquals(0, model1.getForwardVector().size());
    assertTrue(field.equals(model1, model2));
    field.put(model1, "1.32", "http://www.w3.org/2001/XMLSchema#double");
    assertEquals(1, model1.getForwardVector().size());
    assertEquals(1.32, model1.getForwardVector().get(0));
    assertFalse(field.equals(model1, model2));
    // Test getting nodes
    model1.getForwardVector().add(5.32);
    assertArrayEquals(new Node[]{
        NodeFactory.createLiteral("1.32", XSDDatatype.XSDdouble),
        NodeFactory.createLiteral("5.32", XSDDatatype.XSDdouble)
    }, field.getNodes(model1));
    assertArrayEquals(new Node[0], field.getNodes(model2));
    // Test minimisation
    Set<Double> minimisedModel1 = new HashSet<>();
    minimisedModel1.add(1.32);
    minimisedModel1.add(5.32);
    assertEquals(minimisedModel1, field.getMinimised(model1));
    assertEquals(new HashSet<>(), field.getMinimised(model2));
    // Test equals
    assertTrue(field.equals(model1, model1));
    assertTrue(field.equals(model2, model2));
    assertFalse(field.equals(model1, model2));
    // Test clear
    field.clear(model1);
    assertArrayEquals(new Node[0], field.getNodes(model1));
  }

}
