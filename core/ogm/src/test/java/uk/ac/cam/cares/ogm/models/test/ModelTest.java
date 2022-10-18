package uk.ac.cam.cares.ogm.models.test;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.ModelContext;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;

public class ModelTest {

  @Test
  public void testSetClean() {
    ModelContext context = new ModelContext("", "");
    TestModel newModel = context.createNewModel(TestModel.class, "a");
    newModel.setClean("stringProp");
    assertTrue(newModel.isClean("stringProp"));
    assertFalse(newModel.isClean("doubleProp"));
    assertFalse(newModel.isClean("forwardVector"));
    newModel.setClean();
    assertTrue(newModel.isClean("doubleProp"));
    assertTrue(newModel.isClean("forwardVector"));
  }

  @Test
  public void testSetDirty() {
    ModelContext context = new ModelContext("", "");
    TestModel newModel = context.createNewModel(TestModel.class, "a");
    newModel.setClean();
    assertTrue(newModel.isClean("doubleProp"));
    assertTrue(newModel.isClean("forwardVector"));
    newModel.setDirty("doubleProp");
    assertFalse(newModel.isClean("doubleProp"));
    assertTrue(newModel.isClean("forwardVector"));
    newModel.setDirty();
    assertFalse(newModel.isClean("forwardVector"));
  }

  @Test
  public void testIsClean() {
    ModelContext context = new ModelContext("", "");
    TestModel newModel = context.createNewModel(TestModel.class, "a");
    newModel.setClean();
    assertTrue(newModel.isClean("doubleProp"));
    assertTrue(newModel.isClean("forwardVector"));
    newModel.setDoubleProp(5.2);
    assertFalse(newModel.isClean("doubleProp"));
    newModel.getForwardVector().add(7.77);
    assertFalse(newModel.isClean("forwardVector"));
  }

  @Test
  public void testIsHollow() {
    ModelContext context = new ModelContext("", "");
    TestModel newModel = context.createNewModel(TestModel.class, "a");
    assertFalse(newModel.isHollow("doubleProp"));
    TestModel hollowModel = context.createHollowModel(TestModel.class, "b");
    assertTrue(hollowModel.isHollow("doubleProp"));
    hollowModel.setDoubleProp(5.2);
    assertTrue(hollowModel.isHollow("doubleProp"));
    hollowModel.setClean("doubleProp");
    assertFalse(hollowModel.isHollow("doubleProp"));
  }

  @Test
  public void testClear() {
    ModelContext context = new ModelContext("", "");
    TestModel newModel = context.createNewModel(TestModel.class, "a");
    newModel.setDoubleProp(3.33);
    newModel.getForwardVector().add(7.77);
    assertNotNull(newModel.getDoubleProp());
    assertNotEquals(new ArrayList<Double>(), newModel.getForwardVector());
    newModel.clear("doubleProp");
    assertNull(newModel.getDoubleProp());
    assertNotEquals(new ArrayList<Double>(), newModel.getForwardVector());
    newModel.clear();
    assertNull(newModel.getDoubleProp());
    assertEquals(new ArrayList<Double>(), newModel.getForwardVector());
  }

  @Test
  public void testEquals() {
    ModelContext context1 = new ModelContext("1", "");
    TestModel m1 = context1.createNewModel(TestModel.class, "a");
    ModelContext context2 = new ModelContext("2", "");
    TestModel m2 = context2.createNewModel(TestModel.class, "a");
    assertEquals(m1, m2);
    m1.setDoubleProp(3.33);
    assertNotEquals(m1, m2);
    m2.setDoubleProp(3.33);
    assertEquals(m1, m2);
  }

}
