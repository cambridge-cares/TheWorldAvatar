package uk.ac.cam.cares.ogm.models.test;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.SPARQLUtils;
import uk.ac.cam.cares.ogm.models.FieldKey;
import uk.ac.cam.cares.ogm.models.MetaModel;

import java.io.InvalidClassException;

import static org.junit.Assert.*;

public class MetaModelTest {

  @Test
  public void testConstructor() throws InvalidClassException, NoSuchMethodException {
    MetaModel metaModel = MetaModel.get(TestModel.class);
    // test default graph, forward
    assertTrue(metaModel.fieldMap.containsKey(
        new FieldKey("testmodels", SPARQLUtils.expandQualifiedName("JPSLAND:stringpropnull"), false)));
    // test default graph, backward
    assertTrue(metaModel.fieldMap.containsKey(
        new FieldKey("testmodels", SPARQLUtils.expandQualifiedName("JPSLAND:backuriprop"), true)));
    // test specified graph
    assertTrue(metaModel.fieldMap.containsKey(
        new FieldKey("graph3", SPARQLUtils.expandQualifiedName("dbpediao:graphtest3a"), false)));
    // test number of fields loaded is correct
    assertEquals(19, metaModel.scalarFieldList.size());
    assertEquals(3, metaModel.vectorFieldList.size());
    assertEquals(22, metaModel.fieldMap.size());
  }

}
