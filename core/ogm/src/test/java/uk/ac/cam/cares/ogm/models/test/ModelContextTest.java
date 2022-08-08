package uk.ac.cam.cares.ogm.models.test;

import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang.ArrayUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.datatypes.xsd.impl.XSDDouble;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.sparql.core.Quad;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.ogm.models.*;
import uk.ac.cam.cares.ogm.models.SPARQLUtils;
import uk.ac.cam.cares.ogm.models.Model;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;


public class ModelContextTest {

  private static final String testResourceId = "http://localhost:48080/test";
  private static final String testNamespace = "http://localhost:9999/blazegraph/namespace/test/sparql/";

  private static final MetaModel metaModel = MetaModel.get(TestModel.class);

  private int countTriples(ModelContext context) {
    JSONArray response = context.query("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }");
    return response.getJSONObject(0).getInt("count");
  }

  @Test
  public void testNewModelContextFields(){
    ModelContext context = new ModelContext(testResourceId, testNamespace);

    assertEquals(19, context.getClass().getDeclaredFields().length);

    try {
      Field model = context.getClass().getDeclaredField("MODEL");
      model.setAccessible(true);
      assertEquals("model", model.get(context));

      Field graph = context.getClass().getDeclaredField("GRAPH");
      graph.setAccessible(true);
      assertEquals("graph", graph.get(context));

      Field predicate = context.getClass().getDeclaredField("PREDICATE");
      predicate.setAccessible(true);;
      assertEquals("predicate", predicate.get(context));

      Field value = context.getClass().getDeclaredField("VALUE");
      value.setAccessible(true);
      assertEquals("value", value.get(context));

      Field datatype = context.getClass().getDeclaredField("DATATYPE");
      datatype.setAccessible(true);
      assertEquals("datatype", datatype.get(context));

      Field isblank = context.getClass().getDeclaredField("ISBLANK");
      isblank.setAccessible(true);
      assertEquals("isblank", isblank.get(context));

      Field isblank_fun = context.getClass().getDeclaredField("ISBLANK_FUN");
      isblank_fun.setAccessible(true);
      assertEquals("ISBLANK", isblank_fun.get(context));

      Field datatype_fun = context.getClass().getDeclaredField("DATATYPE_FUN");
      datatype_fun.setAccessible(true);
      assertEquals("DATATYPE", datatype_fun.get(context));

      Field QM = context.getClass().getDeclaredField("QM");
      QM.setAccessible(true);
      assertEquals("?", QM.get(context));

      Field OP = context.getClass().getDeclaredField("OP");
      OP.setAccessible(true);
      assertEquals("(", OP.get(context));

      Field CP = context.getClass().getDeclaredField("CP");
      CP.setAccessible(true);
      assertEquals(")", CP.get(context));

      Field Quad_Model_In_Triple_Context_Error_Text = context.getClass().getDeclaredField("QUAD_MODEL_IN_TRIPLE_CONTEXT_ERROR_TEXT");
      Quad_Model_In_Triple_Context_Error_Text.setAccessible(true);
      assertEquals("Quad Model cannot be initialised in triple context.", Quad_Model_In_Triple_Context_Error_Text.get(context));

      Field Object_Not_Found_Exception_Text = context.getClass().getDeclaredField("OBJECT_NOT_FOUND_EXCEPTION_TEXT");
      Object_Not_Found_Exception_Text.setAccessible(true);
      assertEquals("Object not found in database.", Object_Not_Found_Exception_Text.get(context));

      Field Model_Already_Registered_Exception_Text = context.getClass().getDeclaredField("MODEL_ALREADY_REGISTERED_EXCEPTION_TEXT");
      Model_Already_Registered_Exception_Text.setAccessible(true);
      assertEquals("Model already registered for IRI.", Model_Already_Registered_Exception_Text.get(context));

      Field Execution_Character_Threshold = context.getClass().getDeclaredField("EXECUTION_CHARACTER_THRESHOLD");
      Execution_Character_Threshold.setAccessible(true);
      assertEquals(250000, Execution_Character_Threshold.get(context));

    } catch (NoSuchFieldException | IllegalAccessException e) {
      fail();
    }
  }

  @Test
  public void testNewModelContextMethods(){

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    assertEquals(37, context.getClass().getDeclaredMethods().length);
  }

  @Test
  public void testisQuads(){
    ModelContext context = new ModelContext(testResourceId);
    assertFalse(context.isQuads());
    ModelContext context1 = new ModelContext(testResourceId, testNamespace);
    assertTrue(context1.isQuads());
  }

  @Test
  public void testcreatePrototypeModel() {

    ModelContext context = new ModelContext(testResourceId);
    Method createPrototypeModel = null;

    try {
      createPrototypeModel = context.getClass().getDeclaredMethod("createPrototypeModel", Class.class, String.class);
      createPrototypeModel.setAccessible(true);
      createPrototypeModel.invoke(context, TestModel.class, "http://testiri.com/test");
    } catch (Exception e) {
      assertEquals("Quad Model cannot be initialised in triple context.", e.getCause().getMessage());
    }

    ModelContext context1 = new ModelContext(testResourceId, testNamespace);
    try {
      TestModel testModel = (TestModel) createPrototypeModel.invoke(context1, TestModel.class, "http://testiri.com/test");
      assertEquals("http://testiri.com/test", testModel.getIri());
      assertEquals(context1, testModel.getContext());
      assertTrue(context1.members.containsValue(testModel));
    } catch (InvocationTargetException | IllegalAccessException e) {
      fail();
    }

    try{
      createPrototypeModel.invoke(context1, TestModel.class, "http://testiri.com/test");
    }catch (Exception e) {
      assertEquals("Model already registered for IRI.", e.getCause().getMessage());
    }
  }

  @Test
  public void testcreateNewModel() {

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel = context.createNewModel(TestModel.class, "http://testiri.com/test");
    assertEquals("http://testiri.com/test", testModel.getIri());
    assertEquals(context, testModel.getContext());
    assertTrue(context.members.containsValue(testModel));

    Field cleanval;
    Object[] cleanValues = new Object[0];
    try{
      cleanval = Model.class.getDeclaredField("cleanValues");
      cleanval.setAccessible(true);
      cleanValues= (Object[]) cleanval.get(testModel);
    } catch (NoSuchFieldException | IllegalAccessException e){
      fail();
    }


    for (FieldInterface field : metaModel.fieldMap.values()){
      assertEquals(Model.SpecialFieldInstruction.NEW, cleanValues[field.index]);
    }
  }

  @Test
  public void testcreateHollowModel() {

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel = context.createHollowModel(TestModel.class, "http://testiri.com/test");
    assertEquals("http://testiri.com/test", testModel.getIri());
    assertEquals(context, testModel.getContext());
    assertTrue(context.members.containsValue(testModel));

    Field cleanval;
    Object[] cleanValues = new Object[0];
    try{
      cleanval = Model.class.getDeclaredField("cleanValues");
      cleanval.setAccessible(true);
      cleanValues= (Object[]) cleanval.get(testModel);
    } catch (NoSuchFieldException | IllegalAccessException e){
      fail();
    }


    for (FieldInterface field : metaModel.fieldMap.values()){
      assertEquals(Model.SpecialFieldInstruction.UNPULLED, cleanValues[field.index]);
    }

  }

  @Test
  public void testgetModel(){

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel1 = context.createNewModel(TestModel.class, "http://testiri.com/test1");
    TestModel testModel2 = context.createNewModel(TestModel.class, "http://testiri.com/test2");
    TestModel testModel3 = context.createNewModel(TestModel.class, "http://testiri.com/test3");

    //model exists
    assertEquals(testModel2, context.getModel(TestModel.class, "http://testiri.com/test2"));

    //check returned model is hollow when model ==null
    TestModel testModel = context.getModel(TestModel.class, "http://testiri.com/test");
    assertEquals("http://testiri.com/test", testModel.getIri());
    assertEquals(context, testModel.getContext());
    assertTrue(context.members.containsValue(testModel));

    Field cleanval;
    Object[] cleanValues = new Object[0];
    try{
      cleanval = Model.class.getDeclaredField("cleanValues");
      cleanval.setAccessible(true);
      cleanValues= (Object[]) cleanval.get(testModel);
    } catch (NoSuchFieldException | IllegalAccessException e){
      fail();
    }
    for (FieldInterface field : metaModel.fieldMap.values()){
      assertEquals(Model.SpecialFieldInstruction.UNPULLED, cleanValues[field.index]);
    }

    //current pull session != null
    RecursivePullSession pullSession = new RecursivePullSession(1, context);
    Field traversedIris;
    Field pendingPullQueue;
    Set<String> iris;
    Queue<Model> pullQueue;
    try {
      context.currentPullSession = pullSession;
      traversedIris = pullSession.getClass().getDeclaredField("traversedIris");
      pendingPullQueue = pullSession.getClass().getDeclaredField("pendingPullQueue");
      traversedIris.setAccessible(true);
      pendingPullQueue.setAccessible(true);
      assertEquals(testModel3, context.getModel(TestModel.class, "http://testiri.com/test3"));
      iris = (Set<String>) traversedIris.get(pullSession);
      pullQueue = (Queue<Model>) pendingPullQueue.get(pullSession);
      assertTrue(iris.contains(testModel3.getIri()));
      assertTrue(pullQueue.contains(testModel3));
    } catch (NoSuchFieldException | IllegalAccessException e) {
      fail();
    }
  }

  @Test
  public void testoptGetModel(){
    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel = TestModel.createRandom(context, 12345, 3, 0);
    assertEquals(testModel, context.optGetModel(TestModel.class, testModel.getIri()));
    assertNull(context.optGetModel(TestModel.class, "http://testiri.com/test"));
  }

  @Test
  public void testloadAll() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 3, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray1 = createResponse1_0();
    JSONArray jsonArray2 = createResponse2_0();


    Method buildQuery;
    SelectBuilder query1 = null;
    SelectBuilder query2 = null;
    try {
      buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildQuery.setAccessible(true);
      query1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), false);
      query2 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), true);
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(query1.buildString()));
    Mockito.doReturn(jsonArray2).when(pullContext).query(Mockito.contains(query2.buildString()));

    TestModel pullModel = pullContext.loadAll(TestModel.class, pushModel.getIri());
    Mockito.verify(pullContext, Mockito.times(1)).query(query1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2.buildString());
    assertEquals(pushModel, pullModel);

  }

  @Test
  public void testrecursiveLoadAll() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 3, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray1_0 = createResponse1_0();
    JSONArray jsonArray2_0 = createResponse2_0();
    JSONArray jsonArray1_1 = createResponse1_1();
    JSONArray jsonArray2_1 = createResponse2_1();
    JSONArray jsonArray1_2 = createResponse1_2();
    JSONArray jsonArray2_2 = createResponse2_2();

    Method buildQuery;
    SelectBuilder query1_0 = null;
    SelectBuilder query2_0 = null;
    SelectBuilder query1_1 = null;
    SelectBuilder query2_1 = null;
    SelectBuilder query1_2 = null;
    SelectBuilder query2_2 = null;

    try {
      buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildQuery.setAccessible(true);
      query1_0 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), false);
      query2_0 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), true);
      query1_1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), false);
      query2_1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), true);
      query1_2 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), false);
      query2_2 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), true);
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(jsonArray1_0).when(pullContext).query(Mockito.contains(query1_0.buildString()));
    Mockito.doReturn(jsonArray2_0).when(pullContext).query(Mockito.contains(query2_0.buildString()));
    Mockito.doReturn(jsonArray1_1).when(pullContext).query(Mockito.contains(query1_1.buildString()));
    Mockito.doReturn(jsonArray2_1).when(pullContext).query(Mockito.contains(query2_1.buildString()));
    Mockito.doReturn(jsonArray1_2).when(pullContext).query(Mockito.contains(query1_2.buildString()));
    Mockito.doReturn(jsonArray2_2).when(pullContext).query(Mockito.contains(query2_2.buildString()));


    TestModel pullModel = pullContext.recursiveLoadAll(TestModel.class, pushModel.getIri(), 2);

    Mockito.verify(pullContext, Mockito.times(1)).query(query1_0.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2_0.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query1_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query1_2.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2_2.buildString());


    assertEquals(pushModel, pullModel);
    assertEquals(pushModel.getModelProp(), pullModel.getModelProp());
    assertEquals(pushModel.getModelProp().getModelProp(), pullModel.getModelProp().getModelProp());
    assertNotEquals(pushModel.getModelProp().getModelProp().getModelProp(), pullModel.getModelProp().getModelProp().getModelProp());

    ModelContext comparisonContext = new ModelContext(testResourceId, testNamespace);
    TestModel comparisonModel = comparisonContext.createHollowModel(TestModel.class, pushModel.getModelProp().getModelProp().getModelProp().getIri());
    assertEquals(comparisonModel, pullModel.getModelProp().getModelProp().getModelProp());
  }

  @Test
  public void testloadPartial(){

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 3, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray scalarsResponse = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString-287790814")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray vectorResponse = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.34911535662488336"))
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.9138466810904882"))
            .put(new JSONObject().put("isblank", "true").put("value", "e58e22743ed2e618c10374a1b81bfb60"));


    Method buildScalarsQuery;
    SelectBuilder scalarsQuery = null;
    SelectBuilder vectorQuery = null;
    Method buildVectorQuery;

    String[] fieldNames = {"stringProp", "forwardVector"};
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);

      scalarsQuery = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), metaModel, fieldNames);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
        vectorQuery = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), entry.getKey()));
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }

    Mockito.doReturn(scalarsResponse).when(pullContext).query(scalarsQuery.buildString());
    Mockito.doReturn(vectorResponse).when(pullContext).query(vectorQuery.buildString());

    TestModel pullModel = pullContext.loadPartial(TestModel.class, pushModel.getIri(), fieldNames);

    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.buildString());

    assertEquals(pushModel.getStringProp(), pullModel.getStringProp());
    assertEquals(new HashSet<>(pushModel.getForwardVector()), new HashSet<>(pullModel.getForwardVector()));

  }

  @Test
  public void testrecursiveLoadPartial(){

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 1, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray scalarsResponse_1 = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString-287790814")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray scalarsResponse_2 = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString1525380402")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray scalarsResponse_3 = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString-2050421886")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray vectorResponse_1 = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.34911535662488336"));
    JSONArray vectorResponse_2 = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.517410013055376"));
    JSONArray vectorResponse_3 = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.034685238715615796"));

    Method buildScalarsQuery;
    SelectBuilder scalarsQuery_1 = null;
    SelectBuilder scalarsQuery_2 = null;
    SelectBuilder scalarsQuery_3 = null;
    SelectBuilder vectorQuery_1 = null;
    SelectBuilder vectorQuery_2 = null;
    SelectBuilder vectorQuery_3 = null;
    Method buildVectorQuery;

    String[] fieldNames = {"stringProp", "modelProp", "forwardVector"};
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);

      scalarsQuery_1 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), metaModel, fieldNames);
      scalarsQuery_2 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), metaModel, fieldNames);
      scalarsQuery_3 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), metaModel, fieldNames);

      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
        vectorQuery_1 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), entry.getKey()));
        vectorQuery_2 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), entry.getKey()));
        vectorQuery_3 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), entry.getKey()));
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }

    Mockito.doReturn(scalarsResponse_1).when(pullContext).query(scalarsQuery_1.buildString());
    Mockito.doReturn(scalarsResponse_2).when(pullContext).query(scalarsQuery_2.buildString());
    Mockito.doReturn(scalarsResponse_3).when(pullContext).query(scalarsQuery_3.buildString());
    Mockito.doReturn(vectorResponse_1).when(pullContext).query(vectorQuery_1.buildString());
    Mockito.doReturn(vectorResponse_2).when(pullContext).query(vectorQuery_2.buildString());
    Mockito.doReturn(vectorResponse_3).when(pullContext).query(vectorQuery_3.buildString());

    TestModel pullModel = pullContext.recursiveLoadPartial(TestModel.class, pushModel.getIri(), 2, fieldNames);

    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery_2.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery_3.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery_2.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery_3.buildString());

    assertEquals(pushModel.getStringProp(), pullModel.getStringProp());
    assertEquals(pushModel.getModelProp().getStringProp(), pullModel.getModelProp().getStringProp());
    assertEquals(pushModel.getModelProp().getModelProp().getStringProp(), pullModel.getModelProp().getModelProp().getStringProp());
    assertNotEquals(pushModel.getModelProp().getModelProp().getModelProp().getStringProp(), pullModel.getModelProp().getModelProp().getModelProp().getStringProp());
    assertTrue(pushModel.getForwardVector().equals(pullModel.getForwardVector()));
    assertEquals(pushModel.getModelProp().getForwardVector(), pullModel.getModelProp().getForwardVector());
    assertEquals(pushModel.getModelProp().getModelProp().getForwardVector(), pullModel.getModelProp().getModelProp().getForwardVector());
    assertNotEquals(pushModel.getModelProp().getModelProp().getModelProp().getForwardVector(), pullModel.getModelProp().getModelProp().getModelProp().getForwardVector());
  }

  @Test
  public void testRecursivePullAllWhere() throws ParseException {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    Mockito.spy(TestModel.createRandom(pushContext, 12345, 0, 10));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray = createResponseForTestLoadAllWhere_1();
    JSONArray jsonArray1 = createResponseForTestLoadAllWhere_2();

    Method buildQuery;
    SelectBuilder query1 = null;
    SelectBuilder query2 = null;
    WhereBuilder condition = new WhereBuilder().addWhere(ModelContext.getModelVar(),
            NodeFactory.createURI(SPARQLUtils.expandQualifiedName("dbpediao:intprop")),
            "?intprop"
    ).addFilter("?intprop > 0");

    String[] iris = {"https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea",
            "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1",
            "https://eg/examplenamespace/09489aa4-9574-3972-8eed-3919e4cb85ee"};

    List<String> recursionsession_query = new ArrayList<>();
    try {
      buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildQuery.setAccessible(true);
      Field MODEL = pullContext.getClass().getDeclaredField("MODEL");
      MODEL.setAccessible(true);
      Node modelNode = NodeFactory.createVariable((String) MODEL.get(pullContext));
      query1 = ((SelectBuilder) buildQuery.invoke(pullContext, modelNode, false)).addWhere(condition).addVar(modelNode).addOrderBy(modelNode);
      query2 = ((SelectBuilder) buildQuery.invoke(pullContext, modelNode, true)).addWhere(condition).addVar(modelNode).addOrderBy(modelNode);
      for (int i=0; i<iris.length; i++){
        recursionsession_query.add(((SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(iris[i]), false)).buildString());
        recursionsession_query.add(((SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(iris[i]), true)).buildString());
      }
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | NoSuchFieldException e) {
      fail();
    }

    for(int i=0; i<recursionsession_query.size(); i+=2){
      if (recursionsession_query.get(i).contains(iris[0]))
      {
        Mockito.doReturn(createModelResponse_false_1()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i)));
        Mockito.doReturn(createModelResponse_true_1()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i+1)));
      }
      else if (recursionsession_query.get(i).contains(iris[1]))
      {
        Mockito.doReturn(createModelResponse_false_2()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i)));
        Mockito.doReturn(createModelResponse_true_2()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i+1)));
      }
      else if (recursionsession_query.get(i).contains(iris[2]))
      {
        Mockito.doReturn(createModelResponse_false_3()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i)));
        Mockito.doReturn(createModelResponse_true_3()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i+1)));
      }
      else if (recursionsession_query.get(i).contains(iris[3]))
      {
        Mockito.doReturn(createModelResponse_false_4()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i)));
        Mockito.doReturn(createModelResponse_true_4()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i+1)));
      }
      else if (recursionsession_query.get(i).contains(iris[4]))
      {
        Mockito.doReturn(createModelResponse_false_5()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i)));
        Mockito.doReturn(createModelResponse_true_5()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i+1)));
      }
      else if (recursionsession_query.get(i).contains(iris[5]))
      {
        Mockito.doReturn(createModelResponse_false_6()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i)));
        Mockito.doReturn(createModelResponse_true_6()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i+1)));
      }
      else if (recursionsession_query.get(i).contains(iris[6]))
      {
        Mockito.doReturn(createModelResponse_false_7()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i)));
        Mockito.doReturn(createModelResponse_true_7()).when(pullContext).query(Mockito.contains(recursionsession_query.get(i+1)));
      }
    }
    Mockito.doReturn(jsonArray).when(pullContext).query(Mockito.contains(query1.buildString()));
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(query2.buildString()));

    List<TestModel> pullModels = pullContext.recursivePullAllWhere(TestModel.class, condition, 1);

    Mockito.verify(pullContext, Mockito.times(1)).query(query1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2.buildString());
    for (int i=0; i<recursionsession_query.size(); i+=2){
      Mockito.verify(pullContext, Mockito.times(1)).query(recursionsession_query.get(i));
      Mockito.verify(pullContext, Mockito.times(1)).query(recursionsession_query.get(i+1));
    }

    for(Model pm: pushContext.members.values()) {
      TestModel pushModel = (TestModel) pm;
      TestModel pullModelMatch = null;
      for(TestModel pullModel: pullModels) {
        if(pullModel.getIri().equals(pushModel.getIri())) {
          if(pullModelMatch != null) fail();
          pullModelMatch = pullModel;
        }
      }
      if(pushModel.getIntProp() > 0) {
        assertNotNull(pullModelMatch);
        assertEquals(pullModelMatch, pushModel);
        assertFalse(pushModel.isHollow("modelProp"));
        if(pushModel.getModelProp() != null) {
          if (pushModel.getIntProp() <= 0 && pushModel.getModelProp().getIntProp() <= 0) {
            assertTrue(pushModel.getModelProp().isHollow("modelProp"));
          } else {
            assertFalse(pushModel.getModelProp().isHollow("modelProp"));
          }
        }
      } else {
        assertNull(pullModelMatch);
      }
    }
  }

  @Test
  public void testPullAllWhere() throws ParseException {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    Mockito.spy(TestModel.createRandom(pushContext, 12345, 0, 10));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray = createResponseForTestLoadAllWhere_1();
    JSONArray jsonArray1 = createResponseForTestLoadAllWhere_2();

    Method buildQuery;
    SelectBuilder query1 = null;
    SelectBuilder query2 = null;
    WhereBuilder condition = new WhereBuilder().addWhere(ModelContext.getModelVar(),
            NodeFactory.createURI(SPARQLUtils.expandQualifiedName("dbpediao:intprop")),
            "?intprop"
    ).addFilter("?intprop > 0");

    try {
      buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildQuery.setAccessible(true);
      Field MODEL = pullContext.getClass().getDeclaredField("MODEL");
      MODEL.setAccessible(true);
      Node modelNode = NodeFactory.createVariable((String) MODEL.get(pullContext));
      query1 = ((SelectBuilder) buildQuery.invoke(pullContext, modelNode, false)).addWhere(condition).addVar(modelNode).addOrderBy(modelNode);
      query2 = ((SelectBuilder) buildQuery.invoke(pullContext, modelNode, true)).addWhere(condition).addVar(modelNode).addOrderBy(modelNode);
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | NoSuchFieldException e) {
      fail();
    }
    Mockito.doReturn(jsonArray).when(pullContext).query(Mockito.contains(query1.buildString()));
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(query2.buildString()));

    List<TestModel> pullModels = pullContext.pullAllWhere(TestModel.class, condition);

    Mockito.verify(pullContext, Mockito.times(1)).query(query1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2.buildString());

    for(Model pm: pushContext.members.values()) {
      TestModel pushModel = (TestModel) pm;
      TestModel pullModelMatch = null;
      for(TestModel pullModel: pullModels) {
        if(pullModel.getIri().equals(pushModel.getIri())) {
          if(pullModelMatch != null) fail();
          pullModelMatch = pullModel;
        }
      }
      if(pushModel.getIntProp() > 0) {
        assertNotNull(pullModelMatch);
        assertEquals(pullModelMatch, pushModel);
      } else {
        assertNull(pullModelMatch);
      }
    }

  }

  @Test
  public void testRecursivePullPartialWhere() throws ParseException {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    Mockito.spy(TestModel.createRandom(pushContext, 12345, 0, 10));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray = createResponseForTestRecursiveLoadPartialWhere();

    Method buildScalarsQuery;
    SelectBuilder scalarsQuery = null;
    Method buildVectorQuery;
    List<String> vectorQuery = new ArrayList<>();
    List<String> recursion_scalarsQuery = new ArrayList<>();
    List<String> recursion_VectorQuery = new ArrayList<>();
    WhereBuilder condition = new WhereBuilder().addWhere(
            ModelContext.getModelVar(),
            NodeFactory.createURI(SPARQLUtils.expandQualifiedName("dbpediao:intprop")),
            "?intprop"
    ).addFilter("?intprop > 0");
    String [] fieldNames = {"intProp", "modelProp", "backwardVector"};
    String[] iris = {"https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d",
            "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1",
            "https://eg/examplenamespace/09489aa4-9574-3972-8eed-3919e4cb85ee"};

    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);
      Field MODEL = pullContext.getClass().getDeclaredField("MODEL");
      MODEL.setAccessible(true);
      Node modelNode = NodeFactory.createVariable((String) MODEL.get(pullContext));

      scalarsQuery = ((SelectBuilder) buildScalarsQuery.invoke(pullContext, modelNode, metaModel, fieldNames)).addWhere(condition).addVar(modelNode);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
        vectorQuery.add((((SelectBuilder) buildVectorQuery.invoke(pullContext, modelNode, entry.getKey())).addWhere(condition).addVar(modelNode).addOrderBy(modelNode)).buildString());
      }

      for (int i=0; i<iris.length; i++){
        recursion_scalarsQuery.add(((SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(iris[i]), metaModel, fieldNames)).buildString());
        for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
          if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
          recursion_VectorQuery.add((((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(iris[i]), entry.getKey()))).buildString());
        }
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | NoSuchFieldException e) {
      fail();
    }

    Mockito.doReturn(jsonArray).when(pullContext).query(Mockito.contains(scalarsQuery.buildString()));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(Mockito.contains(vectorQuery.get(0)));

    Mockito.doReturn(createModelResponse_1()).when(pullContext).query(recursion_scalarsQuery.get(0));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(recursion_VectorQuery.get(0));

    Mockito.doReturn(createModelResponse_2()).when(pullContext).query(recursion_scalarsQuery.get(1));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(recursion_VectorQuery.get(1));

    Mockito.doReturn(createModelResponse_3()).when(pullContext).query(recursion_scalarsQuery.get(2));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(recursion_VectorQuery.get(2));

    Mockito.doReturn(createModelResponse_4()).when(pullContext).query(recursion_scalarsQuery.get(3));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(recursion_VectorQuery.get(3));

    Mockito.doReturn(createModelResponse_5()).when(pullContext).query(recursion_scalarsQuery.get(4));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(recursion_VectorQuery.get(4));

    Mockito.doReturn(createModelResponse_6()).when(pullContext).query(recursion_scalarsQuery.get(5));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(recursion_VectorQuery.get(5));

    Mockito.doReturn(createModelResponse_7()).when(pullContext).query(recursion_scalarsQuery.get(6));
    Mockito.doReturn(new JSONArray()).when(pullContext).query(recursion_VectorQuery.get(6));

    List<TestModel> pullModels = pullContext.recursivePullPartialWhere(TestModel.class, condition,1, "intProp", "modelProp", "backwardVector");

    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(0));
    for(int i=0; i<iris.length; i++){
      Mockito.verify(pullContext, Mockito.times(1)).query(recursion_scalarsQuery.get(i));
      Mockito.verify(pullContext, Mockito.times(1)).query(recursion_VectorQuery.get(i));
    }

    for(Model pm: pushContext.members.values()) {
      TestModel pushModel = (TestModel) pm;
      TestModel pullModelMatch = null;
      for(TestModel pullModel: pullModels) {
        if(pullModel.getIri().equals(pushModel.getIri())) {
          if(pullModelMatch != null) fail();
          pullModelMatch = pullModel;
        }
      }
      if(pushModel.getIntProp() > 0) {
        assertNotNull(pullModelMatch);
        assertEquals(pullModelMatch.getBackwardVector(), pushModel.getBackwardVector());
        assertEquals(pullModelMatch.getIntProp(), pushModel.getIntProp());
        assertNull(pullModelMatch.getStringProp());
        assertEquals(new ArrayList<Double>(), pullModelMatch.getForwardVector());
        assertFalse(pushModel.isHollow("modelProp"));
        if(pushModel.getModelProp() != null) {
          if (pushModel.getIntProp() <= 0 && pushModel.getModelProp().getIntProp() <= 0) {
            assertTrue(pushModel.getModelProp().isHollow("modelProp"));
          } else {
            assertFalse(pushModel.getModelProp().isHollow("modelProp"));
          }
        }
      } else {
        assertNull(pullModelMatch);
      }
    }
  }

  @Test
  public void testPullPartialWhere() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    Mockito.spy(TestModel.createRandom(pushContext, 12345, 0, 10));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray = createResponseForTestLoadPartialWhere();
    JSONArray jsonArray1 = new JSONArray();

    Method buildScalarsQuery;
    SelectBuilder scalarsQuery = null;
    Method buildVectorQuery;
    List<String> vectorQuery = new ArrayList<>();
    WhereBuilder condition = null;
    try {
      condition = new WhereBuilder().addWhere(
              ModelContext.getModelVar(),
              NodeFactory.createURI(SPARQLUtils.expandQualifiedName("dbpediao:intprop")),
              "?intprop"
      ).addFilter("?intprop > 0");
    } catch (ParseException e) {
      fail();
    }
    String [] fieldNames = {"intProp", "backwardVector"};

    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);
      Field MODEL = pullContext.getClass().getDeclaredField("MODEL");
      MODEL.setAccessible(true);
      Node modelNode = NodeFactory.createVariable((String) MODEL.get(pullContext));

      scalarsQuery = ((SelectBuilder) buildScalarsQuery.invoke(pullContext, modelNode, metaModel, fieldNames)).addWhere(condition).addVar(modelNode);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
        vectorQuery.add((((SelectBuilder) buildVectorQuery.invoke(pullContext, modelNode, entry.getKey())).addWhere(condition).addVar(modelNode).addOrderBy(modelNode)).buildString());
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | NoSuchFieldException e) {
      fail();
    }

    Mockito.doReturn(jsonArray).when(pullContext).query(Mockito.contains(scalarsQuery.buildString()));
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(vectorQuery.get(0)));

    List<TestModel> pullModels = pullContext.pullPartialWhere(TestModel.class, condition, "intProp", "backwardVector");

    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(0));

    for(Model pm: pushContext.members.values()) {
      TestModel pushModel = (TestModel) pm;
      TestModel pullModelMatch = null;
      for(TestModel pullModel: pullModels) {
        if(pullModel.getIri().equals(pushModel.getIri())) {
          if(pullModelMatch != null) fail();
          pullModelMatch = pullModel;
        }
      }
      if(pushModel.getIntProp() > 0) {
        assertNotNull(pullModelMatch);
        assertEquals(pullModelMatch.getBackwardVector(), pushModel.getBackwardVector());
        assertEquals(pullModelMatch.getIntProp(), pushModel.getIntProp());
        assertNull(pullModelMatch.getStringProp());
        assertEquals(new ArrayList<Double>(), pullModelMatch.getForwardVector());
      } else {
        assertNull(pullModelMatch);
      }
    }
  }

  @Test
  public void testrecursivePullAll() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 3, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray1_0 = createResponse1_0();
    JSONArray jsonArray2_0 = createResponse2_0();
    JSONArray jsonArray1_1 = createResponse1_1();
    JSONArray jsonArray2_1 = createResponse2_1();
    JSONArray jsonArray1_2 = createResponse1_2();
    JSONArray jsonArray2_2 = createResponse2_2();

    Method buildQuery;
    SelectBuilder query1_0 = null;
    SelectBuilder query2_0 = null;
    SelectBuilder query1_1 = null;
    SelectBuilder query2_1 = null;
    SelectBuilder query1_2 = null;
    SelectBuilder query2_2 = null;

    try {
      buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildQuery.setAccessible(true);
      query1_0 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), false);
      query2_0 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), true);
      query1_1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), false);
      query2_1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), true);
      query1_2 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), false);
      query2_2 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), true);
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(jsonArray1_0).when(pullContext).query(query1_0.buildString());
    Mockito.doReturn(jsonArray2_0).when(pullContext).query(query2_0.buildString());
    Mockito.doReturn(jsonArray1_1).when(pullContext).query(query1_1.buildString());
    Mockito.doReturn(jsonArray2_1).when(pullContext).query(query2_1.buildString());
    Mockito.doReturn(jsonArray1_2).when(pullContext).query(query1_2.buildString());
    Mockito.doReturn(jsonArray2_2).when(pullContext).query(query2_2.buildString());

    TestModel pullModel = pullContext.createHollowModel(TestModel.class, pushModel.getIri());
    pullContext.recursivePullAll(pullModel, 2);

    Mockito.verify(pullContext, Mockito.times(1)).query(query1_0.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2_0.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query1_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query1_2.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2_2.buildString());


    assertEquals(pushModel, pullModel);
    assertEquals(pushModel.getModelProp(), pullModel.getModelProp());
    assertEquals(pushModel.getModelProp().getModelProp(), pullModel.getModelProp().getModelProp());
    assertNotEquals(pushModel.getModelProp().getModelProp().getModelProp(), pullModel.getModelProp().getModelProp().getModelProp());

    ModelContext comparisonContext = new ModelContext(testResourceId, testNamespace);
    TestModel comparisonModel = comparisonContext.createHollowModel(TestModel.class, pushModel.getModelProp().getModelProp().getModelProp().getIri());
    assertEquals(comparisonModel, pullModel.getModelProp().getModelProp().getModelProp());
  }

  @Test
  public void testpullAll() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 3, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray jsonArray1 = createResponse1_0();
    JSONArray jsonArray2 = createResponse2_0();


    Method buildQuery;
    SelectBuilder query1 = null;
    SelectBuilder query2 = null;
    try {
      buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildQuery.setAccessible(true);
      query1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), false);
      query2 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), true);
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(query1.buildString()));
    Mockito.doReturn(jsonArray2).when(pullContext).query(Mockito.contains(query2.buildString()));

    TestModel pullModel = pullContext.createHollowModel(TestModel.class, pushModel.getIri());
    pullContext.pullAll(pullModel);
    Mockito.verify(pullContext, Mockito.times(1)).query(query1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2.buildString());
    assertEquals(pushModel, pullModel);

  }

  @Test
  public void testreadPullAllInDirectionResponse(){

    ModelContext context1 = new ModelContext(testResourceId, testNamespace);
    ModelContext context2 = new ModelContext(testResourceId, testNamespace);
    TestModel testModel1 = TestModel.createRandom(context1, 12345, 3, 0);
    TestModel testModel2 = TestModel.createRandom(context2, 12345, 3, 0);
    JSONArray jsonArray = createResponse1_0();

    Method readPullInDirectionResponse;
    try{
      readPullInDirectionResponse = context1.getClass().getDeclaredMethod("readPullAllInDirectionResponse", Model.class, JSONArray.class, boolean.class, int.class, int.class);
      readPullInDirectionResponse.setAccessible(true);

      readPullInDirectionResponse.invoke(context1, testModel1, jsonArray, false, 0, jsonArray.length());

      for (int index = 0; index < jsonArray.length(); index++) {
        JSONObject row = jsonArray.getJSONObject(index);
        String graph = row.optString("graph", "");
        if (testNamespace != null && graph.startsWith(testNamespace))
          graph = graph.substring(testNamespace.length());
        FieldKey key = new FieldKey(graph, row.getString("predicate"), false);
        FieldInterface field = metaModel.fieldMap.get(key);
        if (field == null) continue;
        field.put(testModel2, context2.isTruthy(row.getString("isblank")) ? null : row.getString("value"), row.optString("datatype"));
      }
      assertEquals(testModel2, testModel1);

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
      fail();
    }

  }

  @Test
  public void testbuildPullAllInDirectionQuery(){

    // Case 1: is Quads = true (backward = true , false)
    ModelContext context1 = new ModelContext(testResourceId, testNamespace);
    TestModel testModel1 = TestModel.createRandom(context1, 12345, 3, 0);
    Node model = NodeFactory.createURI(testModel1.getIri());
    SelectBuilder select;
    WhereBuilder where;
    Method buildPullAllInDirectionQuery = null;
    try{
      buildPullAllInDirectionQuery = context1.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildPullAllInDirectionQuery.setAccessible(true);
      for(boolean backward : new boolean[]{false, true}){
        select = new SelectBuilder().addVar("?value").addVar("?predicate").addVar("?datatype").addVar("?isblank");
        where = new WhereBuilder().addWhere(backward ? "?value" : model, "?predicate", backward ? model : "?value");
        select.addVar("?graph").addGraph("?graph", where);
        select.addBind("DATATYPE" + "(" + "?" + "value" + ")", "?" + "datatype").addBind("ISBLANK" + "(" + "?" + "value" + ")", "?" + "isblank");
        assertEquals(select.buildString(), buildPullAllInDirectionQuery.invoke(context1, model, backward).toString());
      }
    } catch(NoSuchMethodException | ParseException | IllegalAccessException | InvocationTargetException e){
      fail();
    }

    // Case 2: is Quads = false (backward = true , false)
    ModelContext context2 = new ModelContext(testResourceId);
    TripleTestModel testModel2 = context2.createNewModel(TripleTestModel.class, "http://testiri.com/test");
    model = NodeFactory.createURI(testModel2.getIri());
    try{
      for(boolean backward : new boolean[]{false, true}){
        select = new SelectBuilder().addVar("?value").addVar("?predicate").addVar("?datatype").addVar("?isblank");
        where = new WhereBuilder().addWhere(backward ? "?value" : model, "?predicate", backward ? model : "?value");
        select.addWhere(where);
        select.addBind("DATATYPE" + "(" + "?" + "value" + ")", "?" + "datatype").addBind("ISBLANK" + "(" + "?" + "value" + ")", "?" + "isblank");
        assertEquals(select.buildString(), buildPullAllInDirectionQuery.invoke(context2, model, backward).toString());
      }
    } catch(ParseException | IllegalAccessException | InvocationTargetException e){
      fail();
    }
  }

  @Test
  public void testrecursivePullPartial(){

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 1, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray scalarsResponse_1 = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString-287790814")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray scalarsResponse_2 = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString1525380402")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray scalarsResponse_3 = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString-2050421886")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray vectorResponse_1 = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.34911535662488336"));
    JSONArray vectorResponse_2 = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.517410013055376"));
    JSONArray vectorResponse_3 = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.034685238715615796"));

    Method buildScalarsQuery;
    SelectBuilder scalarsQuery_1 = null;
    SelectBuilder scalarsQuery_2 = null;
    SelectBuilder scalarsQuery_3 = null;
    SelectBuilder vectorQuery_1 = null;
    SelectBuilder vectorQuery_2 = null;
    SelectBuilder vectorQuery_3 = null;
    Method buildVectorQuery;

    String[] fieldNames = {"stringProp", "modelProp", "forwardVector"};
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);

      scalarsQuery_1 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), metaModel, fieldNames);
      scalarsQuery_2 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), metaModel, fieldNames);
      scalarsQuery_3 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), metaModel, fieldNames);

      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
        vectorQuery_1 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), entry.getKey()));
        vectorQuery_2 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), entry.getKey()));
        vectorQuery_3 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getModelProp().getIri()), entry.getKey()));
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }

    Mockito.doReturn(scalarsResponse_1).when(pullContext).query(scalarsQuery_1.buildString());
    Mockito.doReturn(scalarsResponse_2).when(pullContext).query(scalarsQuery_2.buildString());
    Mockito.doReturn(scalarsResponse_3).when(pullContext).query(scalarsQuery_3.buildString());
    Mockito.doReturn(vectorResponse_1).when(pullContext).query(vectorQuery_1.buildString());
    Mockito.doReturn(vectorResponse_2).when(pullContext).query(vectorQuery_2.buildString());
    Mockito.doReturn(vectorResponse_3).when(pullContext).query(vectorQuery_3.buildString());

    TestModel pullModel = pullContext.createHollowModel(TestModel.class, pushModel.getIri());
    pullContext.recursivePullPartial(pullModel, 2, fieldNames);

    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery_2.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery_3.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery_1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery_2.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery_3.buildString());

    assertEquals(pushModel.getStringProp(), pullModel.getStringProp());
    assertEquals(pushModel.getModelProp().getStringProp(), pullModel.getModelProp().getStringProp());
    assertEquals(pushModel.getModelProp().getModelProp().getStringProp(), pullModel.getModelProp().getModelProp().getStringProp());
    assertNotEquals(pushModel.getModelProp().getModelProp().getModelProp().getStringProp(), pullModel.getModelProp().getModelProp().getModelProp().getStringProp());
    assertTrue(pushModel.getForwardVector().equals(pullModel.getForwardVector()));
    assertEquals(pushModel.getModelProp().getForwardVector(), pullModel.getModelProp().getForwardVector());
    assertEquals(pushModel.getModelProp().getModelProp().getForwardVector(), pullModel.getModelProp().getModelProp().getForwardVector());
    assertNotEquals(pushModel.getModelProp().getModelProp().getModelProp().getForwardVector(), pullModel.getModelProp().getModelProp().getModelProp().getForwardVector());
  }

  @Test
  public void testpullPartial(){

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = TestModel.createRandom(pushContext, 12345, 3, 3);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    JSONArray scalarsResponse = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d")
                    .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString-287790814")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray vectorResponse = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.34911535662488336"))
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.9138466810904882"))
            .put(new JSONObject().put("isblank", "true").put("value", "e58e22743ed2e618c10374a1b81bfb60"));


    Method buildScalarsQuery;
    SelectBuilder scalarsQuery = null;
    SelectBuilder vectorQuery = null;
    Method buildVectorQuery;

    String[] fieldNames = {"stringProp", "forwardVector"};
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);

      scalarsQuery = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), metaModel, fieldNames);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
        vectorQuery = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), entry.getKey()));
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }

    Mockito.doReturn(scalarsResponse).when(pullContext).query(scalarsQuery.buildString());
    Mockito.doReturn(vectorResponse).when(pullContext).query(vectorQuery.buildString());

    TestModel pullModel = pullContext.createHollowModel(TestModel.class, pushModel.getIri());
    pullContext.pullPartial(pullModel, fieldNames);

    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.buildString());

    assertEquals(pushModel.getStringProp(), pullModel.getStringProp());
    assertEquals(new HashSet<>(pushModel.getForwardVector()), new HashSet<>(pullModel.getForwardVector()));
  }

  @Test
  public void testreadScalarsResponse(){

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel1 = TestModel.createRandom(context, 12345, 3, 0);
    TestModel testModel2 = TestModel.createRandom(context, 1234, 3, 0);
    JSONArray jsonArray = new JSONArray().put(new JSONObject().put("isblank0", "false").put("value0", "teststring").put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    Field cleanval;
    Object[] cleanValues1;
    Object[] cleanValues2;
    Method readScalarsResponse;
    FieldInterface field;

    try{
      readScalarsResponse = context.getClass().getDeclaredMethod("readScalarsResponse", Model.class, JSONObject.class);
      readScalarsResponse.setAccessible(true);
      cleanval = Model.class.getDeclaredField("cleanValues");
      cleanval.setAccessible(true);
      cleanValues1= (Object[]) cleanval.get(testModel1);
      cleanValues2= (Object[]) cleanval.get(testModel2);

      readScalarsResponse.invoke(context, testModel1, jsonArray.getJSONObject(0));

      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.scalarFieldList) {
        field = entry.getValue();
        if (!jsonArray.getJSONObject(0).has("value" + field.index)) continue;
        field.put(testModel2, context.isTruthy(jsonArray.getJSONObject(0).getString("isblank" + field.index)) ? null : jsonArray.getJSONObject(0).getString("value" + field.index),
                jsonArray.getJSONObject(0).optString("datatype" + field.index));
        cleanValues2[field.index] = field.getMinimised(testModel2);
        assertEquals(cleanValues2[field.index], cleanValues1[field.index]);
      }
      assertEquals(testModel2.getStringProp(), testModel1.getStringProp());

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | NoSuchFieldException e){
      fail();
    }

  }

  @Test
  public void testbuildScalarsQuery(){

    //Case 1: isQuads = true
    ModelContext context1 = new ModelContext(testResourceId, testNamespace);
    TestModel testModel1 = TestModel.createRandom(context1, 12345, 3, 0);
    Method buildScalarsQuery = null;
    Node model = NodeFactory.createURI(testModel1.getIri());
    String[] fieldNames = {"intProp", "backwardVector"};
    SelectBuilder select = new SelectBuilder();
    try{
      buildScalarsQuery = context1.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.scalarFieldList) {
        FieldInterface field = entry.getValue();
        FieldKey key = entry.getKey();
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
        Node predicate = NodeFactory.createURI(key.predicate);
        select.addVar("?value" + field.index).addVar("?datatype" + field.index).addVar("?isblank" + field.index);
        WhereBuilder where = new WhereBuilder().addWhere(key.backward ? "?value" + field.index : model, predicate, key.backward ? model : "?value" + field.index);
        select.addGraph(NodeFactory.createURI(testNamespace + key.graphName), where);
        select.addBind("DATATYPE" + "(" + "?value" + field.index + ")", "?datatype" + field.index).addBind("ISBLANK" + "(" + "?value" + field.index + ")", "?isblank" + field.index);
      }
      assertEquals(select.buildString(), buildScalarsQuery.invoke(context1, model, metaModel, fieldNames).toString());
    } catch (NoSuchMethodException | ParseException | IllegalAccessException | InvocationTargetException e){
      fail();
    }

    //Case 2: isQuads = false
    ModelContext context2 = new ModelContext(testResourceId);
    TripleTestModel testModel2 = context2.createNewModel(TripleTestModel.class, "http://testiri.com/test");
    model = NodeFactory.createURI(testModel2.getIri());
    select = new SelectBuilder();
    try{
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.scalarFieldList) {
        FieldInterface field = entry.getValue();
        FieldKey key = entry.getKey();
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
        Node predicate = NodeFactory.createURI(key.predicate);
        select.addVar("?value" + field.index).addVar("?datatype" + field.index).addVar("?isblank" + field.index);
        WhereBuilder where = new WhereBuilder().addWhere(key.backward ? "?value" + field.index : model, predicate, key.backward ? model : "?value" + field.index);
        select.addWhere(where);
        select.addBind("DATATYPE" + "(" + "?value" + field.index + ")", "?datatype" + field.index).addBind("ISBLANK" + "(" + "?value" + field.index + ")", "?isblank" + field.index);
      }
      assertEquals(select.buildString(), buildScalarsQuery.invoke(context2, model, metaModel, fieldNames).toString());
    } catch (ParseException | IllegalAccessException | InvocationTargetException e){
      fail();
    }

  }

  @Test
  public void testreadPullVectorsResponse(){

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel1 = TestModel.createRandom(context, 12345, 3, 0);
    TestModel testModel2 = TestModel.createRandom(context, 1234, 3, 0);
    String[] fieldNames = {"forwardVector"};
    JSONArray response = forwardVectorResponse();
    Field cleanval;
    Object[] cleanValues1;
    Object[] cleanValues2;
    Method readPullVectorsResponse;
    FieldInterface field;
    try{
      readPullVectorsResponse = context.getClass().getDeclaredMethod("readPullVectorsResponse", Model.class, FieldInterface.class, JSONArray.class, int.class, int.class);
      readPullVectorsResponse.setAccessible(true);
      cleanval = Model.class.getDeclaredField("cleanValues");
      cleanval.setAccessible(true);
      cleanValues1= (Object[]) cleanval.get(testModel1);
      cleanValues2= (Object[]) cleanval.get(testModel2);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        field = entry.getValue();
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
        readPullVectorsResponse.invoke(context, testModel1, field, response, 0, response.length());
        field.clear(testModel2);
        for (int i = 0; i < response.length(); i++) {
          JSONObject row = response.getJSONObject(i);
          field.put(testModel2, context.isTruthy(row.getString("isblank")) ? null : row.getString("value"), row.optString("datatype"));
        }
        cleanValues2[field.index] = field.getMinimised(testModel2);
        assertEquals(cleanValues2[field.index], cleanValues1[field.index]);
      }
      assertEquals(testModel2.getForwardVector(), testModel1.getForwardVector());
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | NoSuchFieldException e){
      fail();
    }
  }

  @Test
  public void testbuildVectorQuery(){

    //Case 1: isQuads = true
    ModelContext context1 = new ModelContext(testResourceId, testNamespace);
    TestModel testModel1 = TestModel.createRandom(context1, 12345, 3, 0);

    Node model = NodeFactory.createURI(testModel1.getIri());
    Method buildVectorQuery = null;
    String[] fieldNames = {"intProp", "backwardVector"};
    SelectBuilder select;

    try{
      buildVectorQuery = context1.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        FieldInterface field = entry.getValue();
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
        Node predicate = NodeFactory.createURI(entry.getKey().predicate);
        select = new SelectBuilder().addVar("?value").addVar("?datatype").addVar("?isblank");
        WhereBuilder where = new WhereBuilder().addWhere(entry.getKey().backward ? "?value" : model, predicate, entry.getKey().backward ? model : "?value");
        select.addGraph(NodeFactory.createURI(testNamespace + entry.getKey().graphName), where);
        select.addBind("DATATYPE" + "(" + "?" + "value" + ")", "?" + "datatype").addBind("ISBLANK" + "(" + "?" + "value" + ")", "?" + "isblank");
        assertEquals(select.buildString(), buildVectorQuery.invoke(context1, model, entry.getKey()).toString());
      }
    } catch(NoSuchMethodException | ParseException | IllegalAccessException | InvocationTargetException e){
      fail();
    }

    //Case 2: isQuads = false
    ModelContext context2 = new ModelContext(testResourceId);
    TripleTestModel testModel2 = context2.createNewModel(TripleTestModel.class, "http://testiri.com/test");

    model = NodeFactory.createURI(testModel2.getIri());
    try{
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        FieldInterface field = entry.getValue();
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
        Node predicate = NodeFactory.createURI(entry.getKey().predicate);
        select = new SelectBuilder().addVar("?value").addVar("?datatype").addVar("?isblank");
        WhereBuilder where = new WhereBuilder().addWhere(entry.getKey().backward ? "?value" : model, predicate, entry.getKey().backward ? model : "?value");
        select.addWhere(where);
        select.addBind("DATATYPE" + "(" + "?" + "value" + ")", "?" + "datatype").addBind("ISBLANK" + "(" + "?" + "value" + ")", "?" + "isblank");
        assertEquals(select.buildString(), buildVectorQuery.invoke(context2, model, entry.getKey()).toString());
      }
    } catch(ParseException | IllegalAccessException | InvocationTargetException e){
      fail();
    }
  }

  @Test
  public void testDelete() {

    ModelContext context1 = Mockito.spy((new ModelContext(testResourceId, testNamespace)));
    TestModel testModel1 = TestModel.createRandom(context1, 12345, 3, 0);
    String query = "INSERT DATA { <" + testModel1.getIri() + "> <test> <data> }";

    Mockito.doNothing().when(context1).update(Mockito.contains("CLEAR ALL"));
    Mockito.doNothing().when(context1).update(Mockito.contains("INSERT DATA"));
    Mockito.doNothing().when(context1).update(Mockito.contains("DELETE WHERE"));
    Mockito.doNothing().when(context1).update(query);

    context1.update("CLEAR ALL");
    Mockito.verify(context1, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    context1.update(query);
    Mockito.verify(context1, Mockito.times(1)).update(query);

    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "1"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    int baseCount = countTriples(context1);

    context1.pushAllChanges();
    Mockito.verify(context1, Mockito.times(2)).update(Mockito.contains("INSERT DATA"));
    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "27"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    int firstModelCount = countTriples(context1);

    TestModel testModel2 = TestModel.createRandom(context1, 3152, 8, 0);
    context1.pushAllChanges();

    Mockito.verify(context1, Mockito.times(3)).update(Mockito.contains("INSERT DATA"));
    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "63"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    int secondModelCount = countTriples(context1);

    testModel1.delete(false);
    context1.pushAllChanges();

    Mockito.verify(context1, Mockito.times(1)).update(Mockito.contains("DELETE WHERE"));
    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "37"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    assertEquals(baseCount + secondModelCount - firstModelCount, countTriples(context1));

    testModel2.delete(false);
    context1.pushAllChanges();

    Mockito.verify(context1, Mockito.times(2)).update(Mockito.contains("DELETE WHERE"));
    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "1"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    assertEquals(baseCount, countTriples(context1));

    testModel1 = Mockito.spy(TestModel.createRandom(context1, 12345, 3, 0));
    testModel1.pushChanges();

    Mockito.verify(context1, Mockito.times(4)).update(Mockito.contains("INSERT DATA"));
    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "27"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    assertEquals(firstModelCount, countTriples(context1));
  }

  @Test
  public void testRetire() {
    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel model1 = TestModel.createRandom(context, 1234, 7, 0);
    try {
      TestModel.createRandom(context, 1234, 7, 0);
      fail();
    } catch (JPSRuntimeException ignored) {
    }
    model1.retire();
    TestModel model2 = TestModel.createRandom(context, 1234, 7, 0);
    assertNotSame(model1, model2);
  }

  @Test
  public void testPushAllChanges(){

    ModelContext context = Mockito.spy(new ModelContext(testResourceId, testNamespace));
    TestModel testModel1 = TestModel.createRandom(context, 12345, 3, 0);
    Mockito.doNothing().when(context).update(Mockito.contains("INSERT DATA"));
    Mockito.doNothing().when(context).update(Mockito.contains("DELETE"));
    Node self;
    UpdateRequest deletions = new UpdateRequest();
    UpdateBuilder insertions = new UpdateBuilder();

    //Case 1: LIVE
    for(Map.Entry<FieldKey, FieldInterface> entry : metaModel.fieldMap.entrySet()){
      FieldInterface fieldInterface = entry.getValue();
      FieldKey key = entry.getKey();
      self = NodeFactory.createURI(testModel1.getIri());
      Node predicate = NodeFactory.createURI(key.predicate);
      Node graph = NodeFactory.createURI(testNamespace + key.graphName);
      for (Node valueValue : fieldInterface.getNodes(testModel1)) {
        Triple triple = new Triple(key.backward ? valueValue : self, predicate, key.backward ? self : valueValue);
        insertions.addInsert(new Quad(graph, triple));
      }
    }
    context.pushAllChanges();
    Mockito.verify(context, Mockito.times(1)).update(deletions.add(insertions.build()).toString());

    //Case 2: TO_DELETE
    deletions = new UpdateRequest();
    testModel1.delete(false);

    for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.fieldMap.entrySet()) {
      FieldKey key = entry.getKey();
      self = NodeFactory.createURI(testModel1.getIri());
      Node predicate = NodeFactory.createURI(key.predicate);
      Node graph = NodeFactory.createURI(testNamespace + key.graphName);
      WhereBuilder where = new WhereBuilder().addWhere(key.backward ? "?value" : self, predicate, key.backward ? self : "?value");
      deletions.add(new UpdateBuilder().addGraph(graph, where).buildDeleteWhere());
    }
    context.pushAllChanges();
    Mockito.verify(context, Mockito.times(1)).update(deletions.toString());
    assertFalse(context.members.containsValue(testModel1));

    //Case 3: TO_DELETE_ZEALOUS
    testModel1 = TestModel.createRandom(context, 12345, 3, 0);
    context.pushAllChanges();
    testModel1.delete(true);
    deletions = new UpdateRequest();

    self = NodeFactory.createURI(testModel1.getIri());
    deletions.add(new UpdateBuilder().addWhere("?value", "?predicate", self).buildDeleteWhere());
    deletions.add(new UpdateBuilder().addWhere(self, "?predicate", "?value").buildDeleteWhere());
    context.pushAllChanges();
    Mockito.verify(context, Mockito.times(1)).update(deletions.toString());
    assertFalse(context.members.containsValue(testModel1));

    //Case 4: DESTROYED
    deletions = new UpdateRequest();
    testModel1 = TestModel.createRandom(context, 12345, 3, 0);
    context.pushAllChanges();
    testModel1.state = Model.LifeCycle.DESTROYED;
    context.pushAllChanges();
    Mockito.verify(context, Mockito.times(0)).update(deletions.toString());
    assertFalse(context.members.containsValue(testModel1));

  }

  @Test
  public void testPushChanges() {
    ModelContext context = Mockito.spy((new ModelContext(testResourceId, testNamespace)));
    TestModel testModel = Mockito.spy(TestModel.createRandom(context, 12345, 3, 0));
    UpdateRequest deletions = new UpdateRequest();
    UpdateBuilder insertions = new UpdateBuilder();

    Mockito.doNothing().when(context).update(Mockito.contains("CLEAR ALL"));
    context.update("CLEAR ALL");
    Mockito.verify(context, Mockito.times(1)).update("CLEAR ALL");

    Method makeChangeDeltas = null;
    try {
      makeChangeDeltas = context.getClass().getDeclaredMethod("makeChangeDeltas", Model.class, UpdateRequest.class, UpdateBuilder.class);
      makeChangeDeltas.setAccessible(true);
      makeChangeDeltas.invoke(context, testModel, deletions, insertions);
      deletions.add(insertions.build());
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
      fail();
    }
    Mockito.doNothing().when(context).update(deletions.toString());
    context.pushChanges(testModel);
    Mockito.verify(context, Mockito.times(1)).update(deletions.toString());

    deletions = new UpdateRequest();
    insertions = new UpdateBuilder();
    testModel.setDoubleProp(7.77);
    try {
      makeChangeDeltas.invoke(context, testModel, deletions, insertions);
      deletions.add(insertions.build());
    } catch (IllegalAccessException | InvocationTargetException e){
      fail();
    }
    Mockito.doNothing().when(context).update(deletions.toString());
    context.pushChanges(testModel);
    Mockito.verify(context, Mockito.times(1)).update(deletions.toString());

    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "25"))).when(context).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    assertEquals(metaModel.scalarFieldList.size() + (metaModel.vectorFieldList.size() - 1) * 3, countTriples(context));
  }

  @Test
  public void testmakeChangeDeltas(){

    ModelContext context = new ModelContext(testResourceId, testNamespace);

    TestModel testModel = TestModel.createRandom(context, 12345, 3, 0);

    UpdateRequest deletions_actual = new UpdateRequest();
    UpdateRequest deletions_expected = new UpdateRequest();
    UpdateBuilder insertions_actual = new UpdateBuilder();
    UpdateBuilder insertions_expected = new UpdateBuilder();

    Method makeChangeDeltas;

    try {
      makeChangeDeltas = context.getClass().getDeclaredMethod("makeChangeDeltas", Model.class, UpdateRequest.class, UpdateBuilder.class);
      makeChangeDeltas.setAccessible(true);
      Node self = null;
      //Case 1: only insertion, no deletion, anyInserts = true
      for(Map.Entry<FieldKey, FieldInterface> entry : metaModel.fieldMap.entrySet()){
        FieldInterface fieldInterface = entry.getValue();
        FieldKey key = entry.getKey();
        self = NodeFactory.createURI(testModel.getIri());
        Node predicate = NodeFactory.createURI(key.predicate);
        Node graph = NodeFactory.createURI(testNamespace + key.graphName);
        for (Node valueValue : fieldInterface.getNodes(testModel)) {
          Triple triple = new Triple(key.backward ? valueValue : self, predicate, key.backward ? self : valueValue);
          insertions_expected.addInsert(new Quad(graph, triple));
        }
      }
      assertTrue((Boolean) makeChangeDeltas.invoke(context, testModel, deletions_actual, insertions_actual));
      assertEquals(insertions_expected.build().toString(), insertions_actual.build().toString());
      assertEquals(deletions_expected.toString(), deletions_actual.toString());
      testModel.setClean();

      //Case 2: both insertion and deletion, anyInserts = true
      deletions_actual = new UpdateRequest();
      deletions_expected = new UpdateRequest();
      insertions_actual = new UpdateBuilder();
      insertions_expected = new UpdateBuilder();

      testModel.setDoubleProp(7.77);

      Node predicate = NodeFactory.createURI("http://dbpedia.org/ontology/doubleprop");
      Node graph = NodeFactory.createURI("http://localhost:9999/blazegraph/namespace/test/sparql/testmodels");
      WhereBuilder where = new WhereBuilder().addWhere(self, predicate, "?value");
      Node value = NodeFactory.createLiteralByValue(7.77, new XSDDouble("double"));
      deletions_expected.add(new UpdateBuilder().addGraph(graph, where).buildDeleteWhere());
      insertions_expected.addInsert(new Quad(graph, new Triple(self, predicate, value)));

      assertTrue((Boolean) makeChangeDeltas.invoke(context, testModel, deletions_actual, insertions_actual));
      assertEquals(insertions_expected.build().toString(), insertions_actual.build().toString());
      assertEquals(deletions_expected.toString(), deletions_actual.toString());
      testModel.setClean();

      //Case 3: no insertion, no deletion, anyInserts = false
      deletions_actual = new UpdateRequest();
      deletions_expected = new UpdateRequest();
      insertions_actual = new UpdateBuilder();

      assertFalse((Boolean) makeChangeDeltas.invoke(context, testModel, deletions_actual, insertions_actual));
      try{
        insertions_actual.build();
      } catch(IllegalStateException e){
        assertEquals("At least one delete or insert must be specified", e.getMessage());
      }
      assertEquals(deletions_expected.toString(), deletions_actual.toString());

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
  }

  @Test
  public void testmakeDeleteDeltas(){

    //Case 1 : isQuads = true
    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel = TestModel.createRandom(context, 12345, 3, 0);
    UpdateRequest deletions_expected = new UpdateRequest();
    UpdateRequest deletions_actual = new UpdateRequest();
    Method makeDeleteDeltas = null;

    try{
      makeDeleteDeltas = context.getClass().getDeclaredMethod("makeDeleteDeltas", Model.class, UpdateRequest.class);
      makeDeleteDeltas.setAccessible(true);
      makeDeleteDeltas.invoke(context, testModel, deletions_actual);
    } catch(NoSuchMethodException | InvocationTargetException | IllegalAccessException e){
      fail();
    }

    for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.fieldMap.entrySet()) {
      FieldKey key = entry.getKey();
      Node self = NodeFactory.createURI(testModel.getIri());
      Node predicate = NodeFactory.createURI(key.predicate);
      Node graph = NodeFactory.createURI(testNamespace + key.graphName);
      WhereBuilder where = new WhereBuilder().addWhere(key.backward ? "?value" : self, predicate, key.backward ? self : "?value");
      deletions_expected.add(new UpdateBuilder().addGraph(graph, where).buildDeleteWhere());
    }
    assertEquals(deletions_expected.toString(), deletions_actual.toString());

    //Case 2: isQuads = false
    ModelContext context1 = new ModelContext(testResourceId);
    TripleTestModel testModel1 = context1.createNewModel(TripleTestModel.class, "http://testiri.com/test");
    deletions_expected = new UpdateRequest();
    deletions_actual = new UpdateRequest();

    try{
      makeDeleteDeltas.invoke(context1, testModel1, deletions_actual);
    } catch(InvocationTargetException | IllegalAccessException e){
      fail();
    }

    for (Map.Entry<FieldKey, FieldInterface> entry : MetaModel.get(TripleTestModel.class).fieldMap.entrySet()) {
      FieldKey key = entry.getKey();
      Node self = NodeFactory.createURI(testModel1.getIri());
      Node predicate = NodeFactory.createURI(key.predicate);
      WhereBuilder where = new WhereBuilder().addWhere(key.backward ? "?value" : self, predicate, key.backward ? self : "?value");
      deletions_expected.add(new UpdateBuilder().addWhere(where).buildDeleteWhere());
    }
    assertEquals(deletions_expected.toString(), deletions_actual.toString());
  }

  @Test
  public void testmakeDeleteZealousDeltas(){

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    TestModel testModel = TestModel.createRandom(context, 12345, 3, 0);

    Method makeDeleteZealousDeltas;
    UpdateRequest deletions_expected = new UpdateRequest();
    UpdateRequest deletions_actual = new UpdateRequest();

    try{
      makeDeleteZealousDeltas = context.getClass().getDeclaredMethod("makeDeleteZealousDeltas", Model.class, UpdateRequest.class);
      makeDeleteZealousDeltas.setAccessible(true);
      makeDeleteZealousDeltas.invoke(context, testModel, deletions_actual);
      Node self = NodeFactory.createURI(testModel.getIri());
      deletions_expected.add(new UpdateBuilder().addWhere("?value", "?predicate", self).buildDeleteWhere());
      deletions_expected.add(new UpdateBuilder().addWhere(self, "?predicate", "?value").buildDeleteWhere());
      assertEquals(deletions_expected.toString(), deletions_actual.toString());
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
      fail();
    }
  }

  @Test
  public void testisTruthy(){

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    assertTrue(context.isTruthy("true"));
    assertTrue(context.isTruthy("1"));
    assertFalse(context.isTruthy("not true"));
  }

  @Test
  public void testgetModelVar() {

    ModelContext context = new ModelContext(testResourceId, testNamespace);
    try{
      Field MODEL = context.getClass().getDeclaredField("MODEL");
      MODEL.setAccessible(true);
      assertEquals(NodeFactory.createVariable((String) MODEL.get(context)), context.getModelVar());
    } catch(NoSuchFieldException | IllegalAccessException e){
      fail();
    }
  }

  @Test
  public void testPushNewObject() {

    ModelContext context = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(context).update(Mockito.contains("CLEAR ALL"));
    Mockito.doNothing().when(context).update(Mockito.contains("INSERT DATA"));

    context.update("CLEAR ALL");
    TestModel testModel = Mockito.spy(TestModel.createRandom(context, 12345, 3, 3));
    testModel.pushChanges();

    Mockito.verify(context, Mockito.times(1)).update("CLEAR ALL");
    Mockito.verify(testModel, Mockito.times(1)).pushChanges();
    Mockito.verify(context, Mockito.times(1)).pushChanges(testModel);
    Mockito.verify(context, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));
    Mockito.verify(context, Mockito.never()).query(Mockito.contains("DELETE"));

    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "25"))).when(context).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    assertEquals(metaModel.scalarFieldList.size() + (metaModel.vectorFieldList.size() - 1) * 3, countTriples(context));

  }

  @Test
  public void testPushPartialChanges() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = Mockito.spy(TestModel.createRandom(pushContext, 12345, 3, 3));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));
    TestModel pullModel = Mockito.spy(pullContext.createHollowModel(TestModel.class, pushModel.getIri()));
    JSONObject jsonObject = new JSONObject();
    jsonObject.put("value4", "https://eg/examplenamespace/randomuris/1402202751");
    jsonObject.put("value3", "0.8330913489710237");
    jsonObject.put("isblank4", "false");
    jsonObject.put("isblank3", "false");
    jsonObject.put("datatype3", "http://www.w3.org/2001/XMLSchema#double");

    Method buildScalarsQuery = null;
    SelectBuilder scalarsQuery = null;
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      scalarsQuery = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pullModel.getIri()), metaModel, new String[]{"doubleProp", "uriProp"});
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(new JSONArray().put(jsonObject)).when(pullContext).query(Mockito.contains(scalarsQuery.buildString()));
    pullModel.pull("doubleProp", "uriProp");
    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery.buildString());

    pullModel.setDoubleProp(7.77);
    pullModel.setStringProp("modified");
    Mockito.doNothing().when(pullContext).update(Mockito.contains("INSERT DATA"));
    Mockito.doNothing().when(pullContext).update(Mockito.contains("DELETE"));
    pullModel.pushChanges();
    // Only fields which were pulled should ever be pushed, even if unpulled fields have been changed.

    Mockito.verify(pullContext, Mockito.never()).update(Mockito.contains("stringProp"));
    Mockito.verify(pullContext, Mockito.never()).update(Mockito.contains("uriProp"));

    JSONObject jsonObject1 = new JSONObject();
    jsonObject1.put("isblank0", "false");
    jsonObject1.put("value0", "randomString-287790814");
    jsonObject1.put("datatype0", "\"http://www.w3.org/2001/XMLSchema#string\"");
    try {
      scalarsQuery = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pullModel.getIri()), metaModel, new String[]{"stringProp"});
    } catch (IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(new JSONArray().put(jsonObject1)).when(pullContext).query(Mockito.contains(scalarsQuery.buildString()));
    pullModel.pull("stringProp");
    Mockito.verify(pullContext, Mockito.times(2)).query(Mockito.contains("SELECT"));

    // If we pull the string field, we should afterwards be able to change and push it.
    pullModel.pushChanges();
    Mockito.verify(pullContext, Mockito.never()).update(Mockito.contains("stringProp"));

    pullModel.setStringProp("modified2");
    pullModel.pushChanges();
    Mockito.verify(pullContext, Mockito.times(2)).update(Mockito.contains("INSERT DATA"));
    Mockito.verify(pullContext, Mockito.times(2)).update(Mockito.contains("DELETE"));
    Mockito.verify(pullContext, Mockito.times(1)).update(Mockito.contains("stringprop"));
    Mockito.verify(pullContext, Mockito.times(1)).update(Mockito.contains("doubleprop"));
    Mockito.verify(pullContext, Mockito.never()).update(Mockito.contains("uriprop"));
  }

  @Test
  public void testDeleteZealous() {

    ModelContext context1 = Mockito.spy((new ModelContext(testResourceId, testNamespace)));
    TestModel testModel1 = TestModel.createRandom(context1, 12345, 3, 0);
    String query = "INSERT DATA { <" + testModel1.getIri() + "> <test> <data> }";

    Mockito.doNothing().when(context1).update(Mockito.contains("CLEAR ALL"));
    Mockito.doNothing().when(context1).update(Mockito.contains("INSERT DATA"));
    Mockito.doNothing().when(context1).update(Mockito.contains("DELETE WHERE"));
    Mockito.doNothing().when(context1).update(query);

    context1.update("CLEAR ALL");
    Mockito.verify(context1, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));
    context1.pushAllChanges();
    Mockito.verify(context1, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));
    context1.update(query);
    Mockito.verify(context1, Mockito.times(1)).update(query);

    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "27"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    int firstModelCount = countTriples(context1);

    TestModel testModel2 = TestModel.createRandom(context1, 3152, 8, 0);
    context1.pushAllChanges();

    Mockito.verify(context1, Mockito.times(3)).update(Mockito.contains("INSERT DATA"));
    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "63"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    int secondModelCount = countTriples(context1);

    testModel1.delete(false);
    context1.pushAllChanges();

    Mockito.verify(context1, Mockito.times(1)).update(Mockito.contains("DELETE WHERE"));
    Mockito.doReturn(new JSONArray().put(new JSONObject().put("count", "36"))).when(context1).query(Mockito.contains("SELECT (COUNT(*) AS ?count) WHERE { ?a ?b ?c }"));
    assertEquals(secondModelCount - firstModelCount, countTriples(context1));
  }

  @Test
  public void testPullScalars() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = Mockito.spy(TestModel.createRandom(pushContext, 12345, 3, 3));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));
    TestModel pullModel = Mockito.spy(pullContext.createHollowModel(TestModel.class, pushModel.getIri()));

    JSONArray jsonArray = createResponseForTestPullScalars_1();
    JSONArray jsonArray1 = createResponseForTestPullScalars_2();
    JSONArray jsonArray2 = new JSONArray();
    JSONArray jsonArray3 = forwardVectorResponse();
    JSONArray jsonArray4 = backwardVectorResponse();

    Method buildScalarsQuery;
    SelectBuilder scalarsQuery = null;
    SelectBuilder query = null;
    Method buildVectorQuery;
    List<String> vectorQuery = new ArrayList<>();
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);

      scalarsQuery = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pullModel.getIri()), metaModel, new String[]{"doubleProp", "uriProp"});
      query = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pullModel.getIri()), metaModel, new String[0]);

      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (new String[0].length > 0 && !ArrayUtils.contains(new String[0], entry.getValue().field.getName())) continue;
        vectorQuery.add(((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pullModel.getIri()), entry.getKey())).buildString());
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }

    Mockito.doReturn(jsonArray).when(pullContext).query(Mockito.contains(scalarsQuery.buildString()));
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(query.buildString()));
    Mockito.doReturn(jsonArray2).when(pullContext).query(Mockito.contains(vectorQuery.get(0)));
    Mockito.doReturn(jsonArray3).when(pullContext).query(Mockito.contains(vectorQuery.get(1)));
    Mockito.doReturn(jsonArray4).when(pullContext).query(Mockito.contains(vectorQuery.get(2)));

    pullModel.pull("doubleProp", "uriProp");
    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery.buildString());

    assertEquals(pushModel.getDoubleProp(), pullModel.getDoubleProp());
    assertEquals(pushModel.getUriProp(), pullModel.getUriProp());
    assertNotEquals(pushModel.getStringProp(), pullModel.getStringProp());

    pullModel.pull();
    Mockito.verify(pullContext, Mockito.times(1)).query(query.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(0));
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(1));
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(2));

    for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.scalarFieldList) {
      assertTrue(entry.getValue().equals(pushModel, pullModel));
    }
  }

  @Test
  public void testPullVector() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TestModel pushModel = Mockito.spy(TestModel.createRandom(pushContext, 12345, 3, 3));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));
    TestModel pullModel = Mockito.spy(pullContext.createHollowModel(TestModel.class, pushModel.getIri()));

    JSONArray jsonArray1 = createResponseForTestPullScalars_2();
    JSONArray jsonArray2 = new JSONArray();
    JSONArray jsonArray3 = forwardVectorResponse();
    JSONArray jsonArray4 = backwardVectorResponse();

    Method buildScalarsQuery;
    SelectBuilder query = null;
    Method buildVectorQuery;
    List<String> vectorQuery = new ArrayList<>();
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);

      query = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pullModel.getIri()), metaModel, new String[0]);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (new String[0].length > 0 && !ArrayUtils.contains(new String[0], entry.getValue().field.getName())) continue;
        vectorQuery.add(((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pullModel.getIri()), entry.getKey())).buildString());
      }

    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }

    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(query.buildString()));
    Mockito.doReturn(jsonArray2).when(pullContext).query(Mockito.contains(vectorQuery.get(0)));
    Mockito.doReturn(jsonArray3).when(pullContext).query(Mockito.contains(vectorQuery.get(1)));
    Mockito.doReturn(jsonArray4).when(pullContext).query(Mockito.contains(vectorQuery.get(2)));

    pullModel.pull("backwardVector", "emptyForwardVector");
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(0));
    Mockito.verify(pullContext, Mockito.never()).query(vectorQuery.get(1));
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(2));

    assertEquals(new HashSet<>(pushModel.getBackwardVector()), new HashSet<>(pullModel.getBackwardVector()));
    assertEquals(new HashSet<>(pushModel.getEmptyForwardVector()), new HashSet<>(pullModel.getEmptyForwardVector()));
    assertNotEquals(new HashSet<>(pushModel.getForwardVector()), new HashSet<>(pullModel.getForwardVector()));

    pullModel.pull();
    Mockito.verify(pullContext, Mockito.times(1)).query(query.buildString());
    Mockito.verify(pullContext, Mockito.times(2)).query(vectorQuery.get(0));
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(1));
    Mockito.verify(pullContext, Mockito.times(2)).query(vectorQuery.get(2));

    for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
      assertTrue(entry.getValue().equals(pushModel, pullModel));
    }
  }

  @Test
  public void testEquals() {
    ModelContext context1 = new ModelContext(testResourceId, testNamespace);
    ModelContext context2 = new ModelContext(testResourceId, testNamespace);
    TestModel model1 = TestModel.createRandom(context1, 1234, 7, 2);
    TestModel model2 = TestModel.createRandom(context2, 1234, 7, 2);
    assertEquals(model1, model2);
    model2.getModelProp().setStringProp("modified string 1");
    assertEquals(model1, model2);
    model2.setModelProp(TestModel.createRandom(context2, 12, 0, 1));
    assertNotEquals(model1, model2);
    model1.setModelProp(TestModel.createRandom(context1, 12, 0, 1));
    assertEquals(model1, model2);
    model2.setStringProp("modified string 2");
    assertNotEquals(model1, model2);
  }

  @Test
  public void testIRICollision() {
    ModelContext context = new ModelContext(testResourceId, testNamespace);
    context.createNewModel(TestModel.class, "test");
    try {
      context.createNewModel(TestModel.class, "test");
      fail();
    } catch (JPSRuntimeException ignored) {
    }
  }

  @Test
  public void testQuadModelInTripleContext() {
    ModelContext context = new ModelContext(testResourceId);
    try {
      context.createNewModel(TestModel.class, "test");
      fail();
    } catch (JPSRuntimeException ignored) {
    }
  }


  public static class TripleTestModel extends Model {
    @Getter @Setter @FieldAnnotation("JPSLAND:stringprop") private String stringProp;
    @Getter @Setter @FieldAnnotation("dbpediao:intprop") private Integer intProp;
    @Getter @Setter @FieldAnnotation("dbpediao:doubleprop") private Double doubleProp;
    @Getter @Setter @FieldAnnotation(value = "dbpediao:forwardvector", innerType = Double.class) private ArrayList<Double> forwardVector;
    @Getter @Setter @FieldAnnotation(value = "dbpediao:emptyforwardvector", innerType = Double.class) private ArrayList<Double> emptyForwardVector;
    @Getter @Setter @FieldAnnotation(value = "dbpediao:backwardVector", backward = true, innerType = URI.class) private ArrayList<URI> backwardVector;
  }

  @Test
  public void testTripleModelPushPullAll() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TripleTestModel pushModel = pushContext.createNewModel(TripleTestModel.class, "http://testiri.com/test");
    pushModel.setStringProp("teststring");
    pushModel.getForwardVector().add(2.77);
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId));

    JSONArray jsonArray1 = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("isblank", "true")
                    .put("value", "194cb54a7c035392c7b0d6c17c932035"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector").put("datatype", "http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank", "false").put("value", "2.77"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("isblank", "true")
                    .put("value", "97c131c385044e482f30f7127d578dc0"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype", "http://www.w3.org/2001/XMLSchema#string")
                    .put("isblank", "false").put("value", "teststring"));
    JSONArray jsonArray2 = new JSONArray();


    Method buildQuery;
    SelectBuilder query1 = null;
    SelectBuilder query2 = null;
    try {
      buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
      buildQuery.setAccessible(true);
      query1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), false);
      query2 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), true);
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(query1.buildString()));
    Mockito.doReturn(jsonArray2).when(pullContext).query(Mockito.contains(query2.buildString()));

    TripleTestModel pullModel = pullContext.loadAll(TripleTestModel.class, pushModel.getIri());
    Mockito.verify(pullContext, Mockito.times(1)).query(query1.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(query2.buildString());
    assertEquals(pushModel, pullModel);

  }

  @Test
  public void testTripleModelPullScalarsVectors() {

    ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId));

    Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
    pushContext.update("CLEAR ALL");
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

    TripleTestModel pushModel = pushContext.createNewModel(TripleTestModel.class, "http://testiri.com/test");
    pushModel.setStringProp("teststring");
    pushModel.setDoubleProp(77.11);
    pushModel.getForwardVector().add(2.77);
    pushModel.getBackwardVector().add(URI.create("http://testiri.com/testuri"));
    Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
    pushContext.pushAllChanges();
    Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

    ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId));

    JSONArray jsonArray1 = new JSONArray().put(new JSONObject().put("isblank0", "false").put("value0", "teststring").put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
    JSONArray jsonArray2 = new JSONArray().put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false").put("value", "2.77"));

    Method buildScalarsQuery;
    SelectBuilder scalarsQuery = null;
    Method buildVectorQuery;
    List<String> vectorQuery = new ArrayList<>();
    String [] fieldNames = {"stringProp", "forwardVector"};
    try {
      buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
      buildScalarsQuery.setAccessible(true);
      buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
      buildVectorQuery.setAccessible(true);

      scalarsQuery = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), metaModel, fieldNames);
      for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
        if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
        vectorQuery.add(((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), entry.getKey())).buildString());
      }
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      fail();
    }
    Mockito.doReturn(jsonArray1).when(pullContext).query(Mockito.contains(scalarsQuery.buildString()));
    Mockito.doReturn(jsonArray2).when(pullContext).query(Mockito.contains(vectorQuery.get(0)));

    TripleTestModel pullModel = pullContext.loadPartial(TripleTestModel.class, pushModel.getIri(), fieldNames);

    Mockito.verify(pullContext, Mockito.times(1)).query(scalarsQuery.buildString());
    Mockito.verify(pullContext, Mockito.times(1)).query(vectorQuery.get(0));

    assertNotEquals(pushModel, pullModel);
    assertEquals(pushModel.getStringProp(), pullModel.getStringProp());
    assertNotEquals(pushModel.getDoubleProp(), pullModel.getDoubleProp());
    assertEquals(pushModel.getForwardVector(), pullModel.getForwardVector());
    assertNotEquals(pushModel.getBackwardVector(), pullModel.getBackwardVector());

  }

  public JSONArray createResponse1_0(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string")
                    .put("isblank","false").put("value", "randomString-287790814").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop")
                    .put("isblank","true").put("value", "1813e294f9ac91753c408b278af3ebf2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.8330913489710237").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull")
                    .put("isblank","true").put("value", "2e99c7c53fa664fd2da413c947485199").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.34911535662488336").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.9138466810904882").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector")
                    .put("isblank","true").put("value", "a37e6b8477e475c9b7ed4e3497a6d451").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a")
                    .put("isblank","true").put("value", "a6abda03daafd007a4bbdf58ded07a0f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a")
                    .put("isblank","true").put("value", "f6e65041ac5f56aa66d28fe69ea31c3d").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b")
                    .put("isblank","true").put("value", "d57dfd33a11e22b9c20d4469d0a20100").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a")
                    .put("isblank","true").put("value", "a3fb68e7cf404de42e7d3962cc94ac97").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b")
                    .put("isblank","true").put("value", "d05a9ab886eb7d5e612261809f3ded33").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c")
                    .put("isblank","true").put("value", "804d12f31819430074ade016221c0475").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int")
                    .put("isblank","false").put("value", "-355989640").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull")
                    .put("isblank","true").put("value", "16343243de676dbe359a8f321ac87733").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop")
                    .put("isblank","false").put("value", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull")
                    .put("isblank","true").put("value", "9697279a4bf1792f251430e0eb92f034").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop")
                    .put("isblank","false").put("value", "https://eg/examplenamespace/randomuris/1402202751").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull")
                    .put("isblank","true").put("value", "f43b45e47362f4e6e44bf62478b597b6").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull")
                    .put("isblank","true").put("value", "e478f5f6d6f8fcea89cd1db9e16e1c25").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createResponse2_0(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/randomuris/151766778").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/randomuris/1924478780").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "4560b9a575b30e0ee06a61fb884974ba").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "5a9dfea5945f93c5d5df9fc2148042cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank","true")
                    .put("value", "ceb0f5eeab1e153aca56a0e3a0058f9a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createResponse1_1(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string")
                    .put("isblank","false").put("value", "randomString1525380402").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop")
                    .put("isblank","true").put("value", "41a067e8afcec196bdbddee743dacd40").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.3475288646103122").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull")
                    .put("isblank","true").put("value", "bf19f5854c3112433370def2ad7b5c0e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.13561788626175464").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.517410013055376").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector")
                    .put("isblank","true").put("value", "cbf1df26d860cd93cc3daecc5684c2f8").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a")
                    .put("isblank","true").put("value", "b6aec88ed2a222ab764da6094f23d244").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a")
                    .put("isblank","true").put("value", "7636e2c8e1ad4ddabd70874a219ed3e7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b")
                    .put("isblank","true").put("value", "ca04894217d214407bd8a4703120339e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a")
                    .put("isblank","true").put("value", "7b6f60c56e66475ea7572aa53da9b026").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b")
                    .put("isblank","true").put("value", "fc92a3d244ee0d7989b507ebdd0af4c6").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c")
                    .put("isblank","true").put("value", "0168100e4034105d94579cacc4335de3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int")
                    .put("isblank","false").put("value", "1094996859").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull")
                    .put("isblank","true").put("value", "7106ec445b1caab74d823381bec5d5fd").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop")
                    .put("isblank","false").put("value", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull")
                    .put("isblank","true").put("value", "6ebe8a29db927b016e96f56e80d2a748").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop")
                    .put("isblank","false").put("value", "https://eg/examplenamespace/randomuris/1905807410").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull")
                    .put("isblank","true").put("value", "44c7dd444cd430d7e07cfee72d8a7fd1").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull")
                    .put("isblank","true").put("value", "6153ef31a84619132f8191795213f661").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createResponse2_1(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/4bb4eeac-5793-3eee-b6d5-d453b9f759bd").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/randomuris/1355381216").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/randomuris/1657489626").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "3924e67c277f76a9c892abff45095f96").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "4560b9a575b30e0ee06a61fb884974ba").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "44ac5a1593f786f20a8412a42e26832f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank","true")
                    .put("value", "30e7ef8d9d2abe73add84117331dca61").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createResponse1_2(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string")
                    .put("isblank","false").put("value", "randomString-2050421886").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop")
                    .put("isblank","true").put("value", "41a067e8afcec196bdbddee743dacd40").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.666633888087799").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull")
                    .put("isblank","true").put("value", "bf19f5854c3112433370def2ad7b5c0e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.023474617533402298").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector").put("datatype","http://www.w3.org/2001/XMLSchema#double")
                    .put("isblank","false").put("value", "0.034685238715615796").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/forwardvector")
                    .put("isblank","true").put("value", "cbf1df26d860cd93cc3daecc5684c2f8").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a")
                    .put("isblank","true").put("value", "b6aec88ed2a222ab764da6094f23d244").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a")
                    .put("isblank","true").put("value", "7636e2c8e1ad4ddabd70874a219ed3e7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b")
                    .put("isblank","true").put("value", "ca04894217d214407bd8a4703120339e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a")
                    .put("isblank","true").put("value", "7b6f60c56e66475ea7572aa53da9b026").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b")
                    .put("isblank","true").put("value", "fc92a3d244ee0d7989b507ebdd0af4c6").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c")
                    .put("isblank","true").put("value", "0168100e4034105d94579cacc4335de3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int")
                    .put("isblank","false").put("value", "486786104").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull")
                    .put("isblank","true").put("value", "7106ec445b1caab74d823381bec5d5fd").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop")
                    .put("isblank","false").put("value", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull")
                    .put("isblank","true").put("value", "6ebe8a29db927b016e96f56e80d2a748").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop")
                    .put("isblank","false").put("value", "https://eg/examplenamespace/randomuris/-727373186").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull")
                    .put("isblank","true").put("value", "44c7dd444cd430d7e07cfee72d8a7fd1").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull")
                    .put("isblank","true").put("value", "6153ef31a84619132f8191795213f661").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createResponse2_2(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/randomuris/-745292122").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank","false")
                    .put("value", "https://eg/examplenamespace/randomuris/874244884").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "3924e67c277f76a9c892abff45095f96").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "4560b9a575b30e0ee06a61fb884974ba").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/backwardVector").put("isblank","true")
                    .put("value", "44ac5a1593f786f20a8412a42e26832f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank","true")
                    .put("value", "30e7ef8d9d2abe73add84117331dca61").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }

  public JSONArray createResponseForTestPullScalars_1(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value4", "https://eg/examplenamespace/randomuris/1402202751").put("value3","0.8330913489710237")
                    .put("isblank4", "false").put("isblank3", "false").put("datatype3", "http://www.w3.org/2001/XMLSchema#double"));
    return jsonArray;
  }
  public JSONArray createResponseForTestPullScalars_2(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5","https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d")
                    .put("value7", "4bfab0f0f8e58a85686560be3882a8be").put("value6", "fa00cebb2454bfc15e0c920d8e0e374c")
                    .put("value2", "7c86355ade39e976269d9c2ac529a6d3").put("value1", "-355989640")
                    .put("value4", "https://eg/examplenamespace/randomuris/1402202751").put("value3", "0.8330913489710237")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int").put("value8", "7c6244ed15fce769ae0377b20eba0849")
                    .put("datatype0", "http://www.w3.org/2001/XMLSchema#string").put("datatype3", "http://www.w3.org/2001/XMLSchema#double")
                    .put("value18", "5c1833d952d595b71461c4e2742f52bc").put("value17", "634e25607bb6e671ca0b39c3b13a60f7")
                    .put("value14", "87542be53d21b81e41a899eb6e741c3b").put("value13", "9e3915fadc00646ba1566ed23f8e2a2b")
                    .put("value16", "12f21ce1ddf2f64db529f9c0a83ae843").put("value15", "3f323345081eaf2cc1e37bbc854658db")
                    .put("value10", "d09395becdab11b4f2652ebd7e9613ec").put("value9", "fbfe8ac83e80eb5db7ecdf7664214938")
                    .put("value12", "ac9355a9a7ae457b7c89f8bd9a1424e7").put("value11", "https://eg/examplenamespace/randomuris/151766778")
                    .put("isblank17", "true").put("isblank16", "true").put("isblank18", "true")
                    .put("isblank0", "false").put("isblank1", "false").put("isblank4", "false")
                    .put("isblank5", "false").put("isblank2", "true").put("isblank3", "false")
                    .put("isblank7", "true").put("isblank8", "true").put("isblank6", "true")
                    .put("isblank9", "true").put("value0", "randomString-287790814").put("isblank11", "false")
                    .put("isblank10", "true").put("isblank13", "true").put("isblank12", "true").put("isblank15", "true").put("isblank14", "true"));
    return jsonArray;
  }

  public JSONArray createResponseForTestRecursiveLoadPartialWhere(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81").put("value1", "486786104").put("isblank1", "false").put("isblank5", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value1", "1094996859").put("isblank1", "false").put("isblank5", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value1", "317989244").put("isblank1", "false").put("isblank5", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value5", "3d2a7cb378372325c9329e2938452eb2").put("value1", "1311774767").put("isblank1", "false").put("isblank5", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/09489aa4-9574-3972-8eed-3919e4cb85ee").put("value1", "1637003915").put("isblank1", "false").put("isblank5", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));

    return jsonArray;
  }

  public JSONArray createResponseForTestLoadAllWhere_1(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "a8d946b1eae26a772283da9c3c51c117").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "0.666633888087799").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "4bdf502ba0c332ab3bd4eb85babdf53a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "932700a34b5697636535e1b43ba342d3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "94a9593c1410d0007c88e8e1c04c5500").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "4ebafc8420489dbf3463d30d1eb55aa5").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "c954314c9c1753deeb73ab24916f7398").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "a51174640d158d6b9f15aa3323cfa1ab").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "49db66264174cf6021ed272dfd002b3a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "486786104").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "6a9a2eaca4934b3fbe8f6dd7a6ca53b2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "93bb4473b77e3a4a6504cd103cf37b78").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "https://eg/examplenamespace/randomuris/-727373186").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "dd455da4f9fe3084fd9c977f11b49f9e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "288736b0cd3e71ab313b46bb8db01b28").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "randomString-2050421886").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "1025dde2b8f71597a43f14d8edfb2bf3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "0.9096600288931762").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "91fdbcabb9b7e6b3f3a1d39d25ef555e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "51f3c3337a5d13f7a3068d3e424ffce0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "f838f680692397b3bde3b28b9bd967b7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "40d1252c24c5cb1772fd63e5a4b75701").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "19ab66842a27dad68f5c317853f66e2f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "8824fede5f53230cfc6b89d4688ad22f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "65e582e1d56b2902d6341dcddcfff6c0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "317989244").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "35fc3c33202695aa293482b297b09c8a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "baa2e41978257e9e89a2632d78077785").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "https://eg/examplenamespace/randomuris/750723155").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "7c90fab79f30df42fa8d4eddc60ff60f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "b3819113e4964e62c92ed85fc76adf56").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "randomString558130596").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "1025dde2b8f71597a43f14d8edfb2bf3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "0.3475288646103122").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "91fdbcabb9b7e6b3f3a1d39d25ef555e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "51f3c3337a5d13f7a3068d3e424ffce0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "f838f680692397b3bde3b28b9bd967b7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "40d1252c24c5cb1772fd63e5a4b75701").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "19ab66842a27dad68f5c317853f66e2f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "8824fede5f53230cfc6b89d4688ad22f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "65e582e1d56b2902d6341dcddcfff6c0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "1094996859").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "35fc3c33202695aa293482b297b09c8a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "baa2e41978257e9e89a2632d78077785").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "https://eg/examplenamespace/randomuris/1905807410").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "7c90fab79f30df42fa8d4eddc60ff60f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "b3819113e4964e62c92ed85fc76adf56").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "randomString1525380402").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "5888c9d846489b335cac40ab1132f2fa").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "0.04699505989148167").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "71d095e54704609c0e8f5118eff199d2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "027156f738071ed44cb379298d21489c").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "c1d174c93a75e7204a375c8f25d7bf39").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "8c2a9a3bfc1e8e02dbaf6630bf23e2e5").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "ab9f68e3468e4ee9d326c09adc6535dc").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "2a0e3a307f8bb4f4e94812e2d4ed2929").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "376d7d06f74642b0522828962c9f23ac").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "1311774767").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "c864935e37e03234d1cdad050b5f4a62").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "75e3f27904d1a1d5ab2aae2955a399d2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "da0e7d1159050a7ffdf8f794b12aedfe").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "https://eg/examplenamespace/randomuris/-1794331249").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "ab3769efa9e87aed22461b0b74133d18").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "8f51f8313e98005b95123c1697782bd0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "randomString-2069892745").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "16afff2244ed0484ee9a00381521764d").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "0.641973939628295").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "6bca02af4a3dfcb4ab4e84f6e19413e7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "0bff0f1e7fd111b743c8f786b8cc5695").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "7aa344aad13e9c6e856312b896d68adb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "078220016fff464b95c465f14ffccb68").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "c8709dce33e9bda27ffe62dbce29a68b").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "d556e9c5f76a83fbfafee97ee1a90ef8").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "38369a97ddaad1f91e7bcc719a90cfba").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "1637003915").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "ccbee93cc2509f800f61bb261e3a8251").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "https://eg/examplenamespace/09489aa4-9574-3972-8eed-3919e4cb85ee").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "151b537d9b1e8ed8f89aee9c51709e90").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "https://eg/examplenamespace/randomuris/-169596869").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "dd9d63edf33708bfad584408eeba1d94").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "0d997bfc24dbbe0b07d8eeb3dec1c2f9").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "randomString478919291").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createResponseForTestLoadAllWhere_2(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "https://eg/examplenamespace/randomuris/874244884").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "https://eg/examplenamespace/0bbe0442-1589-3ab9-a026-19ddafc100fe").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "https://eg/examplenamespace/randomuris/-310488746").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "https://eg/examplenamespace/4bb4eeac-5793-3eee-b6d5-d453b9f759bd").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "https://eg/examplenamespace/randomuris/1657489626").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "https://eg/examplenamespace/ca1036ea-b751-3ae9-b749-fc4f16897fc9").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "https://eg/examplenamespace/randomuris/1644384244").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1").put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "https://eg/examplenamespace/randomuris/370504892").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createResponseForTestLoadPartialWhere(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value1", "486786104").put("isblank1", "false").put("model", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value1", "1094996859").put("isblank1", "false").put("model", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value1", "317989244").put("isblank1", "false").put("model", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value1", "1311774767").put("isblank1", "false").put("model", "https://eg/examplenamespace/868aa231-a97d-36d8-990d-b6b1863345d1")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"))
            .put(new JSONObject().put("value1", "1637003915").put("isblank1", "false").put("model", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));

    return jsonArray;
  }

  public JSONArray forwardVectorResponse(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank","false").put("value", "0.34911535662488336"))
            .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false").put("value", "0.9138466810904882"))
            .put(new JSONObject().put("isblank", "true").put("value", "af0f3876990654840d89538a90bbc54c"));

    return jsonArray;
  }
  public JSONArray backwardVectorResponse(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("isblank","false").put("value", "https://eg/examplenamespace/randomuris/1924478780"))
            .put(new JSONObject().put("isblank","true").put("value", "e636a2847c83a665ba5a8b1049c7527f"))
            .put(new JSONObject().put("isblank","true").put("value", "e56cc5e09e37ce073bd25f5085e2b76c"));

    return jsonArray;
  }

  public JSONArray createModelResponse_false_1(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("value", "a8d946b1eae26a772283da9c3c51c117").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.666633888087799").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("value", "4bdf502ba0c332ab3bd4eb85babdf53a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("value", "932700a34b5697636535e1b43ba342d3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("value", "94a9593c1410d0007c88e8e1c04c5500").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("value", "4ebafc8420489dbf3463d30d1eb55aa5").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("value", "c954314c9c1753deeb73ab24916f7398").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("value", "a51174640d158d6b9f15aa3323cfa1ab").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("value", "49db66264174cf6021ed272dfd002b3a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("value", "486786104").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("value", "6a9a2eaca4934b3fbe8f6dd7a6ca53b2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("value", "93bb4473b77e3a4a6504cd103cf37b78").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/-727373186").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("value", "dd455da4f9fe3084fd9c977f11b49f9e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("value", "288736b0cd3e71ab313b46bb8db01b28").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("value", "randomString-2050421886").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_true_1(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/874244884").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_false_2(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("value", "a8d946b1eae26a772283da9c3c51c117").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.7291793551928549").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("value", "4bdf502ba0c332ab3bd4eb85babdf53a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("value", "932700a34b5697636535e1b43ba342d3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("value", "94a9593c1410d0007c88e8e1c04c5500").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("value", "4ebafc8420489dbf3463d30d1eb55aa5").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("value", "c954314c9c1753deeb73ab24916f7398").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("value", "a51174640d158d6b9f15aa3323cfa1ab").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("value", "49db66264174cf6021ed272dfd002b3a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("value", "-46600813").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("value", "6a9a2eaca4934b3fbe8f6dd7a6ca53b2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/0bbe0442-1589-3ab9-a026-19ddafc100fe").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("value", "93bb4473b77e3a4a6504cd103cf37b78").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/-1766726353").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("value", "dd455da4f9fe3084fd9c977f11b49f9e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("value", "288736b0cd3e71ab313b46bb8db01b28").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("value", "randomString1073390493").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_true_2(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/-2049138693").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_false_3() {

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("value", "1025dde2b8f71597a43f14d8edfb2bf3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.9096600288931762").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("value", "91fdbcabb9b7e6b3f3a1d39d25ef555e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("value", "51f3c3337a5d13f7a3068d3e424ffce0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("value", "f838f680692397b3bde3b28b9bd967b7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("value", "40d1252c24c5cb1772fd63e5a4b75701").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("value", "19ab66842a27dad68f5c317853f66e2f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("value", "8824fede5f53230cfc6b89d4688ad22f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("value", "65e582e1d56b2902d6341dcddcfff6c0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("value", "317989244").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("value", "35fc3c33202695aa293482b297b09c8a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("value", "baa2e41978257e9e89a2632d78077785").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/750723155").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("value", "7c90fab79f30df42fa8d4eddc60ff60f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("value", "b3819113e4964e62c92ed85fc76adf56").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("value", "randomString558130596").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_true_3(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/0bbe0442-1589-3ab9-a026-19ddafc100fe").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/-310488746").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));
    return jsonArray;
  }
  public JSONArray createModelResponse_false_4(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("value", "16afff2244ed0484ee9a00381521764d").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.641973939628295").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("value", "6bca02af4a3dfcb4ab4e84f6e19413e7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("value", "0bff0f1e7fd111b743c8f786b8cc5695").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("value", "7aa344aad13e9c6e856312b896d68adb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("value", "078220016fff464b95c465f14ffccb68").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("value", "c8709dce33e9bda27ffe62dbce29a68b").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("value", "d556e9c5f76a83fbfafee97ee1a90ef8").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("value", "38369a97ddaad1f91e7bcc719a90cfba").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("value", "1637003915").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("value", "ccbee93cc2509f800f61bb261e3a8251").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/09489aa4-9574-3972-8eed-3919e4cb85ee").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("value", "151b537d9b1e8ed8f89aee9c51709e90").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/-169596869").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("value", "dd9d63edf33708bfad584408eeba1d94").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("value", "0d997bfc24dbbe0b07d8eeb3dec1c2f9").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("value", "randomString478919291").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_true_4(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/1d680cfb-9097-3a7e-96d3-ffa7c6cf6aea").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/370504892").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));
    return jsonArray;
  }
  public JSONArray createModelResponse_false_5(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("value", "1025dde2b8f71597a43f14d8edfb2bf3").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.3475288646103122").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("value", "91fdbcabb9b7e6b3f3a1d39d25ef555e").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("value", "51f3c3337a5d13f7a3068d3e424ffce0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("value", "f838f680692397b3bde3b28b9bd967b7").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("value", "40d1252c24c5cb1772fd63e5a4b75701").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("value", "19ab66842a27dad68f5c317853f66e2f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("value", "8824fede5f53230cfc6b89d4688ad22f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("value", "65e582e1d56b2902d6341dcddcfff6c0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("value", "1094996859").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("value", "35fc3c33202695aa293482b297b09c8a").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("value", "baa2e41978257e9e89a2632d78077785").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/1905807410").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("value", "7c90fab79f30df42fa8d4eddc60ff60f").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("value", "b3819113e4964e62c92ed85fc76adf56").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("value", "randomString1525380402").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_true_5(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/4bb4eeac-5793-3eee-b6d5-d453b9f759bd").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/1657489626").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));
    return jsonArray;
  }
  public JSONArray createModelResponse_false_6(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("value", "5888c9d846489b335cac40ab1132f2fa").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.04699505989148167").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("value", "71d095e54704609c0e8f5118eff199d2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("value", "027156f738071ed44cb379298d21489c").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("value", "c1d174c93a75e7204a375c8f25d7bf39").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("value", "8c2a9a3bfc1e8e02dbaf6630bf23e2e5").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("value", "ab9f68e3468e4ee9d326c09adc6535dc").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("value", "2a0e3a307f8bb4f4e94812e2d4ed2929").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("value", "376d7d06f74642b0522828962c9f23ac").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("value", "1311774767").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("value", "c864935e37e03234d1cdad050b5f4a62").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "true")
                    .put("value", "75e3f27904d1a1d5ab2aae2955a399d2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("value", "da0e7d1159050a7ffdf8f794b12aedfe").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/-1794331249").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("value", "ab3769efa9e87aed22461b0b74133d18").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("value", "8f51f8313e98005b95123c1697782bd0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("value", "randomString-2069892745").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_true_6(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/ca1036ea-b751-3ae9-b749-fc4f16897fc9").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/1644384244").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));
    return jsonArray;
  }
  public JSONArray createModelResponse_false_7(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/bigintprop").put("isblank", "true")
                    .put("value", "5888c9d846489b335cac40ab1132f2fa").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doubleprop").put("datatype","http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                    .put("value", "0.8335549161855437").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/doublepropnull").put("isblank", "true")
                    .put("value", "71d095e54704609c0e8f5118eff199d2").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest1a").put("isblank", "true")
                    .put("value", "027156f738071ed44cb379298d21489c").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph1"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2a").put("isblank", "true")
                    .put("value", "c1d174c93a75e7204a375c8f25d7bf39").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest2b").put("isblank", "true")
                    .put("value", "8c2a9a3bfc1e8e02dbaf6630bf23e2e5").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph2"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3a").put("isblank", "true")
                    .put("value", "ab9f68e3468e4ee9d326c09adc6535dc").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3b").put("isblank", "true")
                    .put("value", "2a0e3a307f8bb4f4e94812e2d4ed2929").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/graphtest3c").put("isblank", "true")
                    .put("value", "376d7d06f74642b0522828962c9f23ac").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/graph3"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intprop").put("datatype","http://www.w3.org/2001/XMLSchema#int").put("isblank", "false")
                    .put("value", "-91097177").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/intpropnull").put("isblank", "true")
                    .put("value", "c864935e37e03234d1cdad050b5f4a62").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/258efc84-fa19-3942-9c24-5529c4e22aa1").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelpropnull").put("isblank", "true")
                    .put("value", "da0e7d1159050a7ffdf8f794b12aedfe").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/1836043328").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/uripropnull").put("isblank", "true")
                    .put("value", "ab3769efa9e87aed22461b0b74133d18").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringpropnull").put("isblank", "true")
                    .put("value", "8f51f8313e98005b95123c1697782bd0").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#stringprop").put("datatype","http://www.w3.org/2001/XMLSchema#string").put("isblank", "false")
                    .put("value", "randomString293504500").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));

    return jsonArray;
  }
  public JSONArray createModelResponse_true_7(){

    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("predicate", "http://dbpedia.org/ontology/modelprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuriprop").put("isblank", "false")
                    .put("value", "https://eg/examplenamespace/randomuris/428541664").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
            .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#backuripropnull").put("isblank", "true")
                    .put("value", "063bcf549db644be09cd72cbff0c36cb").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"));
    return jsonArray;
  }

  public JSONArray createModelResponse_1(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/1d5fd7f8-5cb6-3dcf-88d8-edadc309dc81").put("value1", "486786104").put("isblank1", "false").put("isblank5", "false")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));
    return jsonArray;
  }
  public JSONArray createModelResponse_2(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/0bbe0442-1589-3ab9-a026-19ddafc100fe").put("value1", "-46600813").put("isblank1", "false").put("isblank5", "false")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));
    return jsonArray;
  }
  public JSONArray createModelResponse_3(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75").put("value1", "1094996859").put("isblank1", "false").put("isblank5", "false")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));
    return jsonArray;
  }
  public JSONArray createModelResponse_4(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/8e92cd4c-6c42-37b4-ac42-393ef3c08cda").put("value1", "317989244").put("isblank1", "false").put("isblank5", "false")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));
    return jsonArray;
  }
  public JSONArray createModelResponse_5(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/09489aa4-9574-3972-8eed-3919e4cb85ee").put("value1", "1637003915").put("isblank1", "false").put("isblank5", "false")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));
    return jsonArray;
  }
  public JSONArray createModelResponse_6(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "3d2a7cb378372325c9329e2938452eb2").put("value1", "1311774767").put("isblank1", "false").put("isblank5", "true")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));
    return jsonArray;
  }
  public JSONArray createModelResponse_7(){
    JSONArray jsonArray = new JSONArray()
            .put(new JSONObject().put("value5", "https://eg/examplenamespace/258efc84-fa19-3942-9c24-5529c4e22aa1").put("value1", "-91097177").put("isblank1", "false").put("isblank5", "false")
                    .put("datatype1", "http://www.w3.org/2001/XMLSchema#int"));
    return jsonArray;
  }

}
