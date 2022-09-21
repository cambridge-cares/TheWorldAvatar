package uk.ac.cam.cares.ogm.models.test;

import org.apache.commons.lang.ArrayUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import uk.ac.cam.cares.ogm.models.*;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public class RecursivePullSessionTest {

    private static final String testResourceId = "http://localhost:48080/test";
    private static final String testNamespace = "http://localhost:9999/blazegraph/namespace/test/sparql/";

    @Test
    public void testNewRecursivePullSession(){

        assertEquals(7, RecursivePullSession.class.getDeclaredFields().length);
        assertEquals(2, RecursivePullSession.class.getDeclaredMethods().length);

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
                .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#geometryprop")
                        .put("isblank","true").put("value", "43b6bcab4c10dcdc83428db841d08f72").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
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
                .put(new JSONObject().put("predicate", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#geometryprop")
                        .put("isblank","true").put("value", "94cd358a2d2f0e6aae8b6fb60033b893").put("graph", "http://localhost:9999/blazegraph/namespace/test/sparql/testmodels"))
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

    @Test
    public void testexecute(){

        ModelContext pushContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));
        ModelContext pullContext = Mockito.spy(new ModelContext(testResourceId, testNamespace));
        ModelContext pullContext1 = Mockito.spy(new ModelContext(testResourceId, testNamespace));

        Mockito.doNothing().when(pushContext).update(Mockito.contains("CLEAR ALL"));
        pushContext.update("CLEAR ALL");
        Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("CLEAR ALL"));

        TestModel pushModel = TestModel.createRandom(pushContext, 12345, 3, 3);
        Mockito.doNothing().when(pushContext).update(Mockito.contains("INSERT DATA"));
        pushContext.pushAllChanges();
        Mockito.verify(pushContext, Mockito.times(1)).update(Mockito.contains("INSERT DATA"));

        TestModel pullModel = pullContext.createHollowModel(TestModel.class, pushModel.getIri());
        TestModel pullModel1 = pullContext1.createHollowModel(TestModel.class, pushModel.getIri());

        //pullAll
        RecursivePullSession pullSession = new RecursivePullSession(1, pullContext);

        JSONArray jsonArray1_0 = createResponse1_0();
        JSONArray jsonArray2_0 = createResponse2_0();
        JSONArray jsonArray1_1 = createResponse1_1();
        JSONArray jsonArray2_1 = createResponse2_1();
        Method buildQuery;
        SelectBuilder query1_0 = null;
        SelectBuilder query2_0 = null;
        SelectBuilder query1_1 = null;
        SelectBuilder query2_1 = null;

        try {
            buildQuery = pullContext.getClass().getDeclaredMethod("buildPullAllInDirectionQuery", Node.class, boolean.class);
            buildQuery.setAccessible(true);
            query1_0 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), false);
            query2_0 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), true);
            query1_1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), false);
            query2_1 = (SelectBuilder) buildQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), true);
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            e.printStackTrace();
        }

        Mockito.doReturn(jsonArray1_0).when(pullContext).query(query1_0.buildString());
        Mockito.doReturn(jsonArray2_0).when(pullContext).query(query2_0.buildString());
        Mockito.doReturn(jsonArray1_1).when(pullContext).query(query1_1.buildString());
        Mockito.doReturn(jsonArray2_1).when(pullContext).query(query2_1.buildString());

        pullContext.currentPullSession = pullSession;
        pullSession.queue(pullModel);
        pullSession.execute();

        Mockito.verify(pullContext, Mockito.times(1)).query(query1_0.buildString());
        Mockito.verify(pullContext, Mockito.times(1)).query(query2_0.buildString());
        Mockito.verify(pullContext, Mockito.times(1)).query(query1_1.buildString());
        Mockito.verify(pullContext, Mockito.times(1)).query(query2_1.buildString());

        assertEquals(pushModel, pullModel);
        assertEquals(pushModel.getModelProp(), pullModel.getModelProp());
        assertNotEquals(pushModel.getModelProp().getModelProp(), pullModel.getModelProp().getModelProp());
        pullContext.currentPullSession = null;

        //pullPartial
        JSONArray scalarsResponse_1 = new JSONArray()
                .put(new JSONObject().put("value5", "https://eg/examplenamespace/4771c262-0f35-32c8-8865-a04b1a6c2e5d")
                        .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString-287790814")
                        .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
        JSONArray scalarsResponse_2 = new JSONArray()
                .put(new JSONObject().put("value5", "https://eg/examplenamespace/0ed64570-dc61-3703-9f4c-f8975b068b75")
                        .put("isblank0", "false").put("isblank5", "false").put("value0", "randomString1525380402")
                        .put("datatype0", "http://www.w3.org/2001/XMLSchema#string"));
        JSONArray vectorResponse_1 = new JSONArray()
                .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                        .put("value", "0.34911535662488336"))
                .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                        .put("value", "0.9138466810904882"))
                .put(new JSONObject().put("isblank", "true").put("value", "e58e22743ed2e618c10374a1b81bfb60"));
        JSONArray vectorResponse_2 = new JSONArray()
                .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                        .put("value", "0.13561788626175464"))
                .put(new JSONObject().put("datatype", "http://www.w3.org/2001/XMLSchema#double").put("isblank", "false")
                        .put("value", "0.517410013055376"))
                .put(new JSONObject().put("isblank", "true").put("value", "75373e9184271c016214d273b8623a98"));

        Method buildScalarsQuery;
        SelectBuilder scalarsQuery_1 = null;
        SelectBuilder scalarsQuery_2 = null;
        SelectBuilder vectorQuery_1 = null;
        SelectBuilder vectorQuery_2 = null;
        Method buildVectorQuery;

        String[] fieldNames = {"stringProp", "modelProp", "forwardVector"};
        try {
            buildScalarsQuery = pullContext.getClass().getDeclaredMethod("buildScalarsQuery", Node.class, MetaModel.class, String[].class);
            buildScalarsQuery.setAccessible(true);
            buildVectorQuery = pullContext.getClass().getDeclaredMethod("buildVectorQuery", Node.class, FieldKey.class);
            buildVectorQuery.setAccessible(true);

            scalarsQuery_1 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), MetaModel.get(TestModel.class), fieldNames);
            scalarsQuery_2 = (SelectBuilder) buildScalarsQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), MetaModel.get(TestModel.class), fieldNames);
            for (Map.Entry<FieldKey, FieldInterface> entry : MetaModel.get(TestModel.class).vectorFieldList) {
                if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, entry.getValue().field.getName())) continue;
                vectorQuery_1 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getIri()), entry.getKey()));
                vectorQuery_2 = ((SelectBuilder) buildVectorQuery.invoke(pullContext, NodeFactory.createURI(pushModel.getModelProp().getIri()), entry.getKey()));
            }
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            e.printStackTrace();
        }

        Mockito.doReturn(scalarsResponse_1).when(pullContext1).query(scalarsQuery_1.buildString());
        Mockito.doReturn(scalarsResponse_2).when(pullContext1).query(scalarsQuery_2.buildString());
        Mockito.doReturn(vectorResponse_1).when(pullContext1).query(vectorQuery_1.buildString());
        Mockito.doReturn(vectorResponse_2).when(pullContext1).query(vectorQuery_2.buildString());

        RecursivePullSession pullSession1 = new RecursivePullSession(1, pullContext1, fieldNames);

        pullContext1.currentPullSession = pullSession1;
        pullSession1.queue(pullModel1);
        pullSession1.execute();

        Mockito.verify(pullContext1, Mockito.times(1)).query(scalarsQuery_1.buildString());
        Mockito.verify(pullContext1, Mockito.times(1)).query(scalarsQuery_2.buildString());
        Mockito.verify(pullContext1, Mockito.times(1)).query(vectorQuery_1.buildString());
        Mockito.verify(pullContext1, Mockito.times(1)).query(vectorQuery_2.buildString());

        assertEquals(pushModel.getStringProp(), pullModel1.getStringProp());
        assertEquals(pushModel.getModelProp().getStringProp(), pullModel1.getModelProp().getStringProp());
        assertNotEquals(pushModel.getModelProp().getModelProp().getStringProp(), pullModel1.getModelProp().getModelProp().getStringProp());
        assertEquals(new HashSet<>(pushModel.getForwardVector()), new HashSet<>(pullModel1.getForwardVector()));
        assertEquals(new HashSet<>(pushModel.getModelProp().getForwardVector()), new HashSet<>(pullModel1.getModelProp().getForwardVector()));
        assertNotEquals(new HashSet<>(pushModel.getModelProp().getModelProp().getForwardVector()), new HashSet<>(pullModel1.getModelProp().getModelProp().getForwardVector()));
    }

    @Test
    public void testqueue(){

        ModelContext context = new ModelContext(testResourceId, testNamespace);
        RecursivePullSession pullSession = new RecursivePullSession(1, context);
        TestModel testModel = TestModel.createRandom(context, 12345, 3, 2);

        Field traversedIris;
        Field pendingPullQueue;
        Set<String> iris;
        Queue<Model> pullQueue;
        try {
            traversedIris = pullSession.getClass().getDeclaredField("traversedIris");
            pendingPullQueue = pullSession.getClass().getDeclaredField("pendingPullQueue");
            traversedIris.setAccessible(true);
            pendingPullQueue.setAccessible(true);

            pullSession.queue(testModel);

            iris = (Set<String>) traversedIris.get(pullSession);
            pullQueue = (Queue<Model>) pendingPullQueue.get(pullSession);

            assertTrue(iris.contains(testModel.getIri()));
            assertTrue(pullQueue.contains(testModel));

        } catch (NoSuchFieldException | IllegalAccessException e) {
            fail();
        }
    }
}
