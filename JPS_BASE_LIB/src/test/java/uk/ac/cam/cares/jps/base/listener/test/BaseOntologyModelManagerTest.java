package uk.ac.cam.cares.jps.base.listener.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.VCARD;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.*;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.listener.BaseOntologyModelManager;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.mockito.Mockito.*;

public class BaseOntologyModelManagerTest {

    @Rule
    public TemporaryFolder folder= new TemporaryFolder();

    @Test
    public void testGetConcept() throws Exception{
        Model testModel = ModelFactory.createDefaultModel();
        ConcurrentHashMap<String, Resource> testMap = new ConcurrentHashMap<>();
        Resource r1 = testModel.createResource("http://somewhere/test1");
        Resource r2 = testModel.createResource("http://somewhere/test2");
        testMap.put("test1", r1);
        testMap.put("test2", r2);

        Field testConcept = BaseOntologyModelManager.class.getDeclaredField("conceptMap");
        testConcept.setAccessible(true);
        testConcept.set(null, testMap);

        Assert.assertEquals("http://somewhere/test2", BaseOntologyModelManager.getConcept("test2").toString());

    }

    @Test
    public void testSave(){
        OntModel testM = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        String testIRI = "testIRI";
        String testMmsi = "testMmsi";

        try{
            ExecutorService threadPool = Executors.newFixedThreadPool(5);
            for (int i = 0; i < 5; i++) {
                threadPool.execute(() -> {
                    BaseOntologyModelManager.save(testM, testIRI, testMmsi);
                });
            }
            threadPool.shutdown();
            TimeUnit.SECONDS.sleep(5);
        }catch (Exception e){
            Assert.assertTrue(e.getMessage().contains("Saving OWL failed: "));
        }
    }


    @Test
    public void testSaveToOwl() throws Exception {
        File testFolder= folder.newFolder("/test");

        String ABSDIR_ROOT_TEST = testFolder.getPath();
        String ABSDIR_KB_TEST = ABSDIR_ROOT_TEST + "/kb/";
        String IRI_KB = "http://www.test.com/kb/";


        File file1 = new File(ABSDIR_KB_TEST + "/ships/testMmsi/Chimney-1.owl");
        File file2 =

//        File testFolder= folder.newFolder("testIRI/test");

        OntModel testM = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        String[] testURI ={"http://somewhere/test1"};
        String[] testData = {"test1"};

        for (int i=0;i<testURI.length;i++){
            Resource testR = testM.createResource(testURI[i]);
            testR.addProperty(VCARD.FN, testData[i]);
        }

        String testIRI = testFolder.getPath() + "/testIRI#test";
        String testMmsi = "testMmsi";

        MockedStatic<AgentLocator> mockA = Mockito.mockStatic(AgentLocator.class);

        mockA.when(AgentLocator::isJPSRunningForTest).thenReturn(false);
        BaseOntologyModelManager.saveToOwl(testM, testIRI, testMmsi);
        Assert.assertTrue(testFolder.exists());

        mockA.when(AgentLocator::isJPSRunningForTest).thenReturn(true);
        BaseOntologyModelManager.saveToOwl(testM, testIRI, testMmsi);
        Assert.assertTrue(file1.exists());


   }

    @Test
    public void testPrepareDirectory() throws IOException {
        File createdFolder= folder.newFolder("testFolder");
        File testFolder= folder.newFolder("testFolder/test");
        File testFile= folder.newFile("testFolder/test.owl");
        String testFilePath2 = createdFolder.getPath() + "/test";
        String testFilePath1 = createdFolder.getPath() + "/testFolder1/test";

        BaseOntologyModelManager.prepareDirectory(testFilePath2);
        Assert.assertTrue(createdFolder.isDirectory());
        Assert.assertFalse(testFile.exists());
        Assert.assertFalse(testFolder.exists());

        File testFile1 = folder.newFolder("testFolder1/test");
        BaseOntologyModelManager.prepareDirectory(testFilePath1);
        Assert.assertTrue(testFile1.exists());

    }

    @Test
    public void testQuery() {
        OntModel testM = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        String[] personURI ={ "http://somewhere/test", "http://somewhere/test",
                "http://somewhere/test3","http://somewhere/test4"};
        String[] testData = {"test1","test2","test3","test4"};

        for (int i=0;i<personURI.length;i++){
            Resource person = testM.createResource(personURI[i]);
            person.addProperty(VCARD.FN, testData[i]);
        }

        String sparql = "SELECT ?z WHERE{<http://somewhere/test> ?y ?z}";
        ResultSet testrs = BaseOntologyModelManager.query(sparql, testM);
        String testRes = "";
        while (testrs.hasNext()) {
            QuerySolution qs = testrs.nextSolution();
            testRes = testRes + qs.get("z").toString() + "\n";
        }
        Assert.assertEquals("test2\ntest1\n", testRes);

    }

}