package uk.ac.cam.cares.jps.base.listener.test;

import org.apache.jena.ontology.OntDocumentManager;
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
import uk.ac.cam.cares.jps.base.listener.BaseOntologyModelManager;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;

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
    public void testSaveToOwl() throws Exception {

    	// (Re)set default readhook. OntDocumentManager is a singleton and is modified by
    	// uk.ac.cam.cares.jps.base.query.fed.test causing this test to fail.
    	OntDocumentManager.getInstance().setReadHook(new OntDocumentManager.DefaultReadHook());
    	
        // Create a test model that should be written to a file
        OntModel testModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        // The model contains a single triple
        String testURI ="http://somewhere/test1";
        String testData = "test1";
        Resource testR = testModel.createResource(testURI);
        testR.addProperty(VCARD.FN, testData);

        // Create temporary folder for writing the owl file
        File testFolder = folder.newFolder("test");
        // Defining the paths for where to write the owl files
        String filePath1 = Paths.get(testFolder.getAbsolutePath() , "test.owl").toString();
        String filePath2 = Paths.get(testFolder.getAbsolutePath(), "Chimney-1.owl").toString();


        // Set the fields of the BaseOntologyModelManager to use the paths to the test files
        // Related to file1
        String ABSDIR_KB = testFolder.getAbsolutePath();
        String IRI_KB = "http://localhost/kb/";
        // Related to file2
        String ABSDIR_KB_TEST = testFolder.getAbsolutePath();


        Field modifiersField = Field.class.getDeclaredField("modifiers");
        modifiersField.setAccessible(true);

        Field test_ABSDIR_KB = BaseOntologyModelManager.class.getDeclaredField("ABSDIR_KB");
        test_ABSDIR_KB.setAccessible(true);
        modifiersField.setInt( test_ABSDIR_KB, test_ABSDIR_KB.getModifiers() & ~Modifier.FINAL );
        test_ABSDIR_KB.set(null, ABSDIR_KB);

        Field test_ABSDIR_KB_TEST = BaseOntologyModelManager.class.getDeclaredField("ABSDIR_KB_TEST");
        test_ABSDIR_KB_TEST.setAccessible(true);
        modifiersField.setInt( test_ABSDIR_KB_TEST, test_ABSDIR_KB_TEST.getModifiers() & ~Modifier.FINAL );
        test_ABSDIR_KB_TEST.set(null, ABSDIR_KB_TEST);

        Field test_IRI_KB = BaseOntologyModelManager.class.getDeclaredField("IRI_KB");
        test_IRI_KB.setAccessible(true);
        modifiersField.setInt( test_IRI_KB, test_IRI_KB.getModifiers() & ~Modifier.FINAL );
        test_IRI_KB.set(null, IRI_KB);

        String mmsi = "mmsi";

        try (MockedStatic<AgentLocator> mockAgentLocator = Mockito.mockStatic(AgentLocator.class)) {
            mockAgentLocator.when(AgentLocator::isJPSRunningForTest).thenReturn(false);
            String testIRI = filePath1 + "#test";
            BaseOntologyModelManager.saveToOwl(testModel, testIRI, mmsi);
            File file1 = new File(filePath1);
            Assert.assertTrue(file1.exists());
            // Read the saved ontology back in and assert that the property is in there
            OntModel readModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
            readModel.read(filePath1);
            Assert.assertTrue(readModel.isInBaseModel(testR));
            Assert.assertEquals(testData, readModel.getProperty(testR, VCARD.FN).getString());
        }

        try (MockedStatic<AgentLocator> mockAgentLocator = Mockito.mockStatic(AgentLocator.class);
             MockedStatic<Paths> mockPath = Mockito.mockStatic(Paths.class, RETURNS_DEEP_STUBS)) {
            mockAgentLocator.when(AgentLocator::isJPSRunningForTest).thenReturn(true);
            mockPath.when(() -> Paths.get(ABSDIR_KB_TEST, "ships", mmsi,"Chimney-1.owl").toString())
                    .thenReturn(filePath2);

            String testIRI = filePath2 + "#test";
            BaseOntologyModelManager.saveToOwl(testModel, testIRI, mmsi);
            File file2 = new File(filePath2);
            Assert.assertTrue(file2.exists());
            // Read the saved ontology back in and assert that the property is in there
            OntModel readModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
            readModel.read(file2.getAbsolutePath());
            Assert.assertTrue(readModel.isInBaseModel(testR));
            Assert.assertEquals(testData, readModel.getProperty(testR, VCARD.FN).getString());
        }
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