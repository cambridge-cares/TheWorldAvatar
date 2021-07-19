package uk.ac.cam.cares.jps.base.listener.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.ModelFactory;
import org.junit.Assert;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.listener.BaseOntologyModelManager;

import java.io.File;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;


public class BaseOntologyModelManagerTest {

    @Test
    public void testSave() throws InterruptedException{
        OntModel testM = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        String testIRI = "testIRI";
        String testMmsi = "testMmsi";

        ExecutorService threadPool = Executors.newFixedThreadPool(5);
        for (int i = 0; i < 5; i++) {
            threadPool.execute(() -> {
                BaseOntologyModelManager.save(testM, testIRI, testMmsi);
            });
        }
        threadPool.shutdown();
        TimeUnit.SECONDS.sleep(5);

    }

    @Test
    public void testSaveToOwl()  throws Exception {
        OntModel testM = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        String testIRI = "testIRI";
        String testMmsi = "testMmsi";
        File file = new File("/Library/tomcat/apache-tomcat-8.5.68/webapps/ROOT/kb/ships/testMmsi/Chimney-1.owl"); //mac path, should be changed to local development

        BaseOntologyModelManager.saveToOwl(testM, testIRI, testMmsi);
        Assert.assertTrue(file.exists());

    }

    @Test
    public void testPrepareDirectory() {
        String testFilePath2 = "/Users/yjytt59/Documents/test/test"; //mac path, should be changed to local development
        File file = new File("/Users/yjytt59/Documents/test");
        try{
            BaseOntologyModelManager.prepareDirectory(testFilePath2);
            Assert.assertTrue(file.isDirectory());
            Assert.assertTrue(file.list().length<1);
        }catch (Exception e){
            Assert.assertTrue(e.getMessage().contains("No such directory: "));
        }

    }

    @Test
    public void testQuery() {
        OntModel testM = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        String sparql = "PREFIX owl:<http://www.w3.org/2002/07/owl#> \r\n" +
                "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> \r\n" +
                "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> \r\n" +
                "SELECT ?x \r\nWHERE {\r\n?o a owl:test .\r\n?o rdfs:jps ?x .\r\n?o rdf:worldavatar/rdfs:jps ?x .\r\n}";

        Assert.assertNotNull(BaseOntologyModelManager.query(sparql, testM));
    }
}