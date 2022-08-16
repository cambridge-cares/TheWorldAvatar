package uk.ac.cam.cares.jps.base.listener;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.servlet.ServletContextEvent;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.shared.Lock;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class BaseOntologyModelManager {

    private static final String IRI_BASE = "http://www.theworldavatar.com";
    protected static final String IRI_KB = IRI_BASE + "/kb/";
    private static final String EX_SAVE_OWL =  "Saving OWL failed: ";
    static final String ABSDIR_ROOT = "C://TOMCAT/webapps/ROOT";
    private static final String ABSDIR_KB = ABSDIR_ROOT + "/kb/";
    // now reading from JPS_BASE_LIB/src/main/resources in jpstest.properties
    protected static final String ABSDIR_ROOT_TEST =  KeyValueMap.getProperty("/jpstest.properties",IKeys.ABSDIR_ROOT);
    private static final String ABSDIR_KB_TEST = ABSDIR_ROOT_TEST + "/kb/";
    private static final String IRI_BASE_TEST = "http://localhost:8080";
    protected static final String IRI_KB_TEST = IRI_BASE_TEST + "/kb/";

    static ConcurrentHashMap<String, Resource> conceptMap = new ConcurrentHashMap<>();
    static OntModel baseEntityModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
    private static Logger LOGGER = LogManager.getLogger(BaseOntologyModelManager.class);

    public void contextDestroyed(ServletContextEvent sce) {
        baseEntityModel.close();
    }

    protected static OntModel createModelFromString(String content) {
        byte[] contentBytes = content.getBytes(StandardCharsets.UTF_8);
        OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        model.read(new ByteArrayInputStream(contentBytes), null);
        return model;
    }

    public static Resource getConcept(String name) {
        return conceptMap.get(name);
    }

    public static void save(OntModel jenaOwlModel, String iriOfChimney, String mmsi) {
        ReadWriteLock readWriteLock = new ReentrantReadWriteLock();
        readWriteLock.writeLock().lock();
        try {
            saveToOwl(jenaOwlModel, iriOfChimney, mmsi);
        } catch (IOException e) {
            throw new JPSRuntimeException(EX_SAVE_OWL + iriOfChimney);
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public static void saveToOwl(OntModel jenaOwlModel, String iriOfChimney, String mmsi) throws IOException {
        String filePath2;
        if (!AgentLocator.isJPSRunningForTest()) {
            filePath2= iriOfChimney.replaceAll(IRI_KB, ABSDIR_KB).split("#")[0];
        } else {
            filePath2= Paths.get(ABSDIR_KB_TEST,"ships",mmsi,"Chimney-1.owl").toString();
        }
        
        LOGGER.info("Created file is at: {}", filePath2);

        try {
            prepareDirectory(filePath2);
        } catch (IOException e) {
            throw new JPSRuntimeException(EX_SAVE_OWL + filePath2);
        } finally {
            FileOutputStream out = new FileOutputStream(filePath2);

            jenaOwlModel.write(out, "RDF/XML-ABBREV");
            out.close();
        }
//        String finalcontent=JenaHelper.writeToString(jenaOwlModel);
//        new QueryBroker().putOld(iriOfChimney,finalcontent);
    }

    public static void prepareDirectory(String filePath2) throws IOException {
        File stockDir = new File(filePath2).getParentFile();

        boolean stockdir = true;

        if (!stockDir.exists()) {
            stockdir = stockDir.mkdirs();
        }
        if (stockdir) {
            File[] listOfFiles = stockDir.listFiles();
            if (listOfFiles != null) {
                for (File listOfFile : listOfFiles) {
                    listOfFile.delete();
                    //@todo AC: work on general concurrent filesystem access solution for JPS
                	/*
                    if (!listOfFile.delete()) {
                        throw new IOException("Could not clean up: " + filePath2);
                    }*/
                }
            }
        } else {
            throw new IOException("No such directory: " + filePath2);
        }
    }

    public static ResultSet query(String sparql, OntModel model) {
        Query query = QueryFactory.create(sparql);
        ResultSet rs;
        model.enterCriticalSection(Lock.READ);
        try {
            QueryExecution queryExec = QueryExecutionFactory.create(query, model);
            rs = queryExec.execSelect();
        } finally {
            model.leaveCriticalSection() ;
        }

        return ResultSetFactory.copyResults(rs);
    }

}
