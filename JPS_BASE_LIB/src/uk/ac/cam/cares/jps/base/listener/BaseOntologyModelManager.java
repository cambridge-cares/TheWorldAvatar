package uk.ac.cam.cares.jps.base.listener;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ConcurrentHashMap;

@WebListener
public class BaseOntologyModelManager implements ServletContextListener {

    protected static ConcurrentHashMap<String, Resource> conceptMap = new ConcurrentHashMap<>();

    @Override
    public void contextInitialized(ServletContextEvent sce) {

    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {

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


}
