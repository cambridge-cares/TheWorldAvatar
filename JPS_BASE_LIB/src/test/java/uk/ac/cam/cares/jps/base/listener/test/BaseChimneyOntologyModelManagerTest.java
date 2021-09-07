package uk.ac.cam.cares.jps.base.listener.test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.listener.BaseChimneyOntologyModelManager;
import uk.ac.cam.cares.jps.base.listener.BaseOntologyModelManager;

public class BaseChimneyOntologyModelManagerTest {

	public static final String SPQ_WASTE = "PREFIX j4:<" + "http://www.theworldavatar.com/ontology/ontocape/" + "chemical_process_system/CPS_function/process.owl#> " +
            "PREFIX j7:<" + "http://www.theworldavatar.com/ontology/" + "meta_model/topology/topology.owl#> " +
            "SELECT ?wasteStream \r\n" +
            "WHERE " +
            "{ ?chimney j7:hasOutput ?wasteStream ." +
            "  ?wasteStream a j4:NonReusableWasteProduct ." +
            "}";
	
    @Test
    public void testGetSpeciesMap() throws Exception{
        Model testModel = ModelFactory.createDefaultModel();
        ConcurrentHashMap<String, Resource> testMap = new ConcurrentHashMap<>();
        Resource r1 = testModel.createResource("http://somewhere/test1");
        Resource r2 = testModel.createResource("http://somewhere/test2");
        testMap.put("test1", r1);
        testMap.put("test2", r2);

        Field testConcept = BaseChimneyOntologyModelManager.class.getDeclaredField("speciesMap");
        testConcept.setAccessible(true);
        testConcept.set(null, testMap);

        Assert.assertEquals(testMap, BaseChimneyOntologyModelManager.getSpeciesMap());
    }
    
    @Test
    public void testGetWasteStreamInfo() {

        OntModel testModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        QueryExecution queryExec = QueryExecutionFactory.create(SPQ_WASTE, testModel);
        ResultSet expectedResult = queryExec.execSelect();
  
        try (MockedStatic<BaseOntologyModelManager> mockedStatic = Mockito.mockStatic(BaseOntologyModelManager.class)) {
            mockedStatic
              .when(() -> BaseOntologyModelManager.query(SPQ_WASTE,testModel))
              .thenReturn(expectedResult);
       
            ResultSet actualResult = BaseChimneyOntologyModelManager.getWasteStreamInfo(testModel);
            
            Assert.assertEquals(expectedResult, actualResult);
          }
    }

}
