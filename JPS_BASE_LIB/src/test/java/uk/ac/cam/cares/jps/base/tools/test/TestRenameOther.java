package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.tools.RenamingToolMultiUpdate;
import uk.ac.cam.cares.jps.base.tools.RenamingToolTempStore;

class TestRenameOther {

	private String queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
	private String updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/update";
	
	private String target = "http://www.theworldavatar.com/kb/ontokin/ontokin.owl";
	private String replacement = "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl";
	
	@Test
	void testMultipleUpdates() throws ParseException {

		long startTime = System.nanoTime();
		
		KnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint,updateEndpoint);		
		
		RenamingToolMultiUpdate renameTool = new RenamingToolMultiUpdate(target, replacement);
		renameTool.stepSize = 4000000;
		renameTool.perform(kbClient);
		
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println(duration);
	}

	@Test
	void testUndoMultipleUpdates() throws ParseException {

		long startTime = System.nanoTime();
		
		KnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint,updateEndpoint);		
		
		RenamingToolMultiUpdate renameTool = new RenamingToolMultiUpdate(replacement, target);
		renameTool.stepSize = 8000000;
		renameTool.perform(kbClient);
		
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println(duration);
	}
	
	@Test
	void testTempStore() throws ParseException {
	
		String targetQuery = "http://localhost:8080/blazegraph/namespace/testSource/sparql";
		String targetUpdate = "http://localhost:8080/blazegraph/namespace/testSource/update";

		String sourceQuery = "http://localhost:8080/blazegraph/namespace/testTarget/sparql";
		String sourceUpdate = "http://localhost:8080/blazegraph/namespace/testTarget/update";

		long startTime = System.nanoTime();
		
		KnowledgeBaseClient sourceKB = new RemoteKnowledgeBaseClient(sourceQuery,sourceUpdate);
		KnowledgeBaseClient targetKB = new RemoteKnowledgeBaseClient(targetQuery,targetUpdate);
	
		RenamingToolTempStore renameTool = new RenamingToolTempStore(target,replacement);
		renameTool.stepSize = 1000000;
		renameTool.perform(sourceKB, targetKB);
		
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println(duration);
	}
	
	@Test
	void testTempStoreUndo() throws ParseException {
	
		String sourceQuery = "http://localhost:8080/blazegraph/namespace/testSource/sparql";
		String sourceUpdate = "http://localhost:8080/blazegraph/namespace/testSource/update";

		String targetQuery = "http://localhost:8080/blazegraph/namespace/testTarget/sparql";
		String targetUpdate = "http://localhost:8080/blazegraph/namespace/testTarget/update";

		long startTime = System.nanoTime();
		
		KnowledgeBaseClient sourceKB = new RemoteKnowledgeBaseClient(sourceQuery,sourceUpdate);
		KnowledgeBaseClient targetKB = new RemoteKnowledgeBaseClient(targetQuery,targetUpdate);
	
		RenamingToolTempStore renameTool = new RenamingToolTempStore(replacement,target);
		renameTool.stepSize = 2000000;
		renameTool.perform(sourceKB, targetKB);
		
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println(duration);
	}
	
}
