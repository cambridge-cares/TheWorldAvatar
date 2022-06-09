package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.tools.CloningTool;
import uk.ac.cam.cares.jps.base.tools.NewCloningTool;


class NewCloningToolTest {

	@Test
	void testDoubleClone() throws IOException {
		
		String path = "C:\\Users\\CLIN01\\Documents\\Codes\\cloning.txt";
		
		//String sourceURL = "http://localhost:8080/blazegraph/namespace/kb/sparql"; 
		//String targetURL = "http://localhost:8080/blazegraph/namespace/kb2/sparql";
		String sourceURL = "http://localhost:8080/blazegraph/namespace/ontokin/sparql"; 
		String targetURL = "http://localhost:8080/blazegraph/namespace/ontokin2/sparql";
		
		NewCloningTool tool = new NewCloningTool();
		tool.setLimit(1000000);
		tool.doubleClone(sourceURL, targetURL);
		
		tool.writeTimesToFile(path);
	}
	
	@Test
	void testCloneOverlap() throws IOException {
		
		String path = "C:\\Users\\CLIN01\\Documents\\Codes\\cloning_overlap_blank.txt";
		
		//String sourceURL = "http://localhost:8080/blazegraph/namespace/kb/sparql"; 
		//String targetURL = "http://localhost:8080/blazegraph/namespace/kb2/sparql";
		String sourceURL = "http://localhost:8080/blazegraph/namespace/ontokin/sparql"; 
		String targetURL = "http://localhost:8080/blazegraph/namespace/ontokin2/sparql";
		
		NewCloningTool tool = new NewCloningTool();
		
		tool.setLimitOverlap(500000, 10000);
		tool.cloneOverlap(sourceURL, targetURL);
		
		tool.writeTimesToFile(path);
	}
	
	@Test
	void testCloneOverlapFuseki() throws IOException {
		
		String path = "C:\\Users\\CLIN01\\Documents\\Codes\\cloning_overlap_fuseki.txt";
		
		//String sourceURL = "http://localhost:8080/blazegraph/namespace/kb/sparql"; 
		//String targetURL = "http://localhost:8080/blazegraph/namespace/kb2/sparql";
		String sourceURL = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		String targetURLQuery = "http://localhost:8081/fuseki/ontokin2/query";
		String targetURLUpdate = "http://localhost:8081/fuseki/ontokin2/update";
		
		NewCloningTool tool = new NewCloningTool();
		
		tool.setLimitOverlap(500000, 10000);
		tool.cloneOverlap(sourceURL, targetURLQuery, targetURLUpdate);
		
		tool.writeTimesToFile(path);
	}
	
	@Test
	void testOldCloningTool() throws IOException {
		
		String path = "C:\\Users\\CLIN01\\Documents\\Codes\\cloning_oldtool.txt";
		
		String sourceURL = "http://localhost:8080/blazegraph/namespace/ontokin/sparql"; 
		String targetURL = "http://localhost:8080/blazegraph/namespace/ontokin2/sparql";
		
		RemoteStoreClient source = new RemoteStoreClient(sourceURL,sourceURL);
		RemoteStoreClient target = new RemoteStoreClient(targetURL,targetURL);
		
		CloningTool cloningTool = new CloningTool(500000);
		cloningTool.setTripleStore();
		cloningTool.clone(source, target);
		
		cloningTool.writeTimesToFile(path);
	}
}
