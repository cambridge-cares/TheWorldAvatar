package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.tools.CloningTool;
import uk.ac.cam.cares.jps.base.tools.NewCloningTool;


class NewCloningToolTest {

	@Test
	void testClone() throws IOException {
		
		String path = "C:\\Users\\CLIN01\\Documents\\Codes\\cloning_overlap_blank.txt";
		
		String sourceURL = "http://localhost:8080/blazegraph/namespace/ontokin/sparql"; 
		String targetURL = "http://localhost:8080/blazegraph/namespace/ontokin2/sparql";
		
		RemoteStoreClient source = new RemoteStoreClient(sourceURL,null);
		RemoteStoreClient target = new RemoteStoreClient(targetURL,targetURL);
		
		NewCloningTool tool = new NewCloningTool();
		
		tool.setLimitAndOverlap(500000, 10000);
		tool.clone(source, target);
		
		tool.writeTimesToFile(path);
	}
	
	@Test
	void testCloneFuseki() throws IOException {
		
		String path = "C:\\Users\\CLIN01\\Documents\\Codes\\cloning_overlap_fuseki.txt";
		
		String sourceURL = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		String targetURLQuery = "http://localhost:8081/fuseki/ontokin2/query";
		String targetURLUpdate = "http://localhost:8081/fuseki/ontokin2/update";
		
		RemoteStoreClient source = new RemoteStoreClient(sourceURL,null);
		RemoteStoreClient target = new RemoteStoreClient(targetURLQuery,targetURLUpdate);
		
		NewCloningTool tool = new NewCloningTool();
		
		tool.setLimitAndOverlap(500000, 10000);
		tool.clone(source, target);
		
		tool.writeTimesToFile(path);
	}
	
	
}
