package uk.ac.cam.cares.jps.servicespool.test;

import java.net.URI;
import java.util.ArrayList;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.junit.After;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.servicespool.ADMSAgent;

public class TestSPARQLQuery extends TestCase {

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() {
		String buildingsInRegionQuery = "PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
				"PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
				"PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\r\n" + 
				"SELECT distinct ?bdn ?x ?y\r\n" + 
				"WHERE {\r\n" + 
				"?bdn a citygml:BuildingType .\r\n" + 
				"?bdn space_and_time_extended:hasGISCoordinateSystem ?coordinates .\r\n" + 
				"?coordinates space_and_time_extended:hasProjectedCoordinate_x ?xe .\r\n" + 
				"?xe sys:hasValue ?xv .\r\n" + 
				"?xv sys:numericalValue ?x .\r\n" + 
				"?coordinates space_and_time_extended:hasProjectedCoordinate_y ?ye .\r\n" + 
				"?ye sys:hasValue ?yv .\r\n" + 
				"?yv sys:numericalValue ?y .\r\n" + 
				"Filter(xsd:double(?x) > \"79343.447365\"^^xsd:double && xsd:double(?y) > \"454239.320059\"^^xsd:double && xsd:double(?x) < \"80105.109265\"^^xsd:double && xsd:double(?y) < \"455181.701459\"^^xsd:double) \r\n" + 
				"}\r\n" + 
				"LIMIT 200";
		
		
		String myHost = "www.theworldavatar.com" ;
		int myPort = 80;
		String myPath = "/damecoolquestion/thehaguebuildings/query";
		URIBuilder builder;
		
//		builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
//				.setPath(myPath)
//				.setParameter("query", buildingsInRegionQuery );
//				//.setParameter("output", "json");
//		String result = executeGet(builder);	
//		System.out.println(result);
//		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
//		System.out.println(map);
//		ArrayList<String> buildingsList = BuildingQueryPerformer.selectClosestBuilding(79831.0, 454766.0, 25, map);
//		System.out.println(buildingsList.size());
		
		ArrayList<String> testArray = new ArrayList<String>();
		for(int i = 0 ; i < 25; i++) {
			String test = "test" + i;
			testArray.add(test);
		}
		System.out.println(testArray);
	
	}
	
	public void testparallelADMS() {
       
		String startADMSCommand = "\"C:\\\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"./ADMS/test.apl\"";
		String targetFolder = AgentLocator.getPathToJpsWorkingDir() + "/JPS";
//		
//		String startADMSCommand = "\"C:\\\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
//         String targetFolder = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\www_theworldavatar_com\\cde2d26c-842e-44d6-9dd2-bfc7855c297e\\JPS_ADMS";
        //new ADMSAgent().startADMS(targetFolder);
        CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
		
		
	}
	
	
	public void testStartADMSParallel() { //max 4
		
		String[] uuids = {"parallel_1", "parallel_2", "parallel_3", "parallel_4"};
		
		for (String uuid : uuids) {
		
			Thread t1 = new Thread(new Runnable() {
				public void run() {
		         
					System.out.println("starting ADMS for uuid = " + uuid);
					try {
						startADMS(uuid);
						System.out.println("finished ADMS for uuid = " + uuid);
					} catch (Exception e) {
						e.printStackTrace();
					}
					
		    }});  
			t1.start();
			//t1.run();
			
			
			System.out.println("waiting");
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
	}
	
	
	public void startADMS(String uuid) {
	       
		String startADMSCommand = "\"C:\\\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"./" + uuid + "/JPS_ADMS/test.apl\"";
        String targetFolder = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\www_theworldavatar_com\\";
        //new ADMSAgent().startADMS(targetFolder);
        System.out.println(startADMSCommand);
        CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
	}
	
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
}
