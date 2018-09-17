package uk.ac.cam.cares.jps.composition.test;

import org.json.JSONStringer;
import org.junit.Test;

public class TestComposition {

	@Test
	public void test() throws Exception {
//		Service compositeAgent = null;
//		String fileDirectory =  AgentLocator.getCurrentJpsAppDirectory(this) + "/testres/serviceowlfiles";
//		ServiceCompositionEngine engine = new ServiceCompositionEngine(compositeAgent, null, fileDirectory);
//		engine.start();
//		Graph graph = engine.getGraph();
//		
//		ArrayList<Service> serviceEliminationList = OptimalPathSearcher.getAllServicesToBeDeleted(graph);
//		ArrayList<String> eliminationList = new ArrayList<String>();
//		for (Service service : serviceEliminationList) {
//			eliminationList.add(service.getUri().toASCIIString());
//		}
//		
//		JSONObject graphInJSON = FormatTranslator.convertGraphJavaClassTOJSON(engine.getGraph());
//		
//		ExecutorProcessor processor = new ExecutorProcessor(graphInJSON, eliminationList);
//		ArrayList<ExecutionLayer> executionChain = processor.generateExecutionChain();
//		Executor executor = new Executor(executionChain);
		
		
		String json = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object()
						.key("lowerx").value("10.00")
						.key("lowery").value("11.00").endObject()
					.key("uppercorner").object()
						.key("upperx").value("20.00")
						.key("uppery").value("21.00").endObject()
					.key("srsname").value("ESPG:xxxx")
				.endObject()
				.endObject().toString(); 
		
		System.out.println(json);
		
		
		//String result = executor.execute(json);


	}

}
