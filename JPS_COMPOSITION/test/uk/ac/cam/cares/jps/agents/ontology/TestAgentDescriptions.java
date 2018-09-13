package uk.ac.cam.cares.jps.agents.ontology;

import java.io.FileNotFoundException;
import java.net.URISyntaxException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class TestAgentDescriptions extends TestCase {

	private static final String JPS = "http://www.theworldavatar.com/JPS";
	
	private ServiceBuilder addInputRegion(ServiceBuilder builder) {
		return builder.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region").down()
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lowerCornerPoint", "lowercornerpoint").down()
					.input("https://www.w3.org/2001/XMLSchema#double", "lowerx")
					.input("https://www.w3.org/2001/XMLSchema#double", "lowery").up()
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#upperCornerPoint", "uppercornerpoint").down()
					.input("https://www.w3.org/2001/XMLSchema#double", "upperx")
					.input("https://www.w3.org/2001/XMLSchema#double", "uppery").up()
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#srsname", "srsname").up();
	}
	
	public void testcreateDescription() throws URISyntaxException, FileNotFoundException {
		
		Service service = createDescrForAgentRegionToCity();
		backAndforthAndWrite(service, "RegionToCity");
		service = createDescrForAgentGetPlantsInRegion();
		backAndforthAndWrite(service, "GetPlantsInRegion");
	}
	
	private void backAndforthAndWrite(Service service, String name) throws URISyntaxException, FileNotFoundException {
		
		new ServiceWriter().writeAsOwlFile(service, name, "C:\\Users\\Andreas\\TMP\\newAgentsMSM");
		
		service.setUri(null);
		String owlService = new ServiceWriter().generateSerializedModel(service, name);
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		new ServiceReader().parse(owlService, null).get(0);	
	}
	
	private Service createDescrForAgentRegionToCity() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/RegionToCity"))
			.output("http://dbpedia.org/ontology/city", "city")
			.build();
	}
	
	private Service createDescrForAgentGetPlantsInRegion() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/GetPlantsInRegion"))
			.output("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant", true, "plant", true)
			.build();
	}
}
