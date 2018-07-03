package uk.ac.cam.cares.jps.composition.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONException;
import org.junit.Test;

import uk.ac.cam.cares.jps.composition.WebServer.ServiceDiscovery;
import uk.ac.cam.cares.jps.composition.ServiceModel.*;

public class ServiceDiscoveryTest {

	@Test
	public void test() throws JSONException, IOException, URISyntaxException {
		ServiceDiscovery serviceDiscovery = new ServiceDiscovery();
		ArrayList<MessagePart> inputs = new ArrayList<MessagePart>();
		
		MessagePart messagePartPlant = new MessagePart(new URI("http://www.theworldvatar.com/wInParamCityXYZ123"));
		messagePartPlant.setModelReference(new URI("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant"));
		inputs.add(messagePartPlant);

		MessagePart messagePartCoor = new MessagePart(new URI("http://www.theworldvatar.com/wInParamCoorXYZ123"));
		messagePartCoor.setModelReference(new URI("http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate"));
		inputs.add(messagePartCoor);
		
		//ArrayList<Service> services = serviceDiscovery.getAllServiceCandidates(0,inputs);
		//System.out.println(services);
		//for (Service service : services) {
		//	System.out.println(service.getUri());
		//}
		
		
	}
}
