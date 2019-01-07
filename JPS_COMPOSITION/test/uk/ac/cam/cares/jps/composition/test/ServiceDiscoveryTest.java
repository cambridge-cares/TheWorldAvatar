package uk.ac.cam.cares.jps.composition.test;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONException;
import org.junit.Test;

import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;

public class ServiceDiscoveryTest {

	@Test
	public void test() throws JSONException, IOException, URISyntaxException {
		ArrayList<MessagePart> inputs = new ArrayList<MessagePart>();

		MessagePart messagePartPlant = new MessagePart(new URI("http://www.theworldvatar.com/wInParamCityXYZ123"));
		messagePartPlant.setType(new URI(
				"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant"));
		inputs.add(messagePartPlant);

		MessagePart messagePartCoor = new MessagePart(new URI("http://www.theworldvatar.com/wInParamCoorXYZ123"));
		messagePartCoor.setType(new URI(
				"http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate"));
		inputs.add(messagePartCoor);
 
	}
}
