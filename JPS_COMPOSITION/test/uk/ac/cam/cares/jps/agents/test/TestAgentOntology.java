package uk.ac.cam.cares.jps.agents.test;

import java.io.FileNotFoundException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.agents.ontology.ServiceBuilder;
import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.agents.ontology.ServiceWriter;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Operation;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class TestAgentOntology extends TestCase {
	
	private List<MessagePart> createMessageParts(String... types) throws URISyntaxException {
		List<MessagePart> result = new ArrayList<MessagePart>();
		
		for (String current : types) {
			MessagePart part = new MessagePart();
			part.setType(new URI(current));
			result.add(part);
		}
		
		return result;
	}

	public void testServiceBuilder() {
		
		Service service = new ServiceBuilder()
			.operation(null, "http://www.theworldavatar.com/test1")
			.input("inputrefuri1", false, "inputname1", true)
			.input("inputrefuri2", false, "inputname2", false)
			.output("outputrefuri3", false, "outputname3", true)
			.output("outputrefuri4", false, "outputname4", false)
			.operation(null, "http://www.theworldavatar.com/test2")
			.input("inputrefuri5", false, "inputname5", true)
			.build();
		
		Operation op1 = service.getOperations().get(0);
		assertEquals("http://www.theworldavatar.com/test1", op1.getHttpUrl());
		MessagePart part = op1.getInputs().get(0).getMandatoryParts().get(0);
		assertEquals("inputrefuri1", part.getType().toASCIIString());
		assertEquals("inputname1", part.getName());
		part = op1.getInputs().get(0).getOptionalParts().get(0);
		assertEquals("inputrefuri2", part.getType().toASCIIString());
		assertEquals("inputname2", part.getName());
		part = op1.getOutputs().get(0).getMandatoryParts().get(0);
		assertEquals("outputrefuri3", part.getType().toASCIIString());
		assertEquals("outputname3", part.getName());
		part = op1.getOutputs().get(0).getOptionalParts().get(0);
		assertEquals("outputrefuri4", part.getType().toASCIIString());
		assertEquals("outputname4", part.getName());
		
		Operation op2 = service.getOperations().get(1);
		assertEquals("http://www.theworldavatar.com/test2", op2.getHttpUrl());
		part = op2.getInputs().get(0).getMandatoryParts().get(0);
		assertEquals("inputrefuri5", part.getType().toString());
		assertEquals("inputname5", part.getName());
	}	
	
	public void testOWLSerializationOneOperationTwoMandatoryParameters() throws FileNotFoundException, URISyntaxException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/test1")
				.input("inputrefuri1", "inputname1")
				.output("outputrefuri2", "outputname2")
				.build();
		
		String owlService = new ServiceWriter().generateSerializedModel(service, "Test");
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		List<Service> services = new ServiceReader().parse(owlService, null);
		assertEquals(1, services.size());
		assertEquals(1, services.get(0).getOperations().size());

		Operation op = services.get(0).getOperations().get(0);
		assertEquals("http://www.theworldavatar.com/test1", op.getHttpUrl());
		
		// assert input parameters
		assertEquals(1, op.getInputs().size());
		assertEquals(1, op.getInputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getInputs().get(0).getOptionalParts().size());
		MessagePart part = op.getInputs().get(0).getMandatoryParts().get(0);
		assertEquals("inputrefuri1", part.getType().toASCIIString());
		assertEquals(false, part.isArray());
		assertEquals("inputname1", part.getName());
		
		// assert output parameters
		assertEquals(1, op.getOutputs().size());
		assertEquals(1, op.getOutputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getOutputs().get(0).getOptionalParts().size());
		part = op.getOutputs().get(0).getMandatoryParts().get(0);
		assertEquals("outputrefuri2", part.getType().toASCIIString());
		assertEquals(false, part.isArray());
		assertEquals("outputname2", part.getName());
	}
	
	public void testOWLSerializationTwoOperationsSevenParameters() throws FileNotFoundException, URISyntaxException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/test1")
				.output("outputrefuri1", false, "outputname1", true)
				.input("inputrefuri2", false, "inputname2", true)
				.output("outputrefuri3", false, "outputname3", true)
				.input("inputrefuri4", false, "inputname4", false)
				.input("inputrefuri5", false, "inputname5", true)
				.operation(null, "http://www.theworldavatar.com/test2")
				.input("inputrefuri6", false, "inputname6", true)
				.output("outputrefuri7", false, "outputname7", true)
				.build();
		
		String owlService = new ServiceWriter().generateSerializedModel(service, "Test");
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		List<Service> services = new ServiceReader().parse(owlService, null);
		assertEquals(1, services.size());
		assertEquals(2, services.get(0).getOperations().size());

		// assert first operation
		Operation op = services.get(0).getOperations().get(0);
		assertEquals("http://www.theworldavatar.com/test1", op.getHttpUrl());
		
		// assert input parameters
		assertEquals(1, op.getInputs().size());
		assertEquals(2, op.getInputs().get(0).getMandatoryParts().size());
		assertEquals(1, op.getInputs().get(0).getOptionalParts().size());
		MessagePart part = op.getInputs().get(0).getMandatoryParts().get(0);
		assertEquals("inputrefuri2", part.getType().toASCIIString());
		part = op.getInputs().get(0).getMandatoryParts().get(1);
		assertEquals("inputrefuri5", part.getType().toASCIIString());
		part = op.getInputs().get(0).getOptionalParts().get(0);
		assertEquals("inputrefuri4", part.getType().toASCIIString());

		// assert output parameters
		assertEquals(1, op.getOutputs().size());
		assertEquals(2, op.getOutputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getOutputs().get(0).getOptionalParts().size());
		part = op.getOutputs().get(0).getMandatoryParts().get(0);
		assertEquals("outputrefuri1", part.getType().toASCIIString());
		part = op.getOutputs().get(0).getMandatoryParts().get(1);
		assertEquals("outputrefuri3", part.getType().toASCIIString());
		
		// assert second operation
		op = services.get(0).getOperations().get(1);
		assertEquals("http://www.theworldavatar.com/test2", op.getHttpUrl());
		
		// assert input parameters
		assertEquals(1, op.getInputs().size());
		assertEquals(1, op.getInputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getInputs().get(0).getOptionalParts().size());
		part = op.getInputs().get(0).getMandatoryParts().get(0);
		assertEquals("inputrefuri6", part.getType().toASCIIString());
		
		// assert output parameters
		assertEquals(1, op.getOutputs().size());
		assertEquals(1, op.getOutputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getOutputs().get(0).getOptionalParts().size());
		part = op.getOutputs().get(0).getMandatoryParts().get(0);
		assertEquals("outputrefuri7", part.getType().toASCIIString());
	}
	
	public void testOWLSerializationWithArrayParameters() throws FileNotFoundException, URISyntaxException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/test1")
				.output("outputrefuri1", true, "outputname1", true)
				.input("inputrefuri2", false, "inputname2", true)
				.input("inputrefuri3", true, "inputname3", true)
				.output("outputrefuri4", false, "outputname4", true)
				.build();
		
		String owlService = new ServiceWriter().generateSerializedModel(service, null);
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		List<Service> services = new ServiceReader().parse(owlService, null);
		
		Operation op = services.get(0).getOperations().get(0);
		List<MessagePart> parts = op.getInputs().get(0).getMandatoryParts();
		assertEquals(false, parts.get(0).isArray());
		assertEquals(true, parts.get(1).isArray());
		parts = op.getOutputs().get(0).getMandatoryParts();
		assertEquals(true, parts.get(0).isArray());
		assertEquals(false, parts.get(1).isArray());
	}
	
	public void testOWLSerializationWithValues() throws FileNotFoundException, URISyntaxException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/test1")
				.output("outputrefuri1", true, "outputname1", true)
				.input("inputrefuri2", false, "inputname2", true)
				.input("inputrefuri3", true, "inputname3", true)
				.output("outputrefuri4", false, "outputname4", true)
				.build();
		
		String owlService = new ServiceWriter().generateSerializedModel(service, "Test");
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		List<Service> services = new ServiceReader().parse(owlService, null);
		
		Operation op = services.get(0).getOperations().get(0);
		List<MessagePart> parts = op.getInputs().get(0).getMandatoryParts();
		assertEquals(false, parts.get(0).isArray());
		assertEquals(true, parts.get(1).isArray());
		parts = op.getOutputs().get(0).getMandatoryParts();
		assertEquals(true, parts.get(0).isArray());
		assertEquals(false, parts.get(1).isArray());
	}
	
	public void testOwlSerializationWithNestedInputParameter() throws FileNotFoundException, URISyntaxException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/test1")
				.input("inputrefuri1", "region")
				.down().input("inputrefuri2", "lowercorner")
					   .down().input("inputrefuri3", "lowerx")
					   		  .input("inputrefuri4", "lowery").up()
					   .input("inputrefuri5", "uppercorner")
					   .down().input("inputrefuri6", "upperx")
				   		      .input("inputrefuri7", "uppery").up()
					   .input("inputrefuri8", "srsname").up()
			    .input("inputrefuri9", "foo")
				.output("outputrefuri1", "city")
				.build();
		
		String owlService = new ServiceWriter().generateSerializedModel(service, "Test");
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		List<Service> services = new ServiceReader().parse(owlService, null);
		Operation op = services.get(0).getOperations().get(0);
		
		// assert input parameters
		assertEquals(1, op.getInputs().size());
		assertEquals(2, op.getInputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getInputs().get(0).getOptionalParts().size());
		MessagePart part = op.getInputs().get(0).getMandatoryParts().get(0);
		assertEquals("inputrefuri1", part.getType().toASCIIString());
		assertEquals("region", part.getName());
		assertEquals(3, part.getMandatoryParts().size());
		MessagePart partDown = part.getMandatoryParts().get(0);
		assertEquals("inputrefuri2", partDown.getType().toASCIIString());
		assertEquals("lowercorner", partDown.getName());
		assertEquals(2, partDown.getMandatoryParts().size());
		MessagePart partDownDown = partDown.getMandatoryParts().get(0);
		assertEquals("inputrefuri3", partDownDown.getType().toASCIIString());
		assertEquals("lowerx", partDownDown.getName());
		partDownDown = partDown.getMandatoryParts().get(1);
		assertEquals("inputrefuri4", partDownDown.getType().toASCIIString());
		assertEquals("lowery", partDownDown.getName());
		partDown = part.getMandatoryParts().get(1);
		assertEquals("inputrefuri5", partDown.getType().toASCIIString());
		assertEquals("uppercorner", partDown.getName());
		assertEquals(2, partDown.getMandatoryParts().size());
		partDownDown = partDown.getMandatoryParts().get(0);
		assertEquals("inputrefuri6", partDownDown.getType().toASCIIString());
		assertEquals("upperx", partDownDown.getName());
		partDownDown = partDown.getMandatoryParts().get(1);
		assertEquals("inputrefuri7", partDownDown.getType().toASCIIString());
		assertEquals("uppery", partDownDown.getName());
		partDown = part.getMandatoryParts().get(2);
		assertEquals("inputrefuri8", partDown.getType().toASCIIString());
		assertEquals("srsname", partDown.getName());
		assertEquals(0, partDown.getMandatoryParts().size());
		part = op.getInputs().get(0).getMandatoryParts().get(1);
		assertEquals("inputrefuri9", part.getType().toASCIIString());
		assertEquals("foo", part.getName());
		
		// assert output parameters
		assertEquals(1, op.getOutputs().size());
		assertEquals(1, op.getOutputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getOutputs().get(0).getOptionalParts().size());
		part = op.getOutputs().get(0).getMandatoryParts().get(0);
		assertEquals("outputrefuri1", part.getType().toASCIIString());
		assertEquals("city", part.getName());
	}
	
	public void testOwlSerializationComposedService() throws URISyntaxException {
		Service service = new ServiceBuilder()
				.composed()
				.operation(null, null)
				.input("inputrefuri1", "inputname1")
				.output("outputrefuri2", "outputname2")
				.build();
		
		
		String owlService = new ServiceWriter().generateSerializedModel(service, "Test");
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		List<Service> services = new ServiceReader().parse(owlService, null);
		Operation op = services.get(0).getOperations().get(0);
		
		assertTrue(services.get(0).composed);
		assertEquals(1, op.getInputs().get(0).getMandatoryParts().size());
		assertEquals(1, op.getOutputs().get(0).getMandatoryParts().size());
	}
	
	public void testServiceDiscoveryByType() throws Exception {
		
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		//KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/serviceowlfiles");
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
		
		ServiceDiscovery discovery = ServiceDiscovery.getInstance();
		//List<MessagePart> inputs = createMessageParts("op2inputrefuri1");
		List<MessagePart> inputs = createMessageParts("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant");
		List<Service> result = discovery.getAllServiceCandidates(inputs, null);
				
		assertEquals(1, result.size());
	}
	
	public void testServiceDiscoveryByUri() throws Exception {
		
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
		
		ServiceDiscovery discovery = ServiceDiscovery.getInstance();
		Service result = discovery.getServiceByUri("http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		
		assertTrue(result.isComposed());
	}
		
	public void testOWLSerializationWriteToFile() throws FileNotFoundException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/op2")
				.input("op2inputrefuri1", "op1inputname1")
				.output("op2outputrefuri2", "op1outputname2")
				.build();
		
		//new ServiceWriter().writeAsOwlFile(service, "Op2", "C:\\Users\\Andreas\\TMP\\newAgentsMSM");
	}
	
	public void testJSONSerialization() {
		
		Service compositeAgent = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/Composite_Service_ODsMpRv")
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
				.output("https://www.w3.org/ns/csvw#Table", "dispersiongrid")
				.output("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", true, "buildings", true)
				.build();
		
		
		MessagePart region = compositeAgent.getOperations().get(0).getInputs().get(0).getMandatoryParts().get(0);
		System.out.println(region.getName());
		System.out.println(region.getType());
		
		String json = new Gson().toJson(compositeAgent);
		System.out.println(json);
		
		
		MessagePart part = new MessagePart();
		part.setName("hello");
		json = new Gson().toJson(part);
		System.out.println(json);
	}
}
