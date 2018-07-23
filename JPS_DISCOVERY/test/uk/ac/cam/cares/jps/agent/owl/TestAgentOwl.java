package uk.ac.cam.cares.jps.agent.owl;

import java.io.FileNotFoundException;
import java.net.URISyntaxException;
import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Operation;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class TestAgentOwl extends TestCase {
	
	private String getPathToJpsKbAgentDir() {
		return "C:\\JPS_DATA\\kb\\agent";
	}

	public void testServiceBuilder() {
		
		Service service = new ServiceBuilder()
			.operation(null, "http://www.theworldavatar.com/test1")
			.input("inputrefuri1", true)
			.input("inputrefuri2", false)
			.output("outputrefuri3", true)
			.output("outputrefuri4", false)
			.operation(null, "http://www.theworldavatar.com/test2")
			.input("inputrefuri5", true)
			.build();
		
		Operation op1 = service.getOperations().get(0);
		assertEquals("http://www.theworldavatar.com/test1", op1.getHttpUrl());
		assertEquals("inputrefuri1", op1.getInputs().get(0).getMandatoryParts().get(0).getModelReference().toASCIIString());
		assertEquals("inputrefuri2", op1.getInputs().get(0).getOptionalParts().get(0).getModelReference().toASCIIString());
		assertEquals("outputrefuri3", op1.getOutputs().get(0).getMandatoryParts().get(0).getModelReference().toASCIIString());
		assertEquals("outputrefuri4", op1.getOutputs().get(0).getOptionalParts().get(0).getModelReference().toASCIIString());
		
		Operation op2 = service.getOperations().get(1);
		assertEquals("http://www.theworldavatar.com/test2", op2.getHttpUrl());
		assertEquals("inputrefuri5", op2.getInputs().get(0).getMandatoryParts().get(0).getModelReference().toString());
	}	
	
	public void testOWLSerializationOneOperationTwoParameters() throws FileNotFoundException, URISyntaxException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/test1")
				.input("inputrefuri1", true)
				.output("outputrefuri2", false)
				.build();
		
		String owlService = new ServiceWriter().generateModel(service);
		
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
		assertEquals("inputrefuri1", part.getModelReference().toASCIIString());
		
		// assert output parameters
		assertEquals(1, op.getOutputs().size());
		assertEquals(0, op.getOutputs().get(0).getMandatoryParts().size());
		assertEquals(1, op.getOutputs().get(0).getOptionalParts().size());
		part = op.getOutputs().get(0).getOptionalParts().get(0);
		assertEquals("outputrefuri2", part.getModelReference().toASCIIString());	
	}
	
	public void testOWLSerializationTwoOperationsSevenParameters() throws FileNotFoundException, URISyntaxException {
		
		Service service = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/test1")
				.output("outputrefuri1", true)
				.input("inputrefuri2", true)
				.output("outputrefuri3", true)
				.input("inputrefuri4", false)
				.input("inputrefuri5", true)
				.operation(null, "http://www.theworldavatar.com/test2")
				.input("inputrefuri6", true)
				.output("outputrefuri7", true)
				.build();
		
		String owlService = new ServiceWriter().generateModel(service);
		
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
		assertEquals("inputrefuri2", part.getModelReference().toASCIIString());
		part = op.getInputs().get(0).getMandatoryParts().get(1);
		assertEquals("inputrefuri5", part.getModelReference().toASCIIString());
		part = op.getInputs().get(0).getOptionalParts().get(0);
		assertEquals("inputrefuri4", part.getModelReference().toASCIIString());

		// assert output parameters
		assertEquals(1, op.getOutputs().size());
		assertEquals(2, op.getOutputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getOutputs().get(0).getOptionalParts().size());
		part = op.getOutputs().get(0).getMandatoryParts().get(0);
		assertEquals("outputrefuri1", part.getModelReference().toASCIIString());
		part = op.getOutputs().get(0).getMandatoryParts().get(1);
		assertEquals("outputrefuri3", part.getModelReference().toASCIIString());
		
		// assert second operation
		op = services.get(0).getOperations().get(1);
		assertEquals("http://www.theworldavatar.com/test2", op.getHttpUrl());
		
		// assert input parameters
		assertEquals(1, op.getInputs().size());
		assertEquals(1, op.getInputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getInputs().get(0).getOptionalParts().size());
		part = op.getInputs().get(0).getMandatoryParts().get(0);
		assertEquals("inputrefuri6", part.getModelReference().toASCIIString());
		
		// assert output parameters
		assertEquals(1, op.getOutputs().size());
		assertEquals(1, op.getOutputs().get(0).getMandatoryParts().size());
		assertEquals(0, op.getOutputs().get(0).getOptionalParts().size());
		part = op.getOutputs().get(0).getMandatoryParts().get(0);
		assertEquals("outputrefuri7", part.getModelReference().toASCIIString());
	}
}
