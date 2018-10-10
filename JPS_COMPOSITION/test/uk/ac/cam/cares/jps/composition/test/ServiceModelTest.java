package uk.ac.cam.cares.jps.composition.test;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.junit.Test;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.composition.servicemodel.MessageContent;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Operation;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class ServiceModelTest {

	public Service testService;

	@Test
	public void test() throws URISyntaxException, IOException {
		// Use Jackson library to serialize the Java object messageContent to JSON
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);

		// =======================================================================================
		// Here we declare the two input parameters
		MessagePart messagePartCity = new MessagePart(new URI("http://www.theworldvatar.com/wInParamCityXYZ123"));
		messagePartCity.setType(new URI("http://www.theworldavatar.com/CityGML.owl#City"));
		messagePartCity.setValue(new URI("http://dbpedia.org/resource/Singapore"));
		messagePartCity.setDatatypeValue("xsd:anyURI");
		// We put value and value data type in the class MessagePart...

		MessagePart messagePartDate = new MessagePart(new URI("http://www.theworldvatar.com/wInParamDateXYZ123"));
		messagePartDate.setType(new URI("http://www.theworldavatar.com/Date.owl#Date"));
		// =======================================================================================

		String MessagePartInJSON = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(messagePartDate);
		System.out.println("\n================================================\n");
		System.out.println("MessagePartInJSON: ");
		System.out.println(MessagePartInJSON);

		mapper.readValue(MessagePartInJSON, MessagePart.class);
		System.out.println("MessagePartConvertedBack");

		MessageContent messageContent_input = new MessageContent(
				new URI("http://www.theworldavatar.com/weatherInXYZ123"));
		messageContent_input.addMandatoryPart(messagePartCity);
		messageContent_input.addOptionalPart(messagePartDate);

		MessagePart messagePartTemperature = new MessagePart(
				new URI("http://www.theworldavatar.com/wOutParamTempXYX123"));
		messagePartTemperature.setType(new URI("http://www.theworldavatar.com/Weather.owl#Temperature"));
		MessagePart messagePartWinddirection = new MessagePart(
				new URI("http://www.theworldavatar.com/wOutParamWindXYX123"));
		messagePartWinddirection.setType(new URI("http://www.theworldavatar.com/Weather.owl#Winddirection"));

		URI uri_messageContent_output = new URI("http://www.theworldavatar.com/weatherOutXYZ123");
		MessageContent messageContent_output = new MessageContent(uri_messageContent_output);
		messageContent_output.addMandatoryPart(messagePartTemperature);
		messageContent_output.addMandatoryPart(messagePartWinddirection);

		String MessageContentInJSON = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(messageContent_output);
		System.out.println("\n================================================\n");
		System.out.println("MessageContentInJSON: ");
		System.out.println(MessageContentInJSON);
		mapper.readValue(MessageContentInJSON, MessageContent.class);

		URI uri_operation = new URI("http://www.theworldavatar.com/someRandomOperation");
		Operation operation = new Operation(uri_operation, "http://www.theworldavatar.com/ADMS/someRandomOperation");
		operation.addInput(messageContent_input);
		operation.addOutput(messageContent_output);

		String OperationInJSON = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(operation);
		System.out.println("\n================================================\n");
		System.out.println("OperationInJSON: ");
		System.out.println(OperationInJSON);
		Operation OperationConvertedBack = mapper.readValue(OperationInJSON, Operation.class);
		System.out.println(OperationConvertedBack.getHttpUrl());

		Service service = new Service(new URI("http://www.theworldavatar.com/weatherServiceXYZ123"));
		service.addOperation(operation);

		new Service(new URI("http://www.theworldavatar.com/ADMS/anthorRandomService001"));
		Operation operation2 = new Operation(new URI("http://www.theworldavatar.com/ADMS/anotherRandomOperation001"),
				"http://www.theworldavatar.com/ADMS/anotherRandomOperationHTTPURL");
		MessageContent inputOfOperation2 = new MessageContent(
				new URI("http://www.theworldavatar.com/inputOfanotherRandomOperation001"));
		MessagePart param1OfInputOfOperation2 = new MessagePart(
				new URI("http://www.theworldavatar.com/param1OfInputOfOperation2"));
		inputOfOperation2.addMandatoryPart(param1OfInputOfOperation2);
		operation2.addInput(inputOfOperation2);
		service.addOperation(operation2);

		String ServiceInJSON = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(service);
		System.out.println("\n================================================\n");
		System.out.println("ServiceInJSON: ");
		System.out.println(ServiceInJSON);
		Service ServiceConvertedBack = mapper.readValue(ServiceInJSON, Service.class);
		System.out.println("\n================================================\n");
		System.out.println("All Outputs of Service Converted Back: ");
		System.out.println(ServiceConvertedBack.getAllOutputs());

	}

}
