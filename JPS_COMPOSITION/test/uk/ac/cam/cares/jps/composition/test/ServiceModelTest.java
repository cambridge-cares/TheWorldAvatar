package uk.ac.cam.cares.jps.composition.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.composition.ServiceModel.MessageContent;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Operation;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class ServiceModelTest {

	public Service testService;

	@Test
	public void test() throws URISyntaxException, IOException, JSONException {
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
		assertEquals(new JSONObject(MessagePartInJSON).getString("type"), "http://www.theworldavatar.com/Date.owl#Date");

		mapper.readValue(MessagePartInJSON, MessagePart.class);
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
		assertEquals(new JSONObject(MessageContentInJSON).getBoolean("array"),false);
		mapper.readValue(MessageContentInJSON, MessageContent.class);

		URI uri_operation = new URI("http://www.theworldavatar.com/someRandomOperation");
		Operation operation = new Operation(uri_operation, "http://www.theworldavatar.com/ADMS/someRandomOperation");
		operation.addInput(messageContent_input);
		operation.addOutput(messageContent_output);

		String OperationInJSON = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(operation);
		assertEquals(new JSONObject(OperationInJSON).getJSONArray("inputs").length(),1);
		Operation OperationConvertedBack = mapper.readValue(OperationInJSON, Operation.class);
		assertEquals(OperationConvertedBack.getHttpUrl(),"http://www.theworldavatar.com/ADMS/someRandomOperation");  


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
		assertEquals(new JSONObject(ServiceInJSON).getJSONArray("operations").length(),2);
		Service ServiceConvertedBack = mapper.readValue(ServiceInJSON, Service.class);
		assertEquals(ServiceConvertedBack.getAllOutputs().get(0).getType().toASCIIString(),
				"http://www.theworldavatar.com/Weather.owl#Temperature");

	}

}
