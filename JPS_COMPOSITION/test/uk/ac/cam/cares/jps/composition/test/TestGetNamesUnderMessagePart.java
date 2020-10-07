package uk.ac.cam.cares.jps.composition.test;

import java.io.ByteArrayInputStream;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

import org.junit.After;
import org.junit.Test;

import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class TestGetNamesUnderMessagePart {

	public String filename = "E:\\log\\log.txt";
	
	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws Exception {
		ServiceReader serviceReader = new ServiceReader();
		String CityToWeatherInString = TestNestedMessageParts.readTheServicePool("Service_CityToWeather.owl");
		Service cityToWeatherAgent = serviceReader.parse(new ByteArrayInputStream(CityToWeatherInString.getBytes(Charset.forName("UTF-8"))), "http://www.theworldavatar.com").get(0);
		String WeatherToDirectionInString = TestNestedMessageParts.readTheServicePool("Service_WeatherToWindDirection.owl");
		Service weatherToDirection = serviceReader.parse(new ByteArrayInputStream(WeatherToDirectionInString.getBytes(Charset.forName("UTF-8"))), "http://www.theworldavatar.com").get(0);
		
		List<MessagePart> outputs = cityToWeatherAgent.getAllOutputs();
		for(MessagePart part : outputs) {
			Map<String,String> namesToNamesMapping = part.getTypeNamesUnderThisMessagePart();
			System.out.println(namesToNamesMapping);
		}
		System.out.println("===========================================");
		outputs = weatherToDirection.getAllInputs();
		for(MessagePart part : outputs) {
			Map<String,String> namesToNamesMapping = part.getTypeNamesUnderThisMessagePart();
			System.out.println(namesToNamesMapping);
		}
		
		
	}

}
