package uk.ac.cam.cares.jps.composition.test;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.junit.After;
import org.junit.Test;

import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class TestNestedMessageParts {

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws Exception {
		ServiceReader serviceReader = new ServiceReader();
		// Load the two owl files and convert it to 
		String CityToWeatherInString = readTheServicePool("Service_CityToWeather.owl");
		String WeatherToDirectionInString = readTheServicePool("Service_WeatherToWindDirection.owl");
		
		Service cityToWeatherAgent = serviceReader.parse(new ByteArrayInputStream(CityToWeatherInString.getBytes(Charset.forName("UTF-8"))), "http://www.theworldavatar.com").get(0);
		List<MessagePart> inputs = cityToWeatherAgent.getAllInputs();
		for(MessagePart part: inputs) {
			System.out.println(part.getName());
			System.out.println(part.getType());
		}
		
		List<MessagePart> outputs = cityToWeatherAgent.getAllOutputs();
		for(MessagePart part: outputs) {
			System.out.println(part.getName());
			System.out.println(part.getType());
		}
		
		
		
		
	}

	
	public static String readTheServicePool(String filename) throws Exception {

 		String directory = "C:\\Users\\nasac\\Documents\\GIT\\JPS_COMPOSITION\\testres\\serviceowlfiles";
 		String wholeContent = "";

		try (BufferedReader br = new BufferedReader(new FileReader(directory + "\\" + filename))) {
			String sCurrentLine;
			while ((sCurrentLine = br.readLine()) != null) {
				wholeContent = wholeContent + sCurrentLine;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (wholeContent == null || wholeContent == "") {
			return null;
		} else {
			return wholeContent;
		}
	}
	
	
	
	public void whenWriteStringUsingBufferedWritter_thenCorrect(String filename) 
			  throws IOException {
			    String str = "Hello";
			    BufferedWriter writer = new BufferedWriter(new FileWriter(filename));
			    writer.write(str);
			     
			    writer.close();
			}
	
}
