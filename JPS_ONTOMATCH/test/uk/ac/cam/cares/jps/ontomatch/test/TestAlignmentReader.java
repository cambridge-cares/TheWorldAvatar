package uk.ac.cam.cares.jps.ontomatch.test;

import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.ontomatch.AlignmentReader;

public class TestAlignmentReader extends Mockito {
	/** an workaround to test the protected HTTP method */
	@Test
	public void testAlignmentReader() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		// TODO:later make this suitable for another set up
		String alignmentFileIRI = "D:\\workwork\\ontoMatchFiles\\finalTestAggregaotr.owl";
		JSONObject jo = new JSONObject();
		jo.put("alignmentIRI", alignmentFileIRI);
		jo.put("threshold", 0.0);
		AlignmentReader ar = new AlignmentReader();
		Reader inputString = new StringReader(jo.toString());
		BufferedReader reader = new BufferedReader(inputString);

		when(request.getMethod()).thenReturn("POST");
		when(request.getReader()).thenReturn(reader);
		JSONObject result = ar.processRequestParameters(jo, request);
		System.out.println(result.toString());
		assertTrue(result.has("alignmentlist"));
	}

}
