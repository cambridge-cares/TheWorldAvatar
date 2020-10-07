package uk.ac.cam.cares.jps.composition.test;

import static org.junit.Assert.assertEquals;

import java.net.URLEncoder;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.composition.util.SendRequest;

public class RequestTest {

	@Test
	public void test() throws Exception {
		String result = SendRequest
				.sendGet("http://lookup.dbpedia.org/api/search/KeywordSearch?MaxHits=1&QueryClass=place&QueryString="
						+ URLEncoder.encode("Leipzig", "utf-8"));
		System.out.println(new JSONObject(result).getJSONArray("results").getJSONObject(0).getString("uri"));
		assertEquals(new JSONObject(result).getJSONArray("results").getJSONObject(0).getString("uri"),"http://dbpedia.org/resource/Leipzig");
	}

}
