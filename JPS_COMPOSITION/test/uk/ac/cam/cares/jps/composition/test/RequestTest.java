package uk.ac.cam.cares.jps.composition.test;

import java.net.URLEncoder;

import org.junit.Test;

import uk.ac.cam.cares.jps.composition.util.SendRequest;

public class RequestTest {

	@Test
	public void test() throws Exception {
		String result = SendRequest
				.sendGet("http://lookup.dbpedia.org/api/search/KeywordSearch?MaxHits=1&QueryClass=city&QueryString="
						+ URLEncoder.encode("Leipzig", "utf-8"));
		System.out.println(result);
	}

}
