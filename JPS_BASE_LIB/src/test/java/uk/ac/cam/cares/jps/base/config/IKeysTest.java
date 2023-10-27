package uk.ac.cam.cares.jps.base.config;

import junit.framework.TestCase;

public class IKeysTest extends TestCase {
	public void testIKeys() {
		
		assertEquals(IKeys.HOST,"host");
		assertEquals(IKeys.PORT,"port");

		assertEquals(IKeys.URL_BUILDINGSQUERY_BERLIN,"url.buildingsquery.berlin");
		assertEquals(IKeys.URL_BUILDINGSQUERY_THEHAGUE,"url.buildingsquery.thehague");
		assertEquals(IKeys.URL_BUILDINGSQUERY_SINGAPORE,"url.buildingsquery.singapore");
		assertEquals(IKeys.URL_BUILDINGSQUERY_HONGKONG,"url.buildingsquery.hongkong");

		assertEquals(IKeys.URL_POSITIONQUERY,"url.positionquery");
		
		assertEquals(IKeys.URL_VIRTUALSENSOR,"url.virtualsensor");

		assertEquals(IKeys.DATASET_TEMPLATE_URL,"dataset.%s.url");
		assertEquals(IKeys.DATASET_TEMPLATE_KBCLASS,"dataset.%s.kbclass");
		assertEquals(IKeys.DATASET_TEMPLATE_ENDPOINT_URL,"dataset.%s.endpoint.url");
		assertEquals(IKeys.DATASET_META_URL,"dataset.meta.url");
		assertEquals(IKeys.DATASET_WEATHER_URL,"dataset.weather.url");
		assertEquals(IKeys.DATASET_AIRQUALITY_URL,"dataset.airquality.url");
		assertEquals(IKeys.URL_RDF_METADATA,"url.rdf.metadata");
		
		assertEquals(IKeys.SCENARIO_USECASEDIRECTORY_SEPARATOR,"scenario.usecasedirectory.separator");
		assertEquals(IKeys.ABSDIR_ROOT,"absdir.root");
		assertEquals(IKeys.PATH_KNOWLEDGEBASE_SHIPS,"path.knowledgebase.ships");
		assertEquals(IKeys.URL_SCHEME,"url.scheme");
		assertEquals(IKeys.LONG_NUCLEAR_GAMS,"apply.longtime.nuclear.gams");
	}
}
