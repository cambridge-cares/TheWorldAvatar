package uk.ac.cam.cares.jps.base.config;

public interface IKeys {

	String HOST = "host";
	String PORT = "port";
	
	String URL_BUILDINGSQUERY_BERLIN = "url.buildingsquery.berlin";
	String URL_BUILDINGSQUERY_THEHAGUE = "url.buildingsquery.thehague";
	String URL_BUILDINGSQUERY_SINGAPORE = "url.buildingsquery.singapore";
	String URL_BUILDINGSQUERY_HONGKONG = "url.buildingsquery.hongkong";
	
	String URL_POSITIONQUERY = "url.positionquery";

	String DATASET_TEMPLATE_URL = "dataset.%s.url";
	String DATASET_TEMPLATE_KBCLASS = "dataset.%s.kbclass";
	String DATASET_TEMPLATE_ENDPOINT_URL = "dataset.%s.endpoint.url";
	String DATASET_META_URL = "dataset.meta.url";
	
	String SCENARIO_USECASEDIRECTORY_SEPARATOR = "scenario.usecasedirectory.separator";
	String ABSDIR_ROOT =  "absdir.root";
	String PATH_KNOWLEDGEBASE_SHIPS = "path.knowledgebase.ships";
	String URL_SCHEME = "url.scheme";
}
