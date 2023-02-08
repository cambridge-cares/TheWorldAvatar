package uk.ac.cam.cares.jps.base.config;

public interface IKeys {

	String HOST = "host";
	String PORT = "port";
	
	String URL_BUILDINGSQUERY_BERLIN = "url.buildingsquery.berlin";
	String URL_BUILDINGSQUERY_THEHAGUE = "url.buildingsquery.thehague";
	String URL_BUILDINGSQUERY_SINGAPORE = "url.buildingsquery.singapore";
	String URL_BUILDINGSQUERY_HONGKONG = "url.buildingsquery.hongkong";
	
	String URL_POSITIONQUERY = "url.positionquery";

	String URL_VIRTUALSENSOR = "url.virtualsensor";
	
	String DATASET_TEMPLATE_URL = "dataset.%s.url";
	String DATASET_TEMPLATE_KBCLASS = "dataset.%s.kbclass";
	String DATASET_TEMPLATE_ENDPOINT_URL = "dataset.%s.endpoint.url";
	String DATASET_META_URL = "dataset.meta.url";
	String DATASET_WEATHER_URL = "dataset.weather.url";
	String DATASET_AIRQUALITY_URL = "dataset.airquality.url";
	String URL_RDF_METADATA = "url.rdf.metadata";
	
	String SCENARIO_USECASEDIRECTORY_SEPARATOR = "scenario.usecasedirectory.separator";
	String ABSDIR_ROOT =  "absdir.root";
	String PATH_KNOWLEDGEBASE_SHIPS = "path.knowledgebase.ships";
	String URL_SCHEME = "url.scheme";
	String LONG_NUCLEAR_GAMS="apply.longtime.nuclear.gams";
	
	String STOREROUTER_CACHE_SIZE = "storerouter.cache.size";
	String RDB_STOREROUTER_CACHE_SIZE = "rdb.storerouter.cache.size";
	String URL_STOREROUTER_ENDPOINT = "url.storerouter.endpoint";
	String URL_RDB_STOREROUTER_ENDPOINT = "url.rdbstorerouter.endpoint";
	String URL_ACCESSAGENT_HOST = "url.accessagent.host";
	
	String URL_AGENTROUTER_ENDPOINT = "url.agentrouter.endpoint";
	String AGENTROUTER_CACHE_SIZE = "agentrouter.cache.size";
}
