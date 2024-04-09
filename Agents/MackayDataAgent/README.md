# Mackay Data Agent
The ` Mackay Data Agent` 




of TimeSeries data from external APIs into the knowledge graph (KG) using the [OntoTimeSeries] (TS) ontology. Reading and writing TimeSeries from/into the KG relies on the [TimeSeriesClient].


The agent is integrated with the [Derived Information Framework]'s (DIF) to ensure proper data provenance. API information and API-Data-to-TimeSeries mappings are defined as a meta-data instance in KG. TS data is then considered as the derived quantity of the meta-data. The required meta-data triples to derive an API-downloaded TS instance are described in the [required derivation markup](#13-required-derivation-markup) section below.

Once a API meta data is registered using the [`DerivationClient`](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation), API agent automatically manages a periodical re-downloading of the data from that API.

Ontology definition of API meta-data relies on  [Web of Things (WoT) Hypermedia Controls Ontology]. We also extend [RDF Mapping Language (RML)] for TimeSeries data as `RMLTS Ontology`.



The agent now supports three input data formats from API: `json, csv, xlsx.` Downloaded data is instantiated as [TWA](https://github.com/cambridge-cares/TheWorldAvatar) TimeSeries instances. Support of instantiating generic RDF triples is to be added in future.




# 1. Setup

## 1.1 
* api agent
* forecast agent
* rdb & triple store


## 1.3 Required Derivation Markup
Mackay Data agent is a client of API Agent. For the any API data used by the data agent to be properly managed by the API agent, the required meta-data instance need to be properly instantiated. Refer to API Agent section 1.3.



#2. Ontology
This ontology design is not considered as the finalized version and should only serve as a reference. The API request information is defined via [Web of Things (WoT) Hypermedia Controls Ontology]. The API-Data-to-TimeSeries mapping is defined from extending RDF Mapping Language (RML). See examples in the `tbox_dev` folder.

# 3. 











<!-- Links -->
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[py4jps]: https://pypi.org/project/py4jps/#description
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[Darts]: https://unit8co.github.io/darts/index.html
[Prophet]: https://unit8co.github.io/darts/generated_api/darts.models.forecasting.prophet_model.html
[Facebook Prophet]: https://github.com/facebook/prophet
[Github container registry]: https://ghcr.io
[personal access token]: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
[Derived Information Framework]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation
[Stack manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[derivation agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent

[OntoTimeSeries]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontotimeseries
[OntoDerivation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoderivation

<!-- files -->
[HTTP forecast error request]: ./resources/HTTP_evaluate_errors.http
[model mapping]: ./forecastingagent/fcmodels/model_mapping.py
[docker compose file]: ./docker-compose.yml
[stack manager input config file]: ./stack-manager-input-config/forecasting-agent.json
[stack-manager-input-config]: ./stack-manager-input-config
[test_plots]: tests/test_plots/
[Web of Things (WoT) Hypermedia Controls Ontology]:https://www.w3.org/2019/wot/hypermedia
[RDF Mapping Language (RML)]:https://rml.io/specs/rml/