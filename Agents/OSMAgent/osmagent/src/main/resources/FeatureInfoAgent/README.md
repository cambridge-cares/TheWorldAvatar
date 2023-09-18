# Querying Building Usage Information with FeatureInfoAgent
1) Upload [building_usage.obda](./building_usage.obda) via stack-data-uploader.
2) Run the SPARQL Update in [Building_Class.sparql] (building_class.sparql).
3) Place [building_usage_FIA.sparql](queries/building_usage_FIA.sparql) and [fia-config.json](queries/fia-config.json) into FeatureInfoAgent's [queries](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent/queries) subfolder.
4) Spin FeatureInfoAgent up along with the [stack-manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-the-feature-info-agent). 