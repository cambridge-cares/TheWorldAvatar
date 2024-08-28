package com.cmclinnovations.stack.clients.grafana;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
//import com.cmclinnovations.swagger.grafana.api.DashboardsApi;

public class GrafanaClient extends ClientWithEndpoint<GrafanaEndpointConfig> {

    private final PostGISEndpointConfig postgreSQLEndpoint;
    private final GrafanaEndpointConfig grafanaEndpoint;

    private static GrafanaClient instance = null;

    public static GrafanaClient getInstance() {
        if (null == instance) {
            instance = new GrafanaClient();
        }
        return instance;
    }

    private GrafanaClient() {
        super("grafana", GrafanaEndpointConfig.class);
        postgreSQLEndpoint = readEndpointConfig(EndpointNames.POSTGIS, PostGISEndpointConfig.class);
        grafanaEndpoint = readEndpointConfig(EndpointNames.GRAFANA, GrafanaEndpointConfig.class);
    }

    public void addDatasource(String databaseName) {
        // DashboardsApi dashboardsApi = new DashboardsApi();
        throw new UnsupportedOperationException("Unimplemented method 'addDatasource'");
    }

    public void removeDatasource(String databaseName) {
        throw new UnsupportedOperationException("Unimplemented method 'removeDatasource'");
    }

}
