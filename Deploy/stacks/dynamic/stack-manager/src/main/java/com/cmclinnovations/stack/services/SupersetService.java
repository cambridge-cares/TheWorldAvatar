package com.cmclinnovations.stack.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.odiszapc.nginxparser.NgxBlock;
import com.github.odiszapc.nginxparser.NgxConfig;
import com.github.odiszapc.nginxparser.NgxParam;

public class SupersetService extends ContainerService {

    public static final String TYPE = "superset";
    protected static final List<String> PROBLEMATICURIEXTENTIONS_LIST = Arrays.asList(
            "static", "superset", "sqllab", "savedqueryview", "druid", "tablemodelview", "databaseasync",
            "dashboardmodelview", "slicemodelview", "dashboardasync", "druiddatasourcemodelview", "api",
            "csstemplateasyncmodelview", "chart", "savedqueryviewapi", "r", "datasource", "sliceaddview");
    public static final String LOCATION = "location";

    public SupersetService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    public void addServerSpecificNginxSettingsToLocationBlock(NgxBlock locationBlock,
            Map<String, String> upstreams, Entry<String, Connection> endpoint) {
        Connection connection = endpoint.getValue();
        URI externalPath = connection.getExternalPath();

        // adding "proxy_redirect off;"
        NgxParam proxySetHeaderParam = new NgxParam();
        proxySetHeaderParam.addValue("proxy_redirect");
        proxySetHeaderParam.addValue("off");
        locationBlock.addEntry(proxySetHeaderParam);

        // adding "proxy_set_header X-Script-Name /dashboard;" or whatever is specified
        // as the externalPath
        NgxParam proxyRedirectParam = new NgxParam();
        proxyRedirectParam.addValue("proxy_set_header");
        proxyRedirectParam.addValue("X-Script-Name");
        proxyRedirectParam.addValue(FileUtils.fixSlashs(externalPath.getPath(), true, false));
        locationBlock.addEntry(proxyRedirectParam);
    }

    @Override
    public void addServerSpecificNginxLocationBlocks(NgxConfig locationConfigOut,
            Map<String, String> upstreams, Entry<String, Connection> endpoint) {
        Connection connection = endpoint.getValue();
        URI externalPath = connection.getExternalPath();

        // adding redirection of problematic uris
        NgxBlock specialRedirectionBlock = new NgxBlock();
        specialRedirectionBlock.addValue(LOCATION);
        specialRedirectionBlock.addValue("~");
        specialRedirectionBlock.addValue("^/(" + String.join("|", PROBLEMATICURIEXTENTIONS_LIST) + ")");

        NgxParam tryFileParam = new NgxParam();
        tryFileParam.addValue("try_files");
        tryFileParam.addValue("$uri");
        tryFileParam.addValue(FileUtils.fixSlashs(externalPath.getPath(), true, true) + "$uri");
        tryFileParam.addValue(FileUtils.fixSlashs(externalPath.getPath(), true, true) + "$uri?$query_string");
        tryFileParam.addValue("@rules");
        specialRedirectionBlock.addEntry(tryFileParam);
        locationConfigOut.addEntry(specialRedirectionBlock);

        // adding @rules "return 308
        // http://localhost:3850/dashboard$uri$is_args$query_string;"
        NgxBlock rulesBlock = new NgxBlock();
        rulesBlock.addValue(LOCATION);
        rulesBlock.addValue("@rules");

        NgxParam ruleParam = new NgxParam();
        ruleParam.addValue("return");
        ruleParam.addValue("308");
        ruleParam.addValue("$scheme://$http_host" + FileUtils.fixSlashs(externalPath.getPath(), true, false)
                + "$uri$is_args$query_string");
        rulesBlock.addEntry(ruleParam);
        locationConfigOut.addEntry(rulesBlock);
    }

    @Override
    public void doPreStartUpConfiguration() {
        ContainerSpec containerSpec = getContainerSpec();

        try (InputStream supersetConfig = SupersetService.class.getResourceAsStream("superset/superset_config.py");
                InputStreamReader inputStreamReader = new InputStreamReader(supersetConfig);
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
            String fileText = bufferedReader.lines().collect(Collectors.joining(System.lineSeparator()));
            containerSpec
                    .withCommand(List.of("/bin/sh", "-c",
                            "echo \"" + fileText + "\" > pythonpath/superset_config.py"
                                    + " && /usr/bin/run-server.sh"));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load \"superset_config.py\" file.", ex);
        }
    }

    @Override
    public void doPostStartUpConfiguration() {
        executeCommand("superset", "fab", "create-admin", "--username", "admin", "--firstname", "Superset",
                "--lastname", "Admin", "--email", "admin@superset.com", "--password", "admin");
        executeCommand("sh","-c","pip install sqlalchemy==1.4.46 && superset db upgrade && superset init");
    }
}
