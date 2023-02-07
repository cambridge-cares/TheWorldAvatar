package com.cmclinnovations.stack.services;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.exceptions.InvalidTemplateException;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.EndpointSpec;
import com.github.dockerjava.api.model.PortConfig;
import com.github.odiszapc.nginxparser.NgxBlock;
import com.github.odiszapc.nginxparser.NgxConfig;
import com.github.odiszapc.nginxparser.NgxDumper;
import com.github.odiszapc.nginxparser.NgxEntry;
import com.github.odiszapc.nginxparser.javacc.NginxConfigParser;
import com.github.odiszapc.nginxparser.javacc.ParseException;

public final class NginxService extends ContainerService implements ReverseProxyService {

    /**
     *
     */
    private static final String EXTERNAL_PORT = "EXTERNAL_PORT";

    public static final String TYPE = "nginx";

    private static final String TEMPLATE_TYPE = "Nginx config";
    private static final String NGINX_CONFIGS_DIR = "nginx/configs/";
    private static final String SERVER_CONF_TEMPLATE = NGINX_CONFIGS_DIR + "default_server.conf";
    private static final String LOCATIONS_CONF_TEMPLATE = NGINX_CONFIGS_DIR + "default_locations.conf";
    private static final String UPSTREAM_CONF_TEMPLATE = NGINX_CONFIGS_DIR + "default_upstream.conf";

    private static final String CMD = "nginx";

    private ConfigSender sender = new ConfigSender();

    public NginxService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        updateExternalPort(config);

        try (InputStream inStream = new BufferedInputStream(
                NginxService.class.getResourceAsStream(SERVER_CONF_TEMPLATE))) {
            NginxConfigParser parser = new NginxConfigParser(inStream);
            NgxConfig defaultConfigTemplate = parser.parse();
            sender.addConfig(defaultConfigTemplate, "default.conf");
        } catch (ParseException | IOException ex) {
            throw new InvalidTemplateException(TEMPLATE_TYPE, SERVER_CONF_TEMPLATE, ex);
        }
    }

    private void updateExternalPort(ServiceConfig config) {
        String externalPort = System.getenv(EXTERNAL_PORT);
        if (null != externalPort) {
            EndpointSpec endpointSpec = config.getDockerServiceSpec().getEndpointSpec();
            if (null != endpointSpec) {
                List<PortConfig> ports = endpointSpec.getPorts();
                if (null != ports) {
                    ports.stream()
                            .filter(port -> port.getTargetPort() == 80)
                            .forEach(port -> port.withPublishedPort(Integer.parseInt(externalPort)));
                }
            }
        }
    }

    public void addService(ContainerService service) {

        NgxConfig locationConfigOut = new NgxConfig();

        Map<String, String> upstreams = new HashMap<>();

        try {
            for (Entry<String, Connection> endpoint : service.getEndpoints().entrySet()) {
                addLocation(service, locationConfigOut, upstreams, endpoint);
            }
        } catch (ParseException | IOException ex) {
            throw new InvalidTemplateException(TEMPLATE_TYPE, LOCATIONS_CONF_TEMPLATE, ex);
        }

        if (!locationConfigOut.getEntries().isEmpty()) {
            sender.addConfig(locationConfigOut, "locations/" + service.getName() + ".conf");

            if (!upstreams.isEmpty()) {
                try {
                    NgxConfig upstreamConfigOut = new NgxConfig();
                    for (Entry<String, String> upstream : upstreams.entrySet()) {
                        addUpstream(upstreamConfigOut, upstream);
                    }
                    sender.addConfig(upstreamConfigOut, service.getName() + "_upstream.conf");

                    sender.sendConfigs();
                } catch (ParseException | IOException ex) {
                    throw new InvalidTemplateException(TEMPLATE_TYPE, UPSTREAM_CONF_TEMPLATE, ex);
                }
            }
        }
    }

    private void addUpstream(NgxConfig upstreamConfigOut, Entry<String, String> upstream)
            throws ParseException, IOException {
        try (InputStream inStream = new BufferedInputStream(
                NginxService.class.getResourceAsStream(UPSTREAM_CONF_TEMPLATE))) {
            NginxConfigParser parser = new NginxConfigParser(inStream);
            NgxConfig upstreamConfigTemplate = parser.parse();
            NgxBlock upstreamBlock = upstreamConfigTemplate.findBlock("upstream");
            upstreamBlock.addValue(upstream.getKey());
            upstreamBlock.findParam("server").addValue(upstream.getValue());
            upstreamConfigOut.addEntry(upstreamBlock);
        }
    }

    private void addLocation(ContainerService service, NgxConfig locationConfigOut, Map<String, String> upstreams,
            Entry<String, Connection> endpoint) throws ParseException, IOException {
        Connection connection = endpoint.getValue();
        URI externalPath = connection.getExternalPath();
        if (null != externalPath) {
            try (InputStream inStream = new BufferedInputStream(
                    NginxService.class.getResourceAsStream(LOCATIONS_CONF_TEMPLATE))) {
                String serviceName = service.getName();
                String upstreamName = serviceName + "_" + endpoint.getKey();
                NginxConfigParser parser = new NginxConfigParser(inStream);
                NgxConfig configTemplate = parser.parse();
                configTemplate.getEntries();
                List<NgxEntry> locationEntries = configTemplate.findAll(NgxBlock.class, "location");
                for (NgxEntry locationEntry : locationEntries) {
                    NgxBlock locationBlock = NgxBlock.class.cast(locationEntry);
                    List<String> values = locationBlock.getValues();
                    if (1 == values.size() && values.contains("=")) {
                        locationBlock.addValue(FileUtils.fixSlashs(externalPath.getPath(), true, false));
                    } else {
                        locationBlock.addValue(FileUtils.fixSlashs(externalPath.getPath(), true, true));
                        locationBlock.findParam("proxy_pass")
                                .addValue(getProxyPassValue(connection, upstreamName));
                        upstreams.put(upstreamName, getServerURL(connection, serviceName));
                        service.addServerSpecificNginxSettingsToLocationBlock(locationBlock, upstreams,
                                endpoint);
                    }
                    locationConfigOut.addEntry(locationBlock);
                }
                service.addServerSpecificNginxLocationBlocks(locationConfigOut, upstreams, endpoint);
            }
        }
    }

    private String getProxyPassValue(Connection connection, String hostname) {
        URL url = connection.getUrl();
        String fileComponent = url.getFile();
        fileComponent = fileComponent.isEmpty() ? "/" : fileComponent;
        return url.getProtocol() + "://" + hostname + fileComponent;
    }

    private String getServerURL(Connection connection, String hostname) {
        URL url = connection.getUrl();
        int port = url.getPort();
        return hostname + ":" + ((-1 == port) ? 80 : port);
    }

    private final class ConfigSender {
        Map<String, byte[]> files = new HashMap<>();

        private void addConfig(NgxConfig config, String filepath) {
            NgxDumper dumper = new NgxDumper(config);
            String fileContents = dumper.dump();
            files.put(filepath, fileContents.getBytes());
        }

        public void sendConfigs() {
            sendFiles(files, "/etc/nginx/conf.d");
            executeCommand(CMD, "-s", "reload");
            files.clear();
        }

    }

}
