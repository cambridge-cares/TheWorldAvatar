package com.cmclinnovations.services;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.cmclinnovations.FileUtils;
import com.cmclinnovations.exceptions.InvalidTemplateException;
import com.github.odiszapc.nginxparser.NgxBlock;
import com.github.odiszapc.nginxparser.NgxConfig;
import com.github.odiszapc.nginxparser.NgxDumper;
import com.github.odiszapc.nginxparser.NgxEntry;
import com.github.odiszapc.nginxparser.javacc.NginxConfigParser;
import com.github.odiszapc.nginxparser.javacc.ParseException;

public class NginxService extends ContainerService implements ReverseProxyService {

    public static final String TYPE = "nginx";

    private static final String TEMPLATE_TYPE = "Nginx config";
    private static final String SERVER_CONF_TEMPLATE = "nginx/configs/default_server.conf";
    private static final String LOCATIONS_CONF_TEMPLATE = "nginx/configs/default_locations.conf";
    private static final String UPSTREAM_CONF_TEMPLATE = "nginx/configs/default_upstream.conf";

    private static final String CMD = "nginx";

    private ConfigSender sender = new ConfigSender();

    public NginxService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        try (InputStream inStream = new BufferedInputStream(
                NginxService.class.getResourceAsStream(SERVER_CONF_TEMPLATE))) {
            NginxConfigParser parser = new NginxConfigParser(inStream);
            NgxConfig defaultConfigTemplate = parser.parse();
            sender.addConfig(defaultConfigTemplate, "default.conf");
        } catch (ParseException ex) {
            throw new InvalidTemplateException(TEMPLATE_TYPE, SERVER_CONF_TEMPLATE, ex);
        } catch (IOException ex) {
            throw new InvalidTemplateException(TEMPLATE_TYPE, SERVER_CONF_TEMPLATE, ex);
        }
    }

    public void addService(ContainerService service) {

        NgxConfig locationConfigOut = new NgxConfig();

        Map<String, String> upstreams = new HashMap<>();

        try {
            for (Entry<String, Connection> endpoint : service.getEndpoints().entrySet()) {
                addLocation(service, locationConfigOut, upstreams, endpoint);
            }
        } catch (ParseException ex) {
            throw new InvalidTemplateException(TEMPLATE_TYPE, LOCATIONS_CONF_TEMPLATE, ex);
        } catch (IOException ex) {
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
                } catch (ParseException ex) {
                    throw new InvalidTemplateException(TEMPLATE_TYPE, UPSTREAM_CONF_TEMPLATE, ex);
                } catch (IOException ex) {
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
                String upstreamName = service.getName() + "_" + endpoint.getKey();
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
                        upstreams.put(upstreamName, getServerURL(connection, service.getContainerName()));
                    }
                    locationConfigOut.addEntry(locationBlock);
                }
            }
        }
    }

    private String getProxyPassValue(Connection connection, String hostname) {
        URL url = connection.getUrl();
        return url.getProtocol() + "://" + hostname + "/";
    }

    private String getServerURL(Connection connection, String hostname) {
        URL url = connection.getUrl();
        int port = url.getPort();
        return hostname + ":" + ((-1 == port) ? 80 : port);
    }

    private class ConfigSender {
        Map<String, String> files = new HashMap<>();

        private void addConfig(NgxConfig config, String filepath) {
            NgxDumper dumper = new NgxDumper(config);
            String fileContents = dumper.dump();
            files.put(filepath, fileContents);
        }

        public void sendConfigs() throws IOException {
            DockerService docker = getService("docker");
            docker.sendFiles(getContainerId(), files, "/etc/nginx/conf.d");
            docker.executeCommand(getContainerId(), CMD, "-s", "reload");
            files.clear();
        }
    }
}
