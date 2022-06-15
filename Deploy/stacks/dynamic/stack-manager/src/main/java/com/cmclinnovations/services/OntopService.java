package com.cmclinnovations.services;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.cmclinnovations.FileUtils;
import com.cmclinnovations.services.config.ServiceConfig;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

import it.unibz.inf.ontop.exception.MappingException;
import it.unibz.inf.ontop.injection.OntopMappingConfiguration;
import it.unibz.inf.ontop.injection.OntopSQLOWLAPIConfiguration;
import it.unibz.inf.ontop.spec.mapping.PrefixManager;
import it.unibz.inf.ontop.spec.mapping.pp.PreProcessedMapping;
import it.unibz.inf.ontop.spec.mapping.pp.SQLPPMapping;
import it.unibz.inf.ontop.spec.mapping.pp.SQLPPTriplesMap;
import it.unibz.inf.ontop.spec.mapping.serializer.impl.OntopNativeMappingSerializer;

public final class OntopService extends ContainerService {

    private static final class SQLPPMappingImplementation implements SQLPPMapping {

        private Map<String, String> prefixMap = new HashMap<>();
        private List<SQLPPTriplesMap> triplesMap = new ArrayList<>();

        public void addMappings(Path ontopMappingFilePath) {
            try {
                Path tempFilePath = reformatMappingFile(ontopMappingFilePath);

                PreProcessedMapping<SQLPPTriplesMap> extraMapings = generateConfiguration(tempFilePath)
                        .loadProvidedPPMapping();

                prefixMap.putAll(extraMapings.getPrefixManager().getPrefixMap());
                triplesMap.addAll(extraMapings.getTripleMaps());
            } catch (MappingException ex) {
                // TODO Auto-generated catch block
                ex.printStackTrace();
            } catch (IOException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }
        }

        private static Path reformatMappingFile(Path ontopMappingFilePath) throws IOException {
            Path tempFilePath = createTempOBDAFile(ontopMappingFilePath);
            try (BufferedReader bufferedReader = Files.newBufferedReader(ontopMappingFilePath);
                    BufferedWriter bufferedWriter = Files.newBufferedWriter(tempFilePath)) {
                bufferedReader.lines()
                        .map(line -> line.replaceFirst("^ *#.*$", ""))
                        .map(line -> line.replaceFirst("(, *|; *|\\. *)", "\1\n"))
                        .forEachOrdered(string -> {
                            try {
                                bufferedWriter.write(string, 0, string.length());
                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                        });
            }
            return tempFilePath;
        }

        private static Path createTempOBDAFile(Path ontopMappingFilePath) throws IOException {
            return File.createTempFile(FileUtils.getFileNameWithoutExtension(ontopMappingFilePath), ".obda").toPath();
        }

        private static OntopSQLOWLAPIConfiguration generateConfiguration(Path ontopMappingFilePath) {
            return OntopSQLOWLAPIConfiguration
                    .defaultBuilder()
                    .nativeOntopMappingFile(ontopMappingFilePath.toFile())
                    .jdbcDriver("dummy")
                    .jdbcUrl("dummy")
                    .jdbcUser("")
                    .jdbcPassword("")
                    .build();
        }

        @Override
        public PrefixManager getPrefixManager() {
            return OntopMappingConfiguration.defaultBuilder().build()
                    .getSpecificationFactory()
                    .createPrefixManager(ImmutableMap.copyOf(prefixMap));
        }

        @Override
        public ImmutableList<SQLPPTriplesMap> getTripleMaps() {
            return ImmutableList.copyOf(triplesMap);
        }

        public void serialize(Path ontopMappingFilePath) throws IOException {

            OntopNativeMappingSerializer writer = new OntopNativeMappingSerializer();

            writer.write(ontopMappingFilePath.toFile(), this);

        }
    }

    public static final String TYPE = "ontop";

    private static final String ONTOP_DB_DRIVER_URL = "ONTOP_DB_DRIVER_URL";
    private static final String ONTOP_MAPPING_FILE = "ONTOP_MAPPING_FILE";

    public OntopService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        setEnvironmentVariableIfAbsent("ONTOP_LAZY_INIT", "false");
        setEnvironmentVariableIfAbsent("ONTOP_CORS_ALLOWED_ORIGINS", "*");
        setEnvironmentVariableIfAbsent("ONTOP_DEBUG", "false");
        checkEnvironmentVariableNonNull(ONTOP_MAPPING_FILE);
        checkEnvironmentVariableNonNull("ONTOP_DB_URL");
        checkEnvironmentVariableNonNull("ONTOP_DB_DRIVER");
        checkEnvironmentVariableNonNull(ONTOP_DB_DRIVER_URL);
        checkEnvironmentVariableNonNull("ONTOP_DB_USER");
        checkEnvironmentVariableNonNull("ONTOP_DB_PASSWORD_FILE");

        config.getContainerSpec()
                .withCommand(List.of("/bin/sh", "-c", "wget -P /opt/ontop/jdbc "
                        + getEnvironmentVariable(ONTOP_DB_DRIVER_URL)
                        + " && ./entrypoint.sh"));
    }

    @Override
    public void doPostStartUpConfigurationImpl() {
        transformOBDA();
    }

    @Deprecated
    private void copyInJDBCLibrary() {
        try {
            URL url = new URL(getEnvironmentVariable(ONTOP_DB_DRIVER_URL));
            String filename = Path.of(url.getPath()).getFileName().toString();
            String folderPath = "/opt/ontop/jdbc";
            downloadFileAndSendItToContainer(url, folderPath, filename, false);
        } catch (MalformedURLException ex) {
            throw new IllegalArgumentException("Value provided for the environment variable '" + ONTOP_DB_DRIVER_URL
                    + "' for contianer '" + getName() + "' is not a valid URL.", ex);
        }
    }

    private void transformOBDA() {
        Path ontopMappingFilePath = Path.of(getEnvironmentVariable(ONTOP_MAPPING_FILE));

        try {
            SQLPPMappingImplementation mapping = new SQLPPMappingImplementation();
            Path localTempOntopMappingFilePath = SQLPPMappingImplementation.createTempOBDAFile(ontopMappingFilePath);
            mapping.serialize(localTempOntopMappingFilePath);

            try {
                Map<String, byte[]> files = Map.of(ontopMappingFilePath.getFileName().toString(),
                        Files.readAllBytes(localTempOntopMappingFilePath));

                sendFiles(files, ontopMappingFilePath.getParent().toString());
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to write out combined Ontop mapping file '" + ontopMappingFilePath + "'.", ex);
        }
    }

}
