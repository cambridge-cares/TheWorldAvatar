package com.cmclinnovations.stack.clients.postgis;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.Options;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.clients.utils.TempDir;

public class PGRoutingClient extends PostGISClient {

    private static final Logger logger = LoggerFactory.getLogger(PGRoutingClient.class);

    private static PGRoutingClient instance = null;

    public static PGRoutingClient getInstance() {
        if (null == instance) {
            instance = new PGRoutingClient();
        }
        return instance;
    }

    public void uploadRoutingDataDirectoryToPostGIS(String database, String sourceDirectory, String tablePrefix,
            Options options, boolean append) {
        List<Path> allFilesList;
        try (Stream<Path> dirsStream = Files.walk(Path.of(sourceDirectory))) {
            allFilesList = dirsStream.filter(file -> !Files.isDirectory(file)).collect(Collectors.toList());
        } catch (IOException ex) {
            throw new RuntimeException("Failed to walk directory '" + sourceDirectory + "'.", ex);
        }
        List<Path> osmFilesList = allFilesList.stream().filter(file -> FileUtils.hasFileExtension(file, "osm"))
                .collect(Collectors.toList());
        List<Path> pbfFilesList = allFilesList.stream().filter(file -> FileUtils.hasFileExtension(file, "pbf"))
                .collect(Collectors.toList());
        if (osmFilesList.isEmpty() && pbfFilesList.isEmpty()) {
            throw new RuntimeException("No osm or pbf file in routing data directory '" + sourceDirectory + "'.");
        }
        List<Path> configsList = allFilesList.stream().filter(file -> FileUtils.hasFileExtension(file, "xml"))
                .collect(Collectors.toList());
        if (configsList.size() > 1) {
            throw new RuntimeException(
                    "Too many xml config files (" + osmFilesList.size() + ") in routing data directory '"
                            + sourceDirectory + "'.");
        } else if (configsList.isEmpty()) {
            throw new RuntimeException(
                    "No xml config files found in routing data directory '" + sourceDirectory + "'.");
        }
        uploadRoutingFilesToPostGIS(database, configsList.get(0), osmFilesList, pbfFilesList, tablePrefix, options,
                append);
    }

    private void convertPBF2OSM(String pbfFileName, String osmFileName, String containerId) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(containerId, "bash", "-c", "-x",
                "which osmconvert || (apt-get update && apt-get install -y osmctools && rm -rf /var/lib/apt/lists/*) && osmconvert "
                        + pbfFileName + " --drop-author --drop-version --out-osm -o=" + osmFileName)
                .withOutputStream(outputStream)
                .withErrorStream(outputStream)
                .withEvaluationTimeout(300)
                .exec();

        handleErrors(outputStream, execId, logger);
    }

    public void uploadRoutingFilesToPostGIS(String database, Path configFilePath, List<Path> osmFilesList,
            List<Path> pbfFilesList, String tablePrefix, Options options, boolean append) {
        try (TempDir tmpDir = makeLocalTempDir()) {
            tmpDir.copyFrom(configFilePath);
            osmFilesList.stream().forEach(tmpDir::copyFrom);
            pbfFilesList.stream().forEach(tmpDir::copyFrom);

            Path tempConfigFilePath = tmpDir.getPath().resolve(configFilePath.getFileName());
            List<Path> tempOSMFilesList = osmFilesList.stream().map(f -> tmpDir.getPath().resolve(f.getFileName()))
                    .collect(Collectors.toList());
            List<Path> tempPBFFilesList = pbfFilesList.stream().map(f -> tmpDir.getPath().resolve(f.getFileName()))
                    .collect(Collectors.toList());

            tempPBFFilesList.stream().forEach(f -> {
                String osmFileName = f.toString().substring(0, f.toString().lastIndexOf('.'));
                if (!osmFileName.endsWith(".osm")) {
                    osmFileName = osmFileName + ".osm";
                }
                convertPBF2OSM(f.toString(), osmFileName, getContainerId("postgis"));
                tempOSMFilesList.add(Path.of(osmFileName));
            });

            tempOSMFilesList.stream().findFirst().ifPresent(osmFile -> uploadRoutingToPostGIS(database, osmFile,
                    tempConfigFilePath, tablePrefix, options, append));
            tempOSMFilesList.stream().skip(1).forEach(osmFile -> uploadRoutingToPostGIS(database, osmFile,
                    tempConfigFilePath, tablePrefix, options, true));
        }
    }

    public void uploadRoutingToPostGIS(String database, Path osmFilePath, Path configFilePath, String tablePrefix,
            Options options, boolean append) {
        String containerId = getContainerId("postgis");
        ensurePostGISRoutingSupportEnabled(database, containerId);

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(containerId,
                constructOSM2PGRoutingCommand(osmFilePath.toString(), configFilePath.toString(), database, tablePrefix,
                        options,
                        append))
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .withEvaluationTimeout(300)
                .exec();

        handleErrors(errorStream, execId, logger);
    }

    private String[] constructOSM2PGRoutingCommand(String osmFile, String configFile, String database,
            String tablePrefix, Options options, boolean append) {
        List<String> command = new ArrayList<>(Arrays.asList("osm2pgrouting", "--f", osmFile, "--conf",
                configFile, "--dbname", database, "--username", postgreSQLEndpoint.getUsername(),
                "--password", postgreSQLEndpoint.getPassword(), "--prefix", tablePrefix));
        if (!append) {
            command.add("--clean");
        }
        command.addAll(options.getOptionsList());

        return command.toArray(String[]::new);
    }

    private void ensurePostGISRoutingSupportEnabled(String database, String postGISContainerId) {
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument(
                        "CREATE EXTENSION IF NOT EXISTS pgrouting CASCADE; CREATE EXTENSION IF NOT EXISTS hstore;")
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
    }
}
