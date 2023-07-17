package com.cmclinnovations.stack.clients.citydb;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.utils.TempDir;

public class CityDBClient extends ContainerClient {

    private static final Logger logger = LoggerFactory.getLogger(CityDBClient.class);

    private static CityDBClient instance = null;

    public static CityDBClient getInstance() {
        if (null == instance) {
            instance = new CityDBClient();
        }
        return instance;
    }

    private CityDBClient() {
    }

    public void uploadCityGMLStringToPostGIS(String fileContents, String database, ImpExpOptions options,
            String lineage, boolean append) {
        uploadStringToPostGIS("gml", fileContents, database, options, lineage, append);
    }

    public void uploadCityJSONStringToPostGIS(String fileContents, String database, ImpExpOptions options,
            String lineage, boolean append) {
        uploadStringToPostGIS("kml", fileContents, database, options, lineage, append);
    }

    private void uploadStringToPostGIS(String fileType, String fileContents, String database, ImpExpOptions options,
            String lineage, boolean append) {
        try (TempDir tmpDir = makeLocalTempDir()) {
            Path filePath = Files.createTempFile(tmpDir.getPath(), "citydb", "." + fileType);
            try {
                Files.writeString(filePath, fileContents);
                uploadToPostGIS(filePath.toString(), database, options, lineage, append);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to write string for '" + fileType
                        + "' layer to a file in a temporary directory.", ex);
            }
        } catch (IOException ex) {
            throw new RuntimeException("Failed to create temporary " + fileType + " file", ex);
        }
    }

    public void uploadFilesToPostGIS(String dirPath, String database, ImpExpOptions options, String lineage,
            boolean append) {
        try (Stream<Path> paths = Files.list(Path.of(dirPath))) {
            if (paths.filter(path -> Files.isRegularFile(path)
                    && Stream.of(".gz", ".gzip", ".zip")
                            .anyMatch(extension -> path.toString().endsWith(extension)))
                    .map(path -> {
                        uploadFileToPostGIS(path.toString(), database, options, lineage, append);
                        return path;
                    })
                    .count() == 0) {
                try (TempDir tmpDir = makeLocalTempDir()) {
                    tmpDir.copyFrom(Path.of(dirPath));
                    uploadToPostGIS(tmpDir.toString(), database, options, lineage, append);
                }
            }
        } catch (IOException ex) {
            throw new RuntimeException("Failed to list child paths of the directory '" + dirPath + "'.", ex);
        }
    }

    public void uploadFileToPostGIS(String filePath, String database, ImpExpOptions options, String lineage,
            boolean append) {
        try (TempDir tmpDir = makeLocalTempDir()) {
            Path sourcePath = Path.of(filePath);
            tmpDir.copyFrom(sourcePath);

            uploadToPostGIS(tmpDir.getPath().resolve(sourcePath.getFileName()).toString(),
                    database, options, lineage, append);
        }
    }

    public void uploadURLToPostGIS(String url, String database, ImpExpOptions options, String lineage,
            boolean append) {
        uploadToPostGIS(url, database, options, lineage, append);
    }

    private void uploadToPostGIS(String filePath, String database, ImpExpOptions options, String lineage,
            boolean append) {

        updateDatabase(database, options.getSridIn());

        String containerId = getContainerId("citydbimpexp");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(containerId,
                options.appendArgs(filePath,
                        "--db-name", database,
                        "--lineage", lineage))
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .withEvaluationTimeout(600)
                .exec();

        handleErrors(errorStream, execId, logger);

        addIRIs(database);
    }

    private void updateDatabase(String database, String sridIn) {

        String containerId = getContainerId("postgis");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(containerId, "sh", "-c",
                "psql -d ${POSTGRES_DB} -c \"SELECT EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'citydb');\" --csv | grep -x \"t\" || /docker-entrypoint-initdb.d/3dcitydb-initdb.sh")
                .withEnvVar("POSTGRES_DB", database)
                .withEnvVar("SRID", sridIn)
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();

        handleErrors(errorStream, execId, logger);

    }

    private void addIRIs(String database) {
        String sqlFilename = "citydb_add_uuids_and_iris.sql";
        try (InputStream is = CityDBClient.class.getResourceAsStream(sqlFilename)) {
            String sqlQuery = new String(is.readAllBytes());
            PostGISClient.getInstance().getRemoteStoreClient(database).executeUpdate(sqlQuery);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read resource file '" + sqlFilename + "'.", ex);
        }
    }

    public void applyThematicSurfacesFix(String database) {
        String sqlFilename = "citydb_thematic_surfaces_fix.sql";
        try (InputStream is = CityDBClient.class.getResourceAsStream(sqlFilename)) {
            String sqlQuery = new String(is.readAllBytes());
            PostGISClient.getInstance().getRemoteStoreClient(database).executeUpdate(sqlQuery);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read resource file '" + sqlFilename + "'.", ex);
        }
    }
}
