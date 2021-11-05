/*
 * Copyright (c) 2011-2019  CMCL Innovations - All Rights Reserved
 *
 * This application and all inherent data, source files, information and graphics are
 * the copyright and sole property of Computational Modelling Cambridge Ltd (CMCL Innovations).
 *
 * Any unauthorised redistribution or reproduction of part, or all, of the contents of this
 * application in any form is prohibited under UK Copyright Law. You may not, except with the
 * express written permission of CMCL Innovations, distribute or commercially exploit this
 * application or its content. All other rights reserved.
 *
 * For more information please contact support(@)cmclinnovations.com
 */
package uk.ac.cam.cares.jps.agent.mechanism.moo.test;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.apache.jena.atlas.json.JSON;
import org.apache.jena.atlas.json.JsonObject;
import org.junit.Assert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author George Brownbridge {@literal <gbrownbridge@cmclinnovations.com>}
 */
public class FileComparer {

    private static final Logger LOGGER = LoggerFactory.getLogger(FileComparer.class);

    /**
     * Compare the files in the 'resultsDirPath' directory to those in the test
     * resources directory corresponding to the package of the 'testClass'.
     *
     * @param testClass class of the test object, used to specify the location
     * of the reference files
     * @param resultsDirPath directory containing generated files
     * @param fileMap map between paths in the results and paths in the
     * reference directory
     * @throws IOException
     * @throws java.net.URISyntaxException
     */
    public static void compareResultsToReferences(final Class<?> testClass, Path resultsDirPath, Map<String, String> fileMap, List<String> filePathsToIgnore) throws IOException, URISyntaxException {
        final URL baseRootURL = testClass.getResource("");
        Path baseDirPath = Path.of(baseRootURL.toURI());
        compareDirectories(baseDirPath, resultsDirPath, fileMap, filePathsToIgnore);
    }

    /**
     * Compare the content of two directories
     *
     * @param baseDirPath directory containing reference files
     * @param resultsDirPath directory containing generated files
     * @param fileMap map between paths in the results and paths in the
     * reference directory
     * @throws IOException
     */
    public static void compareDirectories(Path baseDirPath, Path resultsDirPath, Map<String, String> fileMap, List<String> filePathsToIgnore) throws IOException {
        try (final Stream<Path> resultsPathStream = Files.walk(resultsDirPath)) {
            resultsPathStream
                    .filter(resultPath -> resultPath.toFile().isFile())
                    .filter(Predicate.not(filePathsToIgnore::contains))
                    .forEach(resultFilePath -> {
                        String relativeResultPath = resultsDirPath.relativize(resultFilePath).toString();
                        Path baseFilePath = baseDirPath.resolve(fileMap.getOrDefault(relativeResultPath, relativeResultPath));
                        if (Files.exists(baseFilePath)) {
                            FileComparer.compareFiles(baseFilePath, resultFilePath);
                        }
                    });
        }
    }

    /**
     *
     * @param basePath the value of basePath
     * @param resultPath the value of resultPath
     */
    public static void compareFiles(Path basePath, Path resultPath) {
        switch (resultPath.toString().substring(resultPath.toString().lastIndexOf("."))) {
            case ".json":
                compareJSONFiles(basePath, resultPath);
                break;
            case ".zip":
                compareZipFiles(basePath, resultPath);
                break;
            default:
                compareTextFiles(basePath, resultPath);
                break;
            case "":
                Assert.fail("File type not recognised: '" + resultPath + "'.");
                break;
        }
    }

    /**
     *
     * @param basePath the value of basePath
     * @param resultPath the value of resultPath
     */
    public static void compareTextFiles(final Path basePath, final Path resultPath) {
        try (final BufferedReader baseReader = Files.newBufferedReader(basePath); final BufferedReader resultReader = Files.newBufferedReader(resultPath)) {
            Iterator<String> baseLineIt = baseReader.lines().iterator();
            Iterator<String> resultLineIt = resultReader.lines().iterator();
            int lineNo = 0;
            while (baseLineIt.hasNext() && resultLineIt.hasNext()) {
                final String baseLine = baseLineIt.next();
                final String resultLine = resultLineIt.next();
                Assert.assertEquals("line " + (++lineNo) + " in " + basePath + " and " + resultPath, baseLine, resultLine);
                Assert.assertEquals(baseLineIt.hasNext(), resultLineIt.hasNext());
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     *
     * @param basePath can be a zip file or an equivalent unzipped folder
     * @param resultPath the generated zip file
     */
    public static void compareZipFiles(Path basePath, Path resultPath) {
        try (final FileSystem resultFS = FileSystems.newFileSystem(URI.create("jar:" + resultPath.toUri().toString()), Collections.emptyMap())) {
            if (basePath.getFileName().toString().endsWith(".zip")) {
                try (final FileSystem baseFS = FileSystems.newFileSystem(URI.create("jar:" + basePath.toUri().toString()), Collections.emptyMap())) {
                    // Both zip files
                    final Iterable<Path> rootDirectories = baseFS.getRootDirectories();
                    compareZipFileInternal(rootDirectories, resultFS);
                }
            } else {
                // Assume the 'basePath' is the directory containing the expanded content of the zip file.
                final Iterable<Path> rootDirectories = List.of(basePath);
                compareZipFileInternal(rootDirectories, resultFS);
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        } catch (Exception ex) {
            throw ex;
        }
    }

    /**
     *
     * @param basePath the value of basePath
     * @param resultPath the value of resultPath
     */
    public static void compareJSONFiles(Path basePath, Path resultPath) {
        try (final InputStream jsonFileStreamBase = new FileInputStream(basePath.toFile());
                final InputStream jsonFileStreamResult = new FileInputStream(resultPath.toFile())) {
            JsonObject jsonBase = JSON.parse(jsonFileStreamBase);
            JsonObject jsonResult = JSON.parse(jsonFileStreamResult);
            Assert.assertEquals(jsonBase, jsonResult);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    private static void compareZipFileInternal(final Iterable<Path> rootDirectories, final FileSystem resultFS) {
        rootDirectories.forEach(root -> {
            try (final Stream<Path> basePathStream = Files.walk(root)) {
                basePathStream.forEach(subpath -> {
                    try {
                        final Path basePathInZip = subpath.toRealPath();
                        final Path baseRelativePath = root.relativize(subpath);
                        final boolean regularFileBase = !Files.isDirectory(basePathInZip) && Files.isReadable(basePathInZip);
                        if (regularFileBase) {
                            final Path resultRootDir = resultFS.getRootDirectories().iterator().next();
                            try (final Stream<Path> resultPathStream = Files.walk(resultRootDir)) {
                                final Path resultPathInZip = resultPathStream
                                        .filter(path1 -> basePathInZip.equals(root.resolve(resultRootDir.relativize(path1).toString())))
                                        .findAny().orElseThrow(() -> new NoSuchElementException("Could not find file '" + basePathInZip + "'."));
                                final boolean regularFileResult = !Files.isDirectory(resultPathInZip) && Files.isReadable(resultPathInZip);
                                System.out.println("p1:" + basePathInZip + ":" + regularFileBase + "\np2:" + resultPathInZip + ":" + regularFileResult);
                                if (regularFileResult) {
                                    compareFiles(basePathInZip, resultPathInZip);
                                }
                            }
                        }
                    } catch (IOException ex) {
                        throw new RuntimeException(ex);
                    }
                });
            } catch (IOException ex) {
                throw new RuntimeException(ex);
            }
        });
    }

}
