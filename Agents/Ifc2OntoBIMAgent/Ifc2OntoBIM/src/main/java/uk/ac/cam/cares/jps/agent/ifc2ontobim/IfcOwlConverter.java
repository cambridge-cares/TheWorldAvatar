package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * A converter class that converts the IFC model into a TTL triples with the IfcOwl ontology as well as list of TTL file outputs.
 *
 * @author qhouyee
 */
public class IfcOwlConverter {
    private String baseURI;
    private String dirPath;
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String TARGET_DIR_ERROR_MSG = "Failed to access target directory at: ";

    /**
     * Standard constructor to set the target directory containing Ifc files and/or the base URI.
     * Will not overwrite default parameters if no argument is passed in the command line
     *
     * @param arguments Command line arguments with flags --baseURI AND/OR --dir flags
     */
    public IfcOwlConverter(String[] arguments) {
        // Set the default parameters if no arguments are passed
        setBaseURI("http://www.theworldavatar.com/ifc/resources_" + UUID.randomUUID() + "/");
        setDirPath(Paths.get(System.getProperty("user.dir") + "/data/").toString());

        // When there are arguments passed, set the relevant flags accordingly
        if (arguments != null || arguments.length == 0) {
            ArrayList<String> argsList = new ArrayList<>(Arrays.asList(arguments));
            // If this flag exists in the command line input, set the baseURI to the input URI
            int uriIndex = argsList.indexOf("--baseURI");
            if (uriIndex > -1) {
                setBaseURI(arguments[uriIndex + 1]);
                LOGGER.debug("Detected Base URI flag passed in arguments. Default Base URI will be overwritten.");
            }
            // If this flag exists in the command line input, set the target directory to the input path
            int dirIndex = argsList.indexOf("--dir");
            if (dirIndex > -1) {
                setDirPath(arguments[dirIndex + 1]);
                LOGGER.debug("Detected target directory flag passed in arguments. Default target directory will be overwritten.");
            }
        }
        LOGGER.info("Target directory containing IFC files to convert is: " + getDirPath());
        LOGGER.info("Base URI for IfcOwl instances has been set to: " + getBaseURI());
    }

    /**
     * Converts the IFC file into a TTL file using the IfcOwl conversion tool, IFC2RDF.
     * Although there is a Maven library, their code has bugs importing the latest Jena. It is better to fall back on the jar application instead.
     */
    public void parse2TTL() throws Exception {
        String resourcePath = Paths.get(System.getProperty("user.dir") + "/IFCtoRDF-0.4-shaded.jar").toString();
        String[] command = {"java", "-jar", resourcePath, "--baseURI", this.getBaseURI(), "--dir", this.getDirPath()};
        LOGGER.info("Executing conversion to IfcOwl instances...");
        Process pr = Runtime.getRuntime().exec(command);
        pr.waitFor();
    }

    /**
     * List all .ttl files in the target directory and store them in a Set
     *
     * @return A set object containing all the ttl files in the target directory
     */
    public Set<String> listTTLFiles() {
        Set<String> fileSet = new HashSet<>();
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(Paths.get(this.getDirPath()))) {
            for (Path path : stream) {
                if (!Files.isDirectory(path) &&
                        path.getFileName().toString().substring(
                                path.getFileName().toString().lastIndexOf('.') + 1).equals("ttl")) {
                    // Add only ttl files to the list
                    fileSet.add(path.toString());
                }
            }
        } catch (IOException e) {
            LOGGER.fatal(TARGET_DIR_ERROR_MSG + this.getDirPath() + "\nRead error message for more details: " + e);
            throw new JPSRuntimeException(TARGET_DIR_ERROR_MSG + this.getDirPath() + "\nRead error message for more details: " + e);
        }
        return fileSet;
    }

    public String getDirPath() {
        return dirPath;
    }

    public void setDirPath(String dirPath) {
        this.dirPath = dirPath;
    }

    public String getBaseURI() {
        return baseURI;
    }

    public void setBaseURI(String baseURI) {
        this.baseURI = baseURI;
    }
}
