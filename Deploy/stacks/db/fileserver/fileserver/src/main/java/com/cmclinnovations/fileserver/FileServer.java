package com.cmclinnovations.fileserver;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.Part;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author Owen Parry {@literal <oparry@cmclinnovations.com>}
 */
public class FileServer extends HttpServlet {

    // Content type prefixes
    private static final String MULTIPART_FORM_TYPE_PREFIX = "multipart/form-data";
    private static final String MULTIPART_MIXED_TYPE_PREFIX = "multipart/mixed";

    private static final Logger LOGGER = LogManager.getLogger(FileServer.class);

    private synchronized String writeFilePart(Part filePart, String pathStrIn) throws IOException {
        String fNameRequested;
        Path subDir = Paths.get(pathStrIn);
        if (pathStrIn.isBlank() || pathStrIn.endsWith("/") || Files.isDirectory(subDir)) {
            // Path provided was a directory so just use the name attached to the file part
            fNameRequested = filePart.getSubmittedFileName();
        } else {
            // Path provided was not a directory so use it instead of the name attached to
            // the file part
            fNameRequested = subDir.getFileName().toString();
            subDir = subDir.getParent();
        }

        String fName = fNameRequested;

        Path destPath = subDir.resolve(fName);

        // If requested filename exists, append a suffix generated from the current
        // sytem time
        if (Files.exists(destPath)) {
            String fNameBase = FilenameUtils.getBaseName(fNameRequested);
            String ext = FilenameUtils.getExtension(fNameRequested);
            String suffix = Long.toString(System.currentTimeMillis());

            // Append suffix, allowing for paths with no extension
            fName = (ext.length() > 0) ? fNameBase + "_" + suffix + "." + ext : fNameBase + suffix;
            destPath = subDir.resolve(fName);
        }

        try {
            // Create the subdirectory, if necessary
            if (!Files.exists(subDir)) {
                // LOGGER.info("Creating subdirectory " + subDirStr);
                try {
                    Files.createDirectories(subDir);
                } catch (IOException ex) {
                    String errorMessage = "Failed to created sub-directory '" + subDir + "'.";
                    throw new IOException(errorMessage, ex);
                }
            }

            // Write the file
            filePart.write(destPath.toString());
        } catch (IOException ex) {
            String errorMessage = "writeFilePart: IOException when trying to write file '" + subDir + "'.";
            throw new IOException(errorMessage, ex);
        }

        /*
         * Wait for a millisecond to ensure the next call of this function never
         * generates the same
         * fallback suffix.
         */
        try {
            wait(1);
        } catch (InterruptedException ex) {
            LOGGER.error("writeFilePart: (Thread)InterruptedException whilst pausing at the end of the method", ex);
        }
        return destPath.toString();
    }

    @Override
    protected void doDelete(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        logRequest(request);

        // Extract the path following the servlet URL, minus the leading slash
        Path path = Paths.get(extractPath(request.getPathInfo()));

        if (!Files.exists(path)) {
            respondWithError(response, HttpServletResponse.SC_NOT_FOUND,
                    "No file or directory found at '" + path + "'.");
        } else if (Files.isRegularFile(path)) {
            LOGGER.info("Received request to DELETE file '{}'.", path);
            try {
                boolean fileDeleted = Files.deleteIfExists(path);
                if (!fileDeleted) {
                    respondWithError(response, HttpServletResponse.SC_NOT_FOUND,
                            "No file found at '" + path + "'.");
                } else {
                    LOGGER.info("Deleted file at '{}'.", path);
                    response.setStatus(HttpServletResponse.SC_OK);
                }
            } catch (IOException ex) {
                respondWithError(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                        "IOException thrown while trying to delete file '" + path + "'.");
            }
        } else if (Files.isDirectory(path)) {
            LOGGER.info("Received request to DELETE directory '{}'.", path);
            try {
                FileUtils.deleteDirectory(path.toFile());
                LOGGER.info("Deleted directory at '{}'.", path);
                response.setStatus(HttpServletResponse.SC_OK);
            } catch (IOException ex) {
                respondWithError(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                        "IOException thrown while trying to delete directory '" + path + "'.");
            }
        }
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        logRequest(request);

        // Extract the path following the servlet URL, minus the leading slash
        Path fPath = Paths.get(extractPath(request.getPathInfo()));

        LOGGER.info("Received a request to GET file " + fPath);
        if (Files.isRegularFile(fPath)) {
            response.setHeader("Content-Type", getServletContext().getMimeType(fPath.toString()));
            response.setHeader("Content-Length", String.valueOf(Files.size(fPath)));
            response.setHeader("Content-Disposition", "inline; filename=\"" + fPath.getFileName() + "\"");
            Files.copy(fPath, response.getOutputStream());
            response.setStatus(HttpServletResponse.SC_OK);
        } else if (Files.isDirectory(fPath)) {
            respondWithError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "The path '" + fPath + "' points to a directory not a file.");
        } else {
            respondWithError(response, HttpServletResponse.SC_NOT_FOUND,
                    "No file found at '" + fPath + "'.");
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        logRequest(request);

        // Reject requests with the wrong Content-Type
        String contentType = request.getContentType();
        if (contentType == null || !contentType.startsWith(MULTIPART_FORM_TYPE_PREFIX)
                && !contentType.contentEquals(MULTIPART_MIXED_TYPE_PREFIX)) {
            respondWithError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "Rejecting POST request with invalid content type '" + contentType + "'.");
            return;
        }

        // Extract subdirectory from header (null if none was supplied)
        String subDir = extractPath(request.getPathInfo());

        if (!subDir.isBlank() && !subDir.endsWith("/") && request.getParts().size() > 1) {
            respondWithError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "Rejecting POST request with explicit filename when multiple files specified.");
            return;
        }

        // Initially set success status code, overwrite later if there's an error
        response.setStatus(HttpServletResponse.SC_OK);

        URL serverURL = new URL(request.getScheme(), request.getServerName(), request.getServerPort(),
                request.getContextPath());
        request.getParts().stream()
                .filter(part -> part.getSize() > 0)
                .takeWhile(part -> response.getStatus() == HttpServletResponse.SC_OK)
                .forEach(part -> {
                    try {
                        String fileNameWritten = writeFilePart(part, subDir);
                        LOGGER.info("Wrote file to path '{}'.", fileNameWritten);
                        response.setHeader(part.getName(), serverURL + "/" + fileNameWritten);
                    } catch (IOException ex) {
                        // If writing failed, set error code (breaks stream forEach)
                        respondWithError(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                                "Failed to write part '" + part.getName() + "' .");
                    }
                });
    }

    private void logRequest(HttpServletRequest request) {
        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("{} request URI = '{}'\n\theaders = '{}'", request.getMethod(), request.getRequestURI(),
                    Collections.list(request.getHeaderNames()).stream()
                            .collect(Collectors.toMap(Function.identity(), request::getHeader)).entrySet().stream()
                            .map(e -> e.getKey() + " = " + e.getValue())
                            .collect(Collectors.joining("\n\t\t")));
        }
    }

    private String extractPath(String pathInfo) throws UnsupportedEncodingException {
        return URLDecoder.decode(
                (null == pathInfo) ? "" : pathInfo.replaceFirst("^/", ""),
                "UTF-8");
    }

    private void respondWithError(HttpServletResponse response, int errorStatus, String errorMessage) {
        LOGGER.error(errorMessage);
        try {
            response.sendError(errorStatus, errorMessage);
        } catch (IOException ex) {
            response.setStatus(errorStatus);
        }
    }

}