package com.cmclinnovations.fileserver;

import java.io.File;
import java.io.IOException;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.servlet.ServletException;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.Part;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author Owen Parry {@literal <oparry@cmclinnovations.com>}
 */
@WebServlet(name = "FileServer", urlPatterns = {FileServer.DOWNLOAD_URL_PATTERN, FileServer.UPLOAD_URL_PATTERN})
@MultipartConfig(
fileSizeThreshold = FileServer.ONE_MB_IN_B,
location = "/app/fs_root/",
maxFileSize = FileServer.TEN_MB_IN_B,
maxRequestSize = FileServer.ONE_HUNDRED_MB_IN_B
)
public class FileServer extends HttpServlet {

    // URL Patterns
    static final String DOWNLOAD_URL_PATTERN = "/download/*";
    static final String UPLOAD_URL_PATTERN = "/upload";

    // Some constants to set size limits
    static final int ONE_MB_IN_B = 1024 * 1024;
    static final int TEN_MB_IN_B = 10 * ONE_MB_IN_B;
    static final int ONE_HUNDRED_MB_IN_B = 100 * ONE_MB_IN_B;

    private static final Logger LOGGER = LogManager.getLogger(FileServer.class);

    private synchronized String writeFilePart(Part filePart, String subDirStr_in) {
        String fNameRequested = filePart.getSubmittedFileName();
        String subDirStr = subDirStr_in == null ? "." : subDirStr_in;

        String fName = fNameRequested;
        Path destPath = Paths.get(subDirStr).resolve(fName);

        // If requested filename exists, append a suffix generated from the current sytem time
        if (Files.exists(destPath)) {
            String fNameBase = FilenameUtils.getBaseName(fNameRequested);
            String ext = FilenameUtils.getExtension(fNameRequested);
            String suffix = Long.toString(System.currentTimeMillis());

            // Append suffix, allowing for paths with no extension
            fName = (ext.length() > 0) ? fNameBase + "_" + suffix + "." + ext : fNameBase + suffix;
            destPath = Paths.get(subDirStr).resolve(fName);
        }

        try {
            // Create the subdirectory, if necessary
            File subDir = new File(subDirStr);
            if (!subDir.exists()) {
                //LOGGER.info("Creating subdirectory " + subDirStr);
                boolean dirCreated = subDir.mkdirs();
                if (!dirCreated) {
                    LOGGER.error("Failed to created sub-directory " + subDirStr);
                    return null;
                }
            }

            // Write the file
            filePart.write(destPath.toString());
        } catch (IOException ex) {
            LOGGER.error("writeFilePart: IOException when trying to write file " + destPath, ex);
            return null;
        }

        /*
         * Wait for a millisecond to ensure the next call of this function never generates the same
         * fallback suffix.
         */
        try {
            wait(1);
        } catch (InterruptedException ex) {
            LOGGER.error("writeFilePart: (Thread)InterruptedException whilst pausing at the end of the method", ex);
        }
        return fName;
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        // Reject GET requests to upload URL
        String servletPath = request.getServletPath();
        if (servletPath.contentEquals(UPLOAD_URL_PATTERN)) {
            LOGGER.error("Rejecting GET request to " + UPLOAD_URL_PATTERN);
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }

        // Extract the path following the servlet URL, minus the leading slash
        String fPath = URLDecoder.decode(request.getPathInfo().substring(1), "UTF-8");

        LOGGER.info("Received a request for file " + fPath);
        File file = new File(fPath);
        if (file.exists()) {
            response.setHeader("Content-Type", getServletContext().getMimeType(fPath));
            response.setHeader("Content-Length", String.valueOf(file.length()));
            response.setHeader("Content-Disposition", "inline; filename=\"" + file.getName() + "\"");
            Files.copy(file.toPath(), response.getOutputStream());
        } else {
            LOGGER.error("No file found at " + fPath);
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        // Reject POST requests to download URL
        String servletPath = request.getServletPath();
        final String downloadPath = DOWNLOAD_URL_PATTERN.substring(0, servletPath.length());
        if (servletPath.contentEquals(downloadPath)) {
            LOGGER.error("Rejecting POST request to " + downloadPath);
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }

        // Extract subdirectory from header (null if none was supplied)
        String subDir = request.getHeader("subDir");

        // Initially set success status code, overwrite later if there's an error
        response.setStatus(HttpServletResponse.SC_OK);

        request.getParts().stream()
        .filter(part -> part.getSize() > 0)
        .takeWhile(part -> response.getStatus() == HttpServletResponse.SC_OK)
        .forEach(part -> {
            String fNameWritten = writeFilePart(part, subDir);

            // If writing failed, set error code (breaks stream forEach)
            if (fNameWritten == null) {
                response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            } else {
                LOGGER.info("Wrote file to path " + fNameWritten);
                response.setHeader(part.getName(), fNameWritten);
            }
        });
    }
}
