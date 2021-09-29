package uk.ac.cam.cares.jps.agent.fileserver;

import java.io.File;
import java.io.IOException;
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
@WebServlet(name = "FileUploadServlet", urlPatterns = {"/upload"})
@MultipartConfig(
fileSizeThreshold = FileUploadServlet.ONE_MB_IN_B,
location = "/app/fs_root/",
maxFileSize = FileUploadServlet.TEN_MB_IN_B,
maxRequestSize = FileUploadServlet.ONE_HUNDRED_MB_IN_B
)
public class FileUploadServlet extends HttpServlet {

    // Some constants to set size limits
    static final int ONE_MB_IN_B = 1024 * 1024;
    static final int TEN_MB_IN_B = 10 * ONE_MB_IN_B;
    static final int ONE_HUNDRED_MB_IN_B = 100 * ONE_MB_IN_B;

    private static final Logger LOGGER = LogManager.getLogger(FileUploadServlet.class);

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
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
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
