package uk.ac.cam.ceb.como.extra.io;

import java.io.File;
import java.io.IOException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;

/**
 *
 * @author pb556
 */
public final class FileTools {

    /**
     * Gets the full path with base name from a full filename, which is the prefix + path + basename.
     *
     * <pre>
     * C:\a\b\c.txt --> C:\a\b\c
     * ~/a/b/c.txt  --> ~/a/b/c
     * a.txt        --> "a"
     * a/b/c        --> a/b/c
     * a/b/c/       --> a/b/c/
     * C:           --> C:
     * C:\          --> C:\
     * ~            --> ~/
     * ~/           --> ~/
     * ~user        --> ~user/
     * ~user/       --> ~user/
     * </pre> <p> The output will be the same irrespective of the machine that the code is running on.
     *
     * @param filename the filename to query, null may throw a NullPointException
     * @return the path of the file, an empty string if none exists, null if invalid
     */
    public static String getFullPathWithBaseName(String filename) {
        return FilenameUtils.getFullPath(filename) + FilenameUtils.getBaseName(filename);
    }

    /**
     * Force making a parent directory for file. If directory exists after making, it returns true. Otherwise, it throws
     * an IOException.
     *
     * @param file directory file.
     * @return true if successfully deleted a directory, else false;
     * @throws IOException if io error occurs.
     */
    public static boolean forceMakeParentDir(File file) throws IOException {
        try {
            File parent = file.getParentFile();
            if (parent != null) {
                FileUtils.forceMkdir(parent);
                if (parent.exists()) {
                    return true;
                } else {
                    throw new IOException("Directory " + file.getCanonicalPath() + " does not exist after succesful creation.");
                }
            }
            return true;
        } catch (IOException ex) {
            throw ex;
        }
    }

    /**
     * Force making a parent directory for file quietly. If directory exists after making, it returns true. Otherwise,
     * it returns false.
     *
     * @param file directory file.
     * @return true if successfully deleted a directory, else false;
     */
    public static boolean forceMakeParentDirQuietly(File file) {
        try {
            return forceMakeParentDir(file);
        } catch (IOException ex) {
            return false;
        }
    }

    /**
     * Delete the content of the folder without removing the folder itself.
     *
     * @param dir
     * @return true if all contents were deleted, otherwise false. if the folder not exist, it return false.
     */
    public static boolean deleteContentQuietly(File dir) {
        File[] dirs = dir.listFiles();
        boolean deleted = true;
        if (dirs != null) {
            for (File d : dirs) {
                deleted = deleted && FileUtils.deleteQuietly(d);
            }
        } else {
            deleted = false;
        }
        return deleted;
    }

    /**
     * convert the given input from dos text format to output unix text format
     * @param input
     * @param output
     * @throws IOException
     */
    public static void dos2unix(File input, File output) throws IOException {
        String str = FileUtils.readFileToString(input, "UTF-8");
        FileUtils.writeStringToFile(output, str.replaceAll("\r", ""), "UTF-8");
    }

    /**
     * convert the given file from dos text format to unix text format
     * @param file
     * @throws IOException
     */
    public static void dos2unix(File file) throws IOException {
        dos2unix(file, file);
    }
}
