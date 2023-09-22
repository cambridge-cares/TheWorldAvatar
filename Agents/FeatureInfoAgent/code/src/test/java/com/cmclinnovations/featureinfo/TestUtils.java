package com.cmclinnovations.featureinfo;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.apache.commons.lang.StringUtils;

/**
 * Misc utilities used for automated tests.
 * 
 * File copying methods taken from user jabber's answer at
 * https://stackoverflow.com/questions/1386809/copy-directory-from-a-jar-file
 */
public final class TestUtils {

    /**
     * Copies the input file (from test resources) to the destination directory.
     * 
     * @param toCopy file to copy.
     * @param destDir destination directory.
     * 
     * @return success flag.
     */
    public static boolean copyFilesRecusively(File toCopy, File destDir) {
        assert destDir.isDirectory();

        if (!toCopy.isDirectory()) {
            return TestUtils.copyFile(toCopy, new File(destDir, toCopy.getName()));
        } else {
            File newDestDir = new File(destDir, toCopy.getName());
            if (!newDestDir.exists() && !newDestDir.mkdir()) {
                return false;
            }
            for (File child : toCopy.listFiles()) {
                if (!TestUtils.copyFilesRecusively(child, newDestDir)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Copy a file out of the resources to the destination.
     * 
     * @param toCopy file to copy.
     * @param destDir destination file.
     * 
     * @return success flag.
     */
    private static boolean copyFile(File toCopy, File destFile) {
        try {
            return TestUtils.copyStream(new FileInputStream(toCopy), new FileOutputStream(destFile));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Recursively copy data from a JAR file to the destination directory.
     * 
     * @param destDir destination directory.
     * @param jarConnection JAR connection.
     * 
     * @return success state.
     * 
     * @throws IOException if JAR data cannot be read or directory cannot be written to.
     */
    private static boolean copyJarResourcesRecursively(File destDir, JarURLConnection jarConnection) throws IOException {
        JarFile jarFile = jarConnection.getJarFile();

        for (Enumeration<JarEntry> e = jarFile.entries(); e.hasMoreElements();) {
            JarEntry entry = e.nextElement();
            if (entry.getName().startsWith(jarConnection.getEntryName())) {
                String filename = StringUtils.removeStart(entry.getName(), jarConnection.getEntryName());

                File f = new File(destDir, filename);
                if (!entry.isDirectory()) {
                    InputStream entryInputStream = jarFile.getInputStream(entry);
                    if (!TestUtils.copyStream(entryInputStream, f)) {
                        return false;
                    }
                    entryInputStream.close();
                } else {
                    if (!TestUtils.ensureDirectoryExists(f)) {
                        throw new IOException("Could not create directory: " + f.getAbsolutePath());
                    }
                }
            }
        }
        return true;
    }

    /**
     * Copy data from the input URL to the output destination recursively.
     * 
     * @param originUrl source URL.
     * @param destination target file.
     * 
     * @return success state.
     */
    public static boolean copyResourcesRecursively(URL originUrl, File destination) {
        try {
            URLConnection urlConnection = originUrl.openConnection();
            if (urlConnection instanceof JarURLConnection) {
                return TestUtils.copyJarResourcesRecursively(destination, (JarURLConnection) urlConnection);
            } else {
                return TestUtils.copyFilesRecusively(new File(originUrl.getPath()), destination);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Copy the input stream to the output file.
     * 
     * @param is input stream.
     * @param f output file.
     * 
     * @return success state.
     */
    private static boolean copyStream(InputStream is, File f) {
        try {
            return TestUtils.copyStream(is, new FileOutputStream(f));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Copies data from the input stream to the output stream.
     * 
     * @param is input stream.
     * @param os output stream.
     * 
     * @return success state.
     */
    private static boolean copyStream(InputStream is, OutputStream os) {
        try {
            byte[] buf = new byte[1024];
            int len = 0;
            while ((len = is.read(buf)) > 0) {
                os.write(buf, 0, len);
            }
            is.close();
            os.close();
            return true;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Return true if file exists.
     * 
     * @param f file.
     * 
     * @return existance.
     */
    private static boolean ensureDirectoryExists(File f) {
        return f.exists() || f.mkdir();
    }

}
// End of class.