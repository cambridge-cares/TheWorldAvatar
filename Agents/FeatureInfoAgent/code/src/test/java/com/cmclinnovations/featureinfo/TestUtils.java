package com.cmclinnovations.featureinfo;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;

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

    /**
     * Mocks a new ConfigStore instance based on the content of the input file.
     * 
     * @param configFile path to configuration file.
     * 
     * @return spied config store.
     * 
     * @throws Exception if configuration file cannot be read.
     */
    public static ConfigStore mockConfig(Path configFile) throws Exception {
        // Create store instance (skipping stack integration)
        ConfigStore configStore = new ConfigStore(configFile.toString());
        configStore.loadDetails(false);

        // Setup mocking for stack endpoints
        ConfigStore spiedConfig = Mockito.spy(configStore);

        Mockito.when(
            spiedConfig.getStackEndpoints(
                ArgumentMatchers.same(StackEndpointType.ONTOP)
            )
        ).thenReturn(
            new ArrayList<>(Arrays.asList(
                new StackEndpoint("https://test-stack/ontop", null, null, StackEndpointType.ONTOP)
            ))
        );

        Mockito.when(
            spiedConfig.getStackEndpoints(
                ArgumentMatchers.same(StackEndpointType.POSTGRES)
            )
        ).thenReturn(
            new ArrayList<>(Arrays.asList(
                new StackEndpoint("https://test-stack/postgres", null, null, StackEndpointType.POSTGRES)
            ))
        );

        Mockito.when(
            spiedConfig.getStackEndpoints(
                ArgumentMatchers.same(StackEndpointType.BLAZEGRAPH)
            )
        ).thenReturn(
            new ArrayList<>(Arrays.asList(
                new StackEndpoint("https://test-stack/blazegraph-one", null, null, StackEndpointType.BLAZEGRAPH),
                new StackEndpoint("https://test-stack/blazegraph-two", null, null, StackEndpointType.BLAZEGRAPH)
            ))
        );

        return spiedConfig;
    }

    /**
     * Mock a HTTP response object.
     * 
     * @return mocked HTTP response.
     */
    public static HttpServletResponse mockResponse() throws Exception {
        HttpServletResponse httpResponse = mock(HttpServletResponse.class);
        StringWriter strWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(strWriter);
        when(httpResponse.getWriter()).thenReturn(printWriter);
        return httpResponse;
    }

}
// End of class.