package uk.ac.cam.cares.jps.base.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.StringTokenizer;

import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class AgentLocator {

	private static String JPS_CONFIG_ENVIRONMENT_TEST_KEY = "test";
    private static AgentLocator instance = null;

    private static Logger logger = LoggerFactory.getLogger(AgentLocator.class);
    private static final String[] SUBDIRECTORIES_FOR_COMPILED_CLASSES = new String[]{
            "/WEB-INF/classes", "\\WEB-INF\\classes", "/bin", "\\bin", "/build/classes", "\\build\\classes",
            "target/classes", "\\target\\classes", "target/test-classes", "\\target\\test-classes"
    };
    private String jpsBaseDirectory = null;
    private String url = null;

    private AgentLocator() {
    }

    private static synchronized AgentLocator getSingleton() {
        if (instance == null) {
            instance = new AgentLocator();
            instance.init();
        }
        return instance;
    }

    /**
     * Loads the property files. There is only one source for the property files, namely the config directory
     * within the deployed JPS_BASE app.
     */
    private void init() {

        String path = getCurrentJpsAppDirectory(this);
        jpsBaseDirectory = createJpsBaseDirectory(path);
        logger.info("JPS_BASE directory = " + jpsBaseDirectory);
    }
    /** previously used to determine the location of JPS BASE CONFIG as part of JPS BASE webapp
     * Now config file is stored in JPS BASE LIB/src/main/resources
     *  
     * @param currentJpsAppDirectory
     * @return
     */
    public static String createJpsBaseDirectory(String currentJpsAppDirectory) {
    	return "/";
//        String result = null;
//        
//        String path = currentJpsAppDirectory.replace("\\", "/");
//        int index = path.lastIndexOf("/");
//        String firstPart = path.substring(0, index);
//        System.out.println("jps base dir first part = " + firstPart);
//        File dir = new File(firstPart);
//
//        // there might be several webapps with name of form JPS_BASE##x.y.z
//        // find the one with the largest version x.y.z
//        String highestVersion = null;
//        for (File current : dir.listFiles()) {
//            if (!current.isDirectory()) {
//                continue;
//            }
//            path = current.getAbsolutePath().replace("\\", "/");
//            index = path.lastIndexOf("/");
//            String secondPart = path.substring(index + 1);
//            System.out.println("jps base dir second part = " + secondPart);
//            if (secondPart.startsWith("JPS_BASE")) {
//                String version = null;
//                index = secondPart.lastIndexOf("##");
//                if (index >= 0) {
//                    version = secondPart.substring(index + 2);
//                }
//                if (result == null || isVersionLargerThan(version, highestVersion)) {
//                    result = path;
//                    highestVersion = version;
//                    System.out.println("jps base dir current path candidate = " + path);
//                }
//            }
//        }
//        if (result == null) {
//            result = path + "/../../../JPS_BASE";
//            if (!Files.isDirectory(Paths.get(result))) {
//                result = path + "/../../JPS_BASE";
//            }
//        }
//        return result;
    }

    public static boolean isVersionLargerThan(String v1, String v2) {

        if (v1 == null) {
            return false;
        }
        if (v2 == null) {
            return true;
        }
        StringTokenizer t1 = new StringTokenizer(v1, ".");
        StringTokenizer t2 = new StringTokenizer(v2, ".");
        while (t1.hasMoreTokens()) {

            if (!t2.hasMoreTokens()) {
                return true;
            }

            int i1 = Integer.valueOf(t1.nextToken());
            int i2 = Integer.valueOf(t2.nextToken());
            if (i1 < i2) {
                return false;
            }
            if (i1 > i2) {
                return true;
            }
        }

        return false;
    }

    public static String getCurrentJpsAppDirectory(Object thisObject) {
        String path;
        URL appURL = thisObject.getClass().getClassLoader().getResource("");
        String classDir = appURL.getPath();
        try {
            path = Paths.get(appURL.toURI()).toFile().getPath();
        } catch (URISyntaxException e) {
            throw new JPSRuntimeException(e);
        }

        int index = -1;
        for (String current : SUBDIRECTORIES_FOR_COMPILED_CLASSES) {
            index = path.lastIndexOf(current);
            if (index > +0) {
                break;
            }
        }

        if (index == -1) {
            String message = "current JPS app directory was not found, class directory = " + classDir;
            logger.error(message);
            throw new JPSRuntimeException(message);
        }
        path = path.substring(0, index);
        logger.info("current JPS app directory = " + path + " , class directory = " + classDir);
        return path;
    }

    public static String getJPSBaseDirectory() {
        return getSingleton().jpsBaseDirectory;
    }

    /**
     * This method is used to determine which enviroment JPS agents should operate in:
     * development/testing or production.
     *
     * @return Boolean testMode
     */
    public static boolean isJPSRunningForTest() {
        Boolean testMode = false;
        testMode = Boolean.valueOf(KeyValueMap.getInstance().get(JPS_CONFIG_ENVIRONMENT_TEST_KEY));
        return testMode;
    }

    public static String getAbsolutePath(String keyForRelativePath, Object thisObject) {
        String relativePath = getProperty(keyForRelativePath);
        return getCurrentJpsAppDirectory(thisObject) + "/" + relativePath;
    }

    /**
     * @param pythonScriptName (including package name followed by script name and .py extension, e.g. caresjpsadmsinputs/ADMSGeoJsonGetter.py)
     * @return
     */
    //TODO-AE replace original methods after Janusz checked that this method is working
    public static String getNewPathToPythonScript(String pythonScriptName, Object thisObject) {
        String relativePath = getProperty("reldir.python");
        return Paths.get(getCurrentJpsAppDirectory(thisObject), relativePath, pythonScriptName).toString();
    }

    public static String getPathToWorkingDir(Object thisObject) {
        String relativePath = getProperty("reldir.workingdir");
        return getCurrentJpsAppDirectory(thisObject) + "/" + relativePath;
    }

    public static String getPathToJpsWorkingDir() {
        return getProperty("absdir.jpsdata.workingdir");
    }

    /**
     * If there is a test property file with the key then its value is returned.
     * Otherwise the value specified in the application property file or null is
     * returned.
     *
     * @param key
     * @return
     */
    public static String getProperty(String key) {

        return KeyValueMap.getInstance().get(key);
    }

    public static String callAgent(String agentKey) throws ClientProtocolException, IOException {
        return getSingleton().callAgentInternally(agentKey);
    }

    private String callAgentInternally(String agentKey) throws ClientProtocolException, IOException {
        String combinedUrl = getUrl() + AgentLocator.getProperty(agentKey);
        logger.info("calling agent " + combinedUrl);
        HttpUriRequest request = new HttpGet(combinedUrl);
        HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
        String response = EntityUtils.toString(httpResponse.getEntity());
        return response;
    }

    private String getUrl() {
        if (url == null) {
            url = "http://" + getProperty("host") + ":" + getProperty("port");
            logger.info("created url from properties: " + url);
        }

        return url;
    }

    public static boolean isJPSRunningAtCMCL() {
        Boolean testMode = false;
        testMode = Boolean.valueOf(KeyValueMap.getInstance().get("cmcl"));
        return testMode;
    }
}
