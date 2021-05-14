/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.extra.io;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;

/**
 * incomplete implementation for now because there is no easy way to determine the bitness of the os.
 *
 * @author pb556
 */
public class PlatformDependentResourceCopier {

    public static final String WIN32_PREFIX = "win32";
    //public static final String WIN64_PREFIX = "win64";
    public static final String LINUX32_PREFIX = "linux32";
    //public static final String LINUX64_PREFIX = "linux64";
    public static final String OSX32_PREFIX = "osx32";
    //public static final String OSX64_PREFIX = "osx64";
    private final ClassLoader classLoader;
    private final String platform_prefix;
    private final String packagePath;
    private final String subPath;

    public static final String getPlatformPrefix() {
        if (SystemUtils.IS_OS_LINUX) {
            return LINUX32_PREFIX;
        } else if (SystemUtils.IS_OS_WINDOWS) {
            return WIN32_PREFIX;
        } else if (SystemUtils.IS_OS_MAC_OSX || SystemUtils.IS_OS_MAC) {
            return OSX32_PREFIX;
        } else {
            throw new RuntimeException("Unsupported Operating System for copying the resources");
        }
    }

    public PlatformDependentResourceCopier(ClassLoader classLoader, String packageName, String subPath) {
        this.classLoader = classLoader;
        this.subPath = (subPath != null) ? trimPath(subPath) : "";
        String pkgPath = trimPath(packageName.replace('.', '/'));
        platform_prefix = getPlatformPrefix();
        packagePath = pkgPath;
    }

    public PlatformDependentResourceCopier(ClassLoader classLoader, Package pkg, String subPath) {
        this(classLoader, pkg.getName(), subPath);
    }

    public PlatformDependentResourceCopier(String packageName, String subPath) {
        this(null, packageName, subPath);
    }

    public PlatformDependentResourceCopier(Package pkg, String subPath) {
        this(null, pkg, subPath);
    }

    public PlatformDependentResourceCopier(ClassLoader classLoader, String packageName) {
        this(classLoader, packageName, null);
    }

    public PlatformDependentResourceCopier(ClassLoader classLoader, Package pkg) {
        this(classLoader, pkg, null);
    }

    public PlatformDependentResourceCopier(String packageName) {
        this(null, packageName, null);
    }

    public PlatformDependentResourceCopier(Package pkg) {
        this(null, pkg, null);
    }

    /**
     *
     * @param toDir
     * @param osRespathMap
     * @return true if os key is found
     * @throws IOException
     */
    public boolean copy(File toDir, Map<String, Collection<String>> osRespathMap) throws IOException {
        if (!osRespathMap.containsKey(platform_prefix)) {
            return false;
        } else {
            copy(toDir, osRespathMap.get(platform_prefix));
            return true;
        }
    }

    public void copy(File toDir, Collection<String> resPaths) throws IOException {
        ClassLoader sourceClassLoader = (classLoader != null) ? classLoader : Thread.currentThread().getContextClassLoader();
        for (String resPath : resPaths) {
            String absoluteResourcePath = makeAbsoluteResourcePath(resPath);
            InputStream in = sourceClassLoader.getResourceAsStream(absoluteResourcePath);
            FileUtils.copyInputStreamToFile(in, makeDestinationFile(toDir, resPath));
        }
    }

    public void copy(File toDir, String... resPaths) throws IOException {
        copy(toDir, Arrays.asList(resPaths));
    }

    private String trimPath(String path) {
        return path.trim().replaceFirst("^/", "").replaceFirst("/$", "");
    }

    private String makeAbsoluteResourcePath(String resPath) {
        return packagePath + ((subPath.isEmpty()) ? "" : "/") + subPath + "/" + platform_prefix + "/" + trimPath(resPath);
    }

    private File makeDestinationFile(File toDir, String resPath) {
        return new File(toDir, trimPath(resPath));
    }
}
