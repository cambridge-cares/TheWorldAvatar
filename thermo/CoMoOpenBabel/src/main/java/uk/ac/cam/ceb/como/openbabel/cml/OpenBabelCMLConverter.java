/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.cml;

import uk.ac.cam.ceb.como.openbabel.util.OpenBabelUtil;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class OpenBabelCMLConverter {

    private static File globalHome = null;
    private static boolean executableExists = false;

    static {
        executableExists = OpenBabelUtil.executableExists();
    }

    public static void setTempDir(File dir) {
        globalHome = dir;
    }

    private static File getTempDir() {
        final File home;
        if (globalHome == null) {
            home = SystemUtils.getUserHome();
        } else {
            home = globalHome;
        }
        File tempDir = new File(home, ".jbabel/converter/" + UUID.randomUUID());
        return tempDir;
    }

    public static String convert(CMLMolecule molecule, String obformat) throws OpenBabelException {
        return convert(molecule.toXML(), obformat);
    }

    public static String convert(CMLMolecule molecule, String obformat, String arguments) throws OpenBabelException {
        return convert(molecule.toXML(), obformat, arguments);
    }

    public static String convert(String xmlMolecule, String obformat) throws OpenBabelException {
        return convert(xmlMolecule, obformat, "");
    }

    public static String convert(String xmlMolecule, String obformat, String arguments) throws OpenBabelException {
        if (!executableExists) {
            throw new OpenBabelException("babel command not found on your system. If you have just"
                    + " installed Open Babel, please restart your application or redeploy your web application"
                    + " or explicitly call method executableExists().");
        }

        int exitValue = Integer.MIN_VALUE;
        File tempDir = getTempDir();
        tempDir.mkdirs();
        File input = new File(tempDir, "input.cml");
        File output = new File(tempDir, "output." + obformat);
        Map map = new HashMap();
        map.put("input", input);
        map.put("obformat", obformat);
        map.put("output", output);
        map.put("arguments", arguments);
        
        // initialize input file
        try {
            FileUtils.writeStringToFile(input, xmlMolecule, "UTF-8");
        } catch (Exception ex) {
            throw new OpenBabelException("Unable to process the CML molecule " + xmlMolecule, ex);
        }
        
        try {
            return OpenBabelConverter.convert(CommandLine.parse("babel -icml ${input} -o${obformat} ${output} ${arguments}", map), input, output, false, false);
        } catch (OpenBabelException obe) {
            throw obe;
        } finally {
            FileUtils.deleteQuietly(tempDir);
        }
    }
}
