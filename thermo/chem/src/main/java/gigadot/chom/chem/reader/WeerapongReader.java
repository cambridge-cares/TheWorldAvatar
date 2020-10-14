package gigadot.chom.chem.reader;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import nu.xom.Attribute;
import nu.xom.Document;
import nu.xom.Element;
import gigadot.chom.chem.reader.wml.wmlReaderBase;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.writer.WeerapongWriter;
import gigatools.extra.xom.XMLTools;
import gigatools.extra.xom.XMLException;
import java.io.File;

/**
 * @deprecated
 * @author Weerapong Phadungsukanan
 */
public class WeerapongReader extends ChemFileReader {

    private static boolean upgrade = false;

    /**
     * Set all the instance of this class to rewrite the WML output with the latest
     * format. This option is false by default. Changing this option will permanently
     * change the setting for all instance of this class including those that will
     * be created in the future. Use with care.
     *
     * @param upgrade true if rewriting on read.
     */
    public static void setUpgradeFileAfterReading(boolean upgrade) {
        WeerapongReader.upgrade = upgrade;
    }

    /**
     * Get the status of the rewriting-on-reading option.
     *
     * @see #setUpgradeFileAfterReading(boolean)
     * @return true if rewriting on read, else false.
     */
    public boolean getUpgradeFileAfterReading() {
        return upgrade;
    }

    /**
     * Read and parse molecular quantum calculation output from Weerapong XML and
     * store information in given JChemDocument object. There is an option
     * {@link #setUpgradeFileAfterReading(boolean)} to force the WML file to be
     * upgraded on reading.
     *
     * @param compound
     * @see #read(String)
     * @see #setUpgradeFileAfterReading(boolean)
     */
    @Override
    public void read(String file, Compound compound) throws IOException {
        try {
            Document doc = XMLTools.readXML(new File(file));
            Element root = doc.getRootElement();
            Attribute ver_att = root.getAttribute("version");
            String version = null;
            wmlReaderBase wmlReader = null;
            if (ver_att != null) {
                version = ver_att.getValue();
            }
            wmlReader = getwmlReader(version);
            wmlReader.read(doc, compound);

            //if (upgrade && !(version.trim().equals(WeerapongWriter.version))) {
            if (upgrade) {
                WeerapongWriter wmlWriter = new WeerapongWriter();
                wmlWriter.write(file, compound);
            }
        } catch (XMLException ex) {
            throw new IOException(ex.getMessage());
        }
    }

    private String convertVersionToClassName(String version) {
        Pattern p = Pattern.compile("^(\\d*)\\.(\\d*)\\.(\\d*)\\.(\\d*)$");
        Matcher m = p.matcher(version.trim());
        if (m.find()) {
            return "gigadot.chom.chem.reader.wml.wmlReaderV_" + m.group(1) + "_" + m.group(2) + "_" + m.group(3) + "_" + m.group(4);
        } else {
            logger.error("WML version : " + version + " not supported. Default version 0.1.0.1 is used.");
            return "gigadot.chom.chem.reader.wml.wmlReaderV_0_1_0_1";
        }
    }

    private wmlReaderBase getwmlReader(String version) {
        if (version == null) {
            version = "default";
        }
        String className = convertVersionToClassName(version);
        logger.info("WeerapongReader version : " + className);
        try {
            return (wmlReaderBase) java.lang.Class.forName(className).newInstance();
        } catch (InstantiationException ex) {
            logger.error("Cannot initiate instance of " + className, ex);
            return null;
        } catch (IllegalAccessException ex) {
            logger.error("Illegal access of " + className, ex);
            return null;
        } catch (ClassNotFoundException ex) {
            logger.error("Cannot find a class " + className, ex);
            return null;
        }
    }
}
