/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered;

import java.io.File;
import java.util.HashMap;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.tools.parser.util.DoubleTool;

/**
 *
 * @author pb556
 */

public class HPFileNameInfoDefault implements HPFileNameInfoIntf {

    protected HashMap<String, String> prop = new HashMap<>();
    protected String[] propOrder = null;
    protected String defSep = "_";
    public static final String PROP_SEP = "separator";
    public static final String PROP_BASE = "base";
    public static final String PROP_TORS_ANGLE = "torsional_angle";
    public static final String PROP_PREFIX = "prefix";
    public static final String PROP_SUFFIX = "suffix";
    public static final String PROP_FILE_EXT = "file_extension";
    public static final String PROP_DIR = "directory";
    protected Logger logger = Logger.getLogger(this.getClass());

    public HPFileNameInfoDefault() {
        propOrder = new String[6];
        propOrder[0] = PROP_DIR;
        propOrder[1] = PROP_PREFIX;
        propOrder[2] = PROP_BASE;
        propOrder[3] = PROP_SUFFIX;
        propOrder[4] = PROP_TORS_ANGLE;
        propOrder[5] = PROP_FILE_EXT;
        prop.put(PROP_SEP, defSep);
    }

    @Override
    public String get(String prop) throws Exception {
        if (this.prop.containsKey(prop)) {
            return this.prop.get(prop);
        }
        throw new Exception("Property " + prop + " is not defined!");
    }

    @Override
    public void set(String prop, Object value) throws Exception {
        if (value instanceof String) {
            this.prop.put(prop, (String) value);
            return;
        }
        throw new Exception("Invalid object type!");
    }

    @Override
    public boolean interpret(File name) throws Exception {
        return interpret(name.getAbsolutePath());
    }

    @Override
    public boolean interpret(String name) throws Exception {
        return interpret(name, propOrder);
    }

    @Override
    public boolean interpret(File name, String[] prop) throws Exception {
        return interpret(name.getPath(), propOrder);
    }

    @Override
    public boolean interpret(String name, String[] prop) throws Exception {
        if (name == null || name.length() == 0) {
            logger.error("Invalid file name has been defined!", new Exception("Invalid file name definition."));
            return false;
        }

        validateProp(prop, propOrder);

        int start = 0;
        int end = prop.length;
        String fName = name;
        String dir = "";
        String ext = "";
        int offset = 0;
        if (prop[0].compareToIgnoreCase(PROP_DIR) == 0) {
            fName = new File(name).getName();
            dir = new File(name).getParent();
            offset++;
            start++;
        }
        
        if (prop[prop.length - 1].compareToIgnoreCase(PROP_FILE_EXT) == 0) {
            ext = fName.substring(fName.lastIndexOf(".") + 1);
            fName = fName.substring(0, fName.lastIndexOf("."));
            offset++;
            end--;
        }

        String[] items = fName.split(this.prop.get(PROP_SEP));
        if (items.length + offset != prop.length) {
            logger.error("Defined file name contains less items than defined!", new Exception("Defined file name contains less items than defined!"));
            return false;
        }

        this.prop = new HashMap<>();
        if (dir.length() != 0) {
            this.prop.put(PROP_DIR, dir);
        }

        for (int i = start; i < end; i++) {
            this.prop.put(prop[i], items[i - start]);
        }
        
        if (ext.length() != 0) {
            this.prop.put(PROP_FILE_EXT, ext);
        }

        return true;
    }

    @Override
    public String generate() throws Exception {
        return generate(propOrder);
    }

    @Override
    public String generate(String[] prop) throws Exception {
        // check if all properties have been defined
        // if not all properties have been defined list the ones missing
        validateProp(prop, propOrder);
        validateData(prop, this.prop);

        // check if a sep has been defined or not!!!
        String sep = defSep;
        if (this.prop.containsKey(PROP_SEP)) {
            sep = this.prop.get(PROP_SEP);
        }

        // check if the separater causes any inconsistencies due to its 
        // presence in any other data value
        String str = "";
        for (int i = 0; i < prop.length; i++) {
            if (prop[i].compareToIgnoreCase(PROP_SEP) == 0) {
                continue;
            }
            boolean identified = false;
            for (String p : this.prop.keySet()) {
                if (prop[i].compareToIgnoreCase(p) == 0) {
                    identified = true;
                    if (!str.isEmpty()) {
                        str += sep;
                    }
                    str += this.prop.get(p);
                }
            }
            if (!identified) {
                throw new Exception("Missing property value ('" + prop[i] + "').");
            }
        }
        return str;
    }

    @Override
    public String toString() {
        try {
            return generate();
        } catch (Exception ex) {
        }
        return "";
    }

    private void validateData(String[] prop, HashMap<String, String> data) throws Exception {
        for (int i = 0; i < prop.length; i++) {
            if (prop[i].compareToIgnoreCase(PROP_SEP) == 0) {
                continue;
            }
            if (!data.containsKey(prop[i])) {
                throw new Exception("Missing property ('" + prop[i] + "') value.");
            }
        }
    }

    private void validateProp(String[] prop, String[] valid) throws Exception {
        // check only for valid property names
        for (int i = 0; i < prop.length; i++) {
            boolean identified = false;
            if (prop[i].compareToIgnoreCase(PROP_SEP) == 0) {
                continue;
            }
            for (int j = 0; j < valid.length; j++) {
                if (prop[i].compareToIgnoreCase(valid[j]) == 0) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                throw new Exception("Invalid property name ('" + prop[i] + "') definition.");
            }
        }
    }

    @Override
    public String getBase() throws Exception {
        return get(PROP_BASE);
    }

    @Override
    public Double getTorsionalAngle() throws Exception {
        return DoubleTool.parseDouble(get(PROP_TORS_ANGLE));
    }

    @Override
    public String getPrefix() throws Exception {
        return get(PROP_PREFIX);
    }

    @Override
    public String getSuffix() throws Exception {
        return get(PROP_SUFFIX);
    }

    @Override
    public String getFileExtension() throws Exception {
        return get(PROP_FILE_EXT);
    }

    @Override
    public File getDirectory() throws Exception {
        return new File(get(PROP_DIR));
    }

    @Override
    public void setBase(String base) throws Exception {
        set(PROP_BASE, base);
    }

    @Override
    public void setTorsionalAngle(String angle) throws Exception {
        set(PROP_TORS_ANGLE, angle);
    }

    @Override
    public void setPrefix(String prefix) throws Exception {
        set(PROP_PREFIX, prefix);
    }

    @Override
    public void setSuffix(String suffix) throws Exception {
        set(PROP_SUFFIX, suffix);
    }

    @Override
    public void setFileExtension(String extension) throws Exception {
        set(PROP_FILE_EXT, extension);
    }

    @Override
    public void setDirectory(File f) throws Exception {
        set(PROP_DIR, f.getAbsolutePath());
    }
}
