/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered;

/**
 *
 * @author pb556
 */
public class HPFileNameInfoExtended extends HPFileNameInfoDefault {
    
    public static final String PROP_CALC_TYPE = "calculation_type";
    public static final String PROP_ROTOR = "rotor";
    public static final String PROP_INDEX = "index";

    public HPFileNameInfoExtended() {
        super();
        propOrder = new String[7];
        propOrder[0] = PROP_DIR;
        propOrder[1] = PROP_CALC_TYPE;
        propOrder[2] = PROP_BASE;
        propOrder[3] = PROP_ROTOR;
        propOrder[4] = PROP_INDEX;
        propOrder[5] = PROP_TORS_ANGLE;
        propOrder[6] = PROP_FILE_EXT;
        prop.put(PROP_SEP, defSep);
    }
}
