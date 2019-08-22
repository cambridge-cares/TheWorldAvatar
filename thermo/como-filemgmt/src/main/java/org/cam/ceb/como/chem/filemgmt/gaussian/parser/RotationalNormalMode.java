/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author pb556
 */
public class RotationalNormalMode {
    public double freq = -1;
    public double redMass = -1;
    public double frcConst = -1;
    public double irInten = -1;
    public List<Integer> atomNumber = null;
    public List<VibrationalNormalMode.XYZ> pos = null;

    //public int mode = -1;
    //public String dK = null;
    
    public class XYZ {
        public double x;
        public double y;
        public double z;
    }

    public class FreqMode {
        final List<XYZ> coord = new ArrayList<XYZ>();
    }
}
