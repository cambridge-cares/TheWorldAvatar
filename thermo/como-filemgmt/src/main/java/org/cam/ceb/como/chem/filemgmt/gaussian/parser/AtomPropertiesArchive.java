/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.xmlcml.euclid.Point3;

/**
 *
 * @author pb556
 */
public class AtomPropertiesArchive {
    public double freq = -1;
    public double redMass = -1;
    public double frcConst = -1;
    public double irInten = -1;
    public HashMap<Integer, Integer> an = new HashMap<Integer, Integer>();
    public HashMap<Integer, Point3> pos = new HashMap<Integer, Point3>();
    public int mode = -1;
    public String dK = null;
}
