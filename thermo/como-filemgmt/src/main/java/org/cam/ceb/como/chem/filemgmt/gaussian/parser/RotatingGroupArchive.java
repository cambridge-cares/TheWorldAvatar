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
public class RotatingGroupArchive {    
    public List<Double> iterationCart = new ArrayList<Double>();
    public List<Double> iterationInt = new ArrayList<Double>();
    public List<Double> iteration = new ArrayList<Double>();
    public List<Integer> rotatingGroup = new ArrayList<Integer>();
    
    public double estimatedErr = -1;
    public int atomA = -1;
    public int atomB = -1;
    
    public int mode = -1;
    
    public int periodicity = -1;
    public int symmetrynum = -1;
    public int multiplicity = -1;
    
    public double[] frequencies = null;
    public double[] reducedMoments = null;
    public double[] data = null;
    
    public AtomPropertiesArchive archive = null;
}
