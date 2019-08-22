/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser;

import java.util.List;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;

/**
 *
 * @author pb556
 */
public class TorsionalBond {
    public CMLBond bond;
    public int periodicity;
    public int symmetry;
    public int multiplicity;
    public double redMoment;
    public double freq;
    public List<CMLAtom> top;
    public VibrationalNormalMode mode;
}
