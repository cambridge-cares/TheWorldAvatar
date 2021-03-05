/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.parser.g09;

import java.util.List;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import uk.ac.cam.ceb.como.chem.property.Vibration;

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
    public Vibration mode;
}
