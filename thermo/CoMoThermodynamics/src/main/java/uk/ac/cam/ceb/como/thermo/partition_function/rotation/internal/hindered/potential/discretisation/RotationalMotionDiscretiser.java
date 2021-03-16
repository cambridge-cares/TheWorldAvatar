/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRUtils;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class RotationalMotionDiscretiser {
    
    protected CMLMolecule mol = null;
    protected CMLBond bond = null;
    
    protected Logger logger = Logger.getLogger(getClass());
    
    public RotationalMotionDiscretiser() {}
    
    public void setReferenceGeometry(CMLMolecule mol) {
        this.mol = mol;
    }
    
    public void setTorsionalBond(CMLBond bond) {
        this.bond = bond;
    }
    
    public CMLMolecule getReferenceGeometry() {
        return mol;
    }
    
    public CMLBond getTorsionalBond() {
        return bond;
    }
    
    /**
     * 
     * @param mol
     * @param bond
     * @param angle in radiant
     * @return
     * @throws Exception 
     */
    public RotationalStep rotate(CMLMolecule mol, CMLBond bond, double angle) throws Exception {
        if (mol == null) {
            logger.error("No molecule has been defined!");
            return null;
        } else if (mol.getAtomCount() < 2 || mol.getBondCount() < 1) {
            logger.error("Invalid molecule has been defined!");
            return null;
        } else if (bond == null) {
            logger.error("No torsional bond has been defined!");
            return null;
        }
        try {
            return new RotationalStep(IRUtils.rotate(mol, bond, angle), angle);
        } catch (Exception ex) {
            logger.error("The top defined by the torsional bond " + bond.getId() + 
                    " of the molecule " + mol.getId() + " could not be rotatet!", ex);
        }
        return new RotationalStep(null, angle);
    }
    
    /**
     * 
     * @param angle in radiant
     * @return
     * @throws Exception 
     */
    public RotationalStep rotate(double angle) throws Exception {
        return rotate(getReferenceGeometry(), getTorsionalBond(), angle);
    }
    
    /**
     * 
     * @param angles in radiant
     * @return
     * @throws Exception 
     */
    public DataSpace rotate(List<Double> angles) throws Exception {
        return rotate(getReferenceGeometry(), getTorsionalBond(), angles);
    }
    
    /**
     * 
     * @param mol
     * @param bond
     * @param angles in radiant
     * @return
     * @throws Exception 
     */
    public DataSpace rotate(CMLMolecule mol, CMLBond bond, List<Double> angles) throws Exception {
        DataSpace steps = new DataSpace();
        for (Double a : angles) {
            steps.add(new DataPoint<Double>(new Double[]{a}, rotate(mol, bond, a)));            
        }
        return steps;
    }
    
    public DataSpace rotate(int num) throws Exception {
        return rotate(getReferenceGeometry(), getTorsionalBond(), num);
    }
    
    public DataSpace rotate(CMLMolecule mol, CMLBond bond, int num) throws Exception {
        double dAngle = (Math.PI * 2.0) / (double) (num + 1);
        ArrayList<Double> angles = new ArrayList<Double>();
        for (int i = 1; i <= num; i++) {
            angles.add(i * dAngle);
        }
        return rotate(mol, bond, angles);
    }
}
