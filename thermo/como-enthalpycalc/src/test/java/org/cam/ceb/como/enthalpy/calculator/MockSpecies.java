/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator;

import org.cam.ceb.como.enthalpy.calculator.species.BondType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import java.util.ArrayList;
import org.cam.ceb.como.tools.periodictable.PeriodicTable;

/**
 *
 * @author pb556
 */
public class MockSpecies {

    public static Species getC2H4() {
        Species species = new Species("C2H4", 52.47, -206315.5126569827);
        species.addAtom("C1", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C2", PeriodicTable.getElementBySymbol("C"));
        
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        
        species.addBond(BondType.DOUBLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C2", "H3");
        species.addBond(BondType.SINGLE, "C2", "H4");
        
        return species;
    }
    
    public static Species getC3H8() {
        Species species = new Species("C3H8", -104.7, -312776.73055472755);
        species.addAtom("C1", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C2", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C3", PeriodicTable.getElementBySymbol("C"));
        
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H5", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H6", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H7", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H8", PeriodicTable.getElementBySymbol("H"));
        
        species.addBond(BondType.SINGLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C2", "C3");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C1", "H3");
        
        species.addBond(BondType.SINGLE, "C2", "H4");
        species.addBond(BondType.SINGLE, "C2", "H5");
        
        species.addBond(BondType.SINGLE, "C3", "H6");
        species.addBond(BondType.SINGLE, "C3", "H7");
        species.addBond(BondType.SINGLE, "C3", "H8");
        
        return species;
    }
    
    public static Species getC2H6() {
        Species species = new Species("C2H6", -83.8, -209566.6071562765);
        species.addAtom("C1", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C2", PeriodicTable.getElementBySymbol("C"));
        
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H5", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H6", PeriodicTable.getElementBySymbol("H"));
        
        species.addBond(BondType.SINGLE, "C1", "C2");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C1", "H3");
        
        species.addBond(BondType.SINGLE, "C2", "H4");
        species.addBond(BondType.SINGLE, "C2", "H5");   
        species.addBond(BondType.SINGLE, "C2", "H6");
        
        return species;
    }
    
    public static Species getC3H6() {
        Species species = new Species("C3H6", 20.41, -309539.79717509745);
        species.addAtom("C1", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C2", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C3", PeriodicTable.getElementBySymbol("C"));
        
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H5", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H6", PeriodicTable.getElementBySymbol("H"));
        
        species.addBond(BondType.SINGLE, "C1", "C2");
        species.addBond(BondType.DOUBLE, "C2", "C3");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C1", "H3");
        
        species.addBond(BondType.SINGLE, "C2", "H4");
        
        species.addBond(BondType.SINGLE, "C3", "H5");
        species.addBond(BondType.SINGLE, "C3", "H6");
        
        return species;
    }
    
    public static Species getC2H6O() {
        Species species = new Species("C2H6O", -234.0, -407053.25075890456);
        species.addAtom("C1", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C2", PeriodicTable.getElementBySymbol("C"));
        
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H5", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H6", PeriodicTable.getElementBySymbol("H"));
        
        species.addAtom("O", PeriodicTable.getElementBySymbol("O"));
        
        species.addBond(BondType.SINGLE, "C1", "C2");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C1", "H3");
        
        species.addBond(BondType.SINGLE, "C2", "H4");
        species.addBond(BondType.SINGLE, "C2", "H5");   
        species.addBond(BondType.SINGLE, "C2", "O");
        species.addBond(BondType.SINGLE, "O", "H6");
        
        return species;
    }
    
    public static Species getCH4() {
        Species species = new Species("CH4", -74.87, -106362.05612155756);
        species.addAtom("C", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        
        species.addBond(BondType.SINGLE, "C", "H1");
        species.addBond(BondType.SINGLE, "C", "H2");
        species.addBond(BondType.SINGLE, "C", "H3");
        species.addBond(BondType.SINGLE, "C", "H4");
        
        return species;
    }
    
    public static Species getC2H4O() {
        Species species = new Species("C2H4O", -128.0, -403827.8303397706);
        species.addAtom("C1", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("C2", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("O", PeriodicTable.getElementBySymbol("O"));
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.DOUBLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C2", "O");
        species.addBond(BondType.SINGLE, "C2", "H3");
        species.addBond(BondType.SINGLE, "O", "H4");
        
        return species;
    }
    
    // CH3OH    
    public static Species getCH4O() {

        Species species = new Species("CH4O", -205.0, -303829.34532854107);
        species.addAtom("C", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H4", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("O", PeriodicTable.getElementBySymbol("O"));
        
        species.addBond(BondType.SINGLE, "C", "H1");
        species.addBond(BondType.SINGLE, "C", "H2");
        species.addBond(BondType.SINGLE, "C", "H3");
        species.addBond(BondType.SINGLE, "C", "O");
        species.addBond(BondType.SINGLE, "O", "H4");
        
        return species;

    } 
    
    public static ArrayList<Species> getPool() {
        ArrayList<Species> pool = new ArrayList<Species>();
        pool.add(getC2H4());
        pool.add(getC2H4O());
        pool.add(getC2H6());
        pool.add(getC3H6());
        pool.add(getC2H6O());
        pool.add(getC3H8());
        pool.add(getCH4());
        pool.add(getCH4O());
        return pool;
    }
}
