/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

import org.cam.ceb.como.tools.periodictable.old.ElementOld;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ElementTest {

    //TEST THE NUMBER OF PAIRED AND UNPAIRED ELECTRONS FOR THE INDIVIDUAL ATOMS!!!
    
    @Test
    public void electroneConfigurationTest() {
        Element he = PeriodicTable.getElementBySymbol("C");
        int paired = he.getNumberOfPairedElectrons();
        int unpaired = he.getNumberOfUnpairedElectrons();
        System.out.println();
        
        // monoatomic oxygen
        // number of e pairs: 3
        // number of unpaired e: 2
        Element o = PeriodicTable.getElementBySymbol("O");
        paired = o.getNumberOfPairedElectrons();
        unpaired = o.getNumberOfUnpairedElectrons();
        System.out.println();
    }
    
    
//    @Test
//    public void elementTiTest() {
//        ElementOld e = new ElementOld();
//        e.setName("Titanium");
//        e.setSymbol("Ti");
//        e.setRow(4);
//        e.setColumn(4);
//        e.setAtomicNumber(22);
//        e.setNumberOfNeutrons(26);
//        e.setAtomicWeight(47.867);
//        e.setBoilingPoint(3560.15);
//        e.setMeltingPoint(1933.15);
//        e.setDensity(4.54);
//        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
//
//        assert(e.getName().equals("Titanium"));
//        assert(e.getSymbol().equals("Ti"));
//        assert(e.getRow()== 4);
//        assert(e.getColumn() == 4);
//        assert(e.getAtomicNumber() == 22);
//        assert(e.getNumberOfNeutrons() == 26);
//        assert(e.getMassNumber()== 48);
//        assert(e.getAtomicWeight() == 47.867);
//        assert(e.getBoilingPoint() == 3560.15);
//        assert(e.getMeltingPoint() == 1933.15);
//        assert(e.getDensity() == 4.54);
//        assert(e.getClassification() == ElementOld.CLASSIFICATION.TRANSITIONMETAL);
//    }
//    
//    @Test
//    public void elementCTest() {
//        ElementOld e = new ElementOld("C", "Carbon");
//        e.setRow(2);
//        e.setColumn(14);
//        e.setAtomicNumber(6);
//        e.setNumberOfNeutrons(6);
//        e.setAtomicWeight(12.0107);
//        e.setBoilingPoint(5100.15);
//        e.setMeltingPoint(3773.15);
//        e.setDensity(2.62);
//        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
//
//        assert(e.getName().equals("Carbon"));
//        assert(e.getSymbol().equals("C"));
//        assert(e.getRow()== 2);
//        assert(e.getColumn() == 14);
//        assert(e.getAtomicNumber() == 6);
//        assert(e.getNumberOfNeutrons() == 6);
//        assert(e.getMassNumber()== 12);
//        assert(e.getAtomicWeight() == 12.0107);
//        assert(e.getBoilingPoint() == 5100.15);
//        assert(e.getMeltingPoint() == 3773.15);
//        assert(e.getDensity() == 2.62);
//        assert(e.getClassification() == ElementOld.CLASSIFICATION.NONMETAL);
//    }
//    
//    @Test
//    public void elementAlTest() {
//        ElementOld e = new ElementOld("Al", 26, 13, 26.981539);
//        e.setName("Aluminium");
//        e.setRow(3);
//        e.setColumn(13);
//        e.setBoilingPoint(2740.15);
//        e.setMeltingPoint(933.52);
//        e.setDensity(2.702);
//        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
//
//        assert(e.getName().equals("Aluminium"));
//        assert(e.getSymbol().equals("Al"));
//        assert(e.getRow()== 3);
//        assert(e.getColumn() == 13);
//        assert(e.getAtomicNumber() == 13);
//        assert(e.getNumberOfNeutrons() == 13);
//        assert(e.getMassNumber()== 26);
//        assert(e.getAtomicWeight() == 26.981539);
//        assert(e.getBoilingPoint() == 2740.15);
//        assert(e.getMeltingPoint() == 933.52);
//        assert(e.getDensity() == 2.702);
//        assert(e.getClassification() == ElementOld.CLASSIFICATION.OTHERMETALS);
//    }
}
