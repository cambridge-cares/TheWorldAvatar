/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

import org.junit.Test;

/**
 *
 * @author pb556
 */
public class PeriodicTableTest {

//    @Test
//    public void elementsTest() {
//        // test the number of elements
//        // System.out.println(PeriodicTable.getElements().size());
//        assert (PeriodicTable.getElements().size() == 112);
//        
//        //assert (PeriodicTable.getElements().size() == 118);
//        
//        assert (PeriodicTable.getElementBySymbol("H") != null);
//        assert (PeriodicTable.getElementBySymbol("He") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Li") != null);
//        assert (PeriodicTable.getElementBySymbol("Be") != null);
//        assert (PeriodicTable.getElementBySymbol("B") != null);
//        assert (PeriodicTable.getElementBySymbol("C") != null);
//        assert (PeriodicTable.getElementBySymbol("N") != null);
//        assert (PeriodicTable.getElementBySymbol("O") != null);
//        assert (PeriodicTable.getElementBySymbol("F") != null);
//        assert (PeriodicTable.getElementBySymbol("Ne") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Na") != null);
//        assert (PeriodicTable.getElementBySymbol("Mg") != null);
//        assert (PeriodicTable.getElementBySymbol("Al") != null);
//        assert (PeriodicTable.getElementBySymbol("Si") != null);
//        assert (PeriodicTable.getElementBySymbol("P") != null);
//        assert (PeriodicTable.getElementBySymbol("S") != null);
//        assert (PeriodicTable.getElementBySymbol("Cl") != null);
//        assert (PeriodicTable.getElementBySymbol("Ar") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("K") != null);
//        assert (PeriodicTable.getElementBySymbol("Ca") != null);
//        assert (PeriodicTable.getElementBySymbol("Sc") != null);
//        assert (PeriodicTable.getElementBySymbol("Ti") != null);
//        assert (PeriodicTable.getElementBySymbol("V") != null);
//        assert (PeriodicTable.getElementBySymbol("Cr") != null);
//        assert (PeriodicTable.getElementBySymbol("Mn") != null);
//        assert (PeriodicTable.getElementBySymbol("Fe") != null);
//        assert (PeriodicTable.getElementBySymbol("Co") != null);
//        assert (PeriodicTable.getElementBySymbol("Ni") != null);
//        assert (PeriodicTable.getElementBySymbol("Cu") != null);
//        assert (PeriodicTable.getElementBySymbol("Zn") != null);
//        assert (PeriodicTable.getElementBySymbol("Ga") != null);
//        assert (PeriodicTable.getElementBySymbol("Ge") != null);
//        assert (PeriodicTable.getElementBySymbol("As") != null);
//        assert (PeriodicTable.getElementBySymbol("Se") != null);
//        assert (PeriodicTable.getElementBySymbol("Br") != null);
//        assert (PeriodicTable.getElementBySymbol("Kr") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Rb") != null);
//        assert (PeriodicTable.getElementBySymbol("Sr") != null);
//        assert (PeriodicTable.getElementBySymbol("Y") != null);
//        assert (PeriodicTable.getElementBySymbol("Zr") != null);
//        assert (PeriodicTable.getElementBySymbol("Nb") != null);
//        assert (PeriodicTable.getElementBySymbol("Mo") != null);
//        assert (PeriodicTable.getElementBySymbol("Tc") != null);
//        assert (PeriodicTable.getElementBySymbol("Ru") != null);
//        assert (PeriodicTable.getElementBySymbol("Rh") != null);
//        assert (PeriodicTable.getElementBySymbol("Pd") != null);
//        assert (PeriodicTable.getElementBySymbol("Ag") != null);
//        assert (PeriodicTable.getElementBySymbol("Cd") != null);
//        assert (PeriodicTable.getElementBySymbol("In") != null);
//        assert (PeriodicTable.getElementBySymbol("Sn") != null);
//        assert (PeriodicTable.getElementBySymbol("Sb") != null);
//        assert (PeriodicTable.getElementBySymbol("Te") != null);
//        assert (PeriodicTable.getElementBySymbol("I") != null);
//        assert (PeriodicTable.getElementBySymbol("Xe") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Cs") != null);
//        assert (PeriodicTable.getElementBySymbol("Ba") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Hf") != null);
//        assert (PeriodicTable.getElementBySymbol("Ta") != null);
//        assert (PeriodicTable.getElementBySymbol("W") != null);
//        assert (PeriodicTable.getElementBySymbol("Re") != null);
//        assert (PeriodicTable.getElementBySymbol("Os") != null);
//        assert (PeriodicTable.getElementBySymbol("Ir") != null);
//        assert (PeriodicTable.getElementBySymbol("Pt") != null);
//        assert (PeriodicTable.getElementBySymbol("Au") != null);
//        assert (PeriodicTable.getElementBySymbol("Hg") != null);
//        assert (PeriodicTable.getElementBySymbol("Tl") != null);
//        assert (PeriodicTable.getElementBySymbol("Pb") != null);
//        assert (PeriodicTable.getElementBySymbol("Bi") != null);
//        assert (PeriodicTable.getElementBySymbol("Po") != null);
//        assert (PeriodicTable.getElementBySymbol("At") != null);
//        assert (PeriodicTable.getElementBySymbol("Rn") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Fr") != null);
//        assert (PeriodicTable.getElementBySymbol("Ra") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Rf") != null);
//        assert (PeriodicTable.getElementBySymbol("Db") != null);
//        assert (PeriodicTable.getElementBySymbol("Sg") != null);
//        assert (PeriodicTable.getElementBySymbol("Bh") != null);
//        assert (PeriodicTable.getElementBySymbol("Hs") != null);
//        assert (PeriodicTable.getElementBySymbol("Mt") != null);
//        assert (PeriodicTable.getElementBySymbol("Uun") != null);
//        assert (PeriodicTable.getElementBySymbol("Uuu") != null);
//        assert (PeriodicTable.getElementBySymbol("Uub") != null);
//        
//        
//        assert (PeriodicTable.getElementBySymbol("La") != null);
//        assert (PeriodicTable.getElementBySymbol("Ce") != null);
//        assert (PeriodicTable.getElementBySymbol("Pr") != null);
//        assert (PeriodicTable.getElementBySymbol("Nd") != null);
//        assert (PeriodicTable.getElementBySymbol("Pm") != null);
//        assert (PeriodicTable.getElementBySymbol("Sm") != null);
//        assert (PeriodicTable.getElementBySymbol("Eu") != null);
//        assert (PeriodicTable.getElementBySymbol("Gd") != null);
//        assert (PeriodicTable.getElementBySymbol("Tb") != null);
//        assert (PeriodicTable.getElementBySymbol("Dy") != null);
//        assert (PeriodicTable.getElementBySymbol("Ho") != null);
//        assert (PeriodicTable.getElementBySymbol("Er") != null);
//        assert (PeriodicTable.getElementBySymbol("Tm") != null);
//        assert (PeriodicTable.getElementBySymbol("Yb") != null);
//        assert (PeriodicTable.getElementBySymbol("Lu") != null);
//        
//        assert (PeriodicTable.getElementBySymbol("Ac") != null);
//        assert (PeriodicTable.getElementBySymbol("Th") != null);
//        assert (PeriodicTable.getElementBySymbol("Pa") != null);
//        assert (PeriodicTable.getElementBySymbol("U") != null);
//        assert (PeriodicTable.getElementBySymbol("Np") != null);
//        assert (PeriodicTable.getElementBySymbol("Pu") != null);
//        assert (PeriodicTable.getElementBySymbol("Am") != null);
//        assert (PeriodicTable.getElementBySymbol("Cm") != null);
//        assert (PeriodicTable.getElementBySymbol("Bk") != null);
//        assert (PeriodicTable.getElementBySymbol("Cf") != null);
//        assert (PeriodicTable.getElementBySymbol("Es") != null);
//        assert (PeriodicTable.getElementBySymbol("Fm") != null);
//        assert (PeriodicTable.getElementBySymbol("Md") != null);
//        assert (PeriodicTable.getElementBySymbol("No") != null);
//        assert (PeriodicTable.getElementBySymbol("Lr") != null);
//        
//        
//        
////        PeriodicTable.getElementByAtomicNumber(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByAtomicNumberRange(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByBlock(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByClassification(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByCol(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByCol(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByRow(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByRow(atomicNumberStart, atomicNumberEnd);
////        PeriodicTable.getElementByName(null);
////        PeriodicTable.getElementByPosition(row, col);
////        PeriodicTable.getElementByPositionRange(rowStart, rowEnd, colStart, colEnd);
////        PeriodicTable.getElementByRow(row);
////        PeriodicTable.getElementBySymbol(null);
////        PeriodicTable.getElements();
//    }
//    
//    @Test
//    public void elementBlocksTest() {
//        // test the number of elements
//        
//        assert (PeriodicTable.getElementByBlock(Block.sBlock).size() == 14); // 14
//        assert (PeriodicTable.getElementByBlock(Block.dBlock).size() == 38); // 38        
//        assert (PeriodicTable.getElementByBlock(Block.pBlock).size() == 36); // 30
//        assert (PeriodicTable.getElementByBlock(Block.fBlock).size() == 30); // 30
//    }
//    
//    @Test
//    public void elementClassificationTest() {
//        // test the number of elements
//        
//        
//        
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.NONMETAL).size() == 7); // 7
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.ALKALIMETAL).size() == 6); // 7
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.ALKALINEEARTH).size() == 6); //4
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.HALOGEN).size() == 5); // 5
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.METALLOID).size() == 7); // 7
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.NOBLEGAS).size() == 6); // 6
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.OTHERMETALS).size() == 7); // 7
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES).size() == 30); //30
////        assert (PeriodicTable.getElementByClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL).size() == 38); //38
//    }
//
//    @Test
//    public void elementGroupsTest() {
//    }
}
