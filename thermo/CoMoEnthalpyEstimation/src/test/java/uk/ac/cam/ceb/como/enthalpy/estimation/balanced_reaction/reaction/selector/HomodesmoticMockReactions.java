/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.MockReactions;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.BondType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */

public class HomodesmoticMockReactions {
    
    public static Reaction getValidHomodesmoticReactionA() {
        Species c2h4 = getC2H4();
        Species c4h4 = getC4H4();
        Species c6h10 = getC6H10();
        Species c8h10 = getC8H10();
        
        Reaction r = new Reaction(c2h4);
        r.addReactant(c2h4, 1);
        r.addReactant(c8h10, 1);
        r.addProduct(c4h4, 1);
        r.addProduct(c6h10, 1);
        
        return r;
    }
    
    public static Reaction getInvalidHomodesmoticReactionA() {
        Reaction r = new Reaction(MockSpecies.getC2H4());
        r.addReactant(MockSpecies.getC2H4(), 1.0);
        r.addReactant(MockSpecies.getCH4O(), 1.0);
        r.addProduct(MockSpecies.getCH4(), 1.0);
        r.addProduct(MockSpecies.getC2H4O(), 1.0);
        return r;
    }
    
    public static Species getC8H10() {
        Species s = new Species("C8H10");
        for (int i = 0; i < 8; i++) {
            s.addAtom("C" + (i + 1), "C");
        }
        for (int i = 0; i < 10; i++) {
            s.addAtom("H" + (i + 1), "H");
        }
        s.addBond(BondType.SINGLE, "H1", "C1");
        s.addBond(BondType.SINGLE, "H2", "C1");
        s.addBond(BondType.SINGLE, "H3", "C3");
        s.addBond(BondType.SINGLE, "H4", "C3");
        s.addBond(BondType.SINGLE, "H5", "C3");
        s.addBond(BondType.SINGLE, "H6", "C5");
        s.addBond(BondType.SINGLE, "H7", "C5");
        s.addBond(BondType.SINGLE, "H8", "C5");
        s.addBond(BondType.SINGLE, "H9", "C6");
        s.addBond(BondType.SINGLE, "H10", "C8");
        s.addBond(BondType.DOUBLE, "C1", "C2");
        s.addBond(BondType.SINGLE, "C2", "C3");
        s.addBond(BondType.SINGLE, "C2", "C4");
        s.addBond(BondType.SINGLE, "C4", "C5");
        s.addBond(BondType.DOUBLE, "C4", "C6");
        s.addBond(BondType.SINGLE, "C6", "C7");
        s.addBond(BondType.TRIPLE, "C7", "C8");
        return s;
    }
    
    public static Species getC6H10() {
        Species s = new Species("C6H10");
        for (int i = 0; i < 6; i++) {
            s.addAtom("C" + (i + 1), "C");
        }
        for (int i = 0; i < 10; i++) {
            s.addAtom("H" + (i + 1), "H");
        }
        s.addBond(BondType.SINGLE, "H1", "C1");
        s.addBond(BondType.SINGLE, "H2", "C1");
        s.addBond(BondType.SINGLE, "H3", "C3");
        s.addBond(BondType.SINGLE, "H4", "C3");
        s.addBond(BondType.SINGLE, "H5", "C3");
        s.addBond(BondType.SINGLE, "H6", "C5");
        s.addBond(BondType.SINGLE, "H7", "C5");
        s.addBond(BondType.SINGLE, "H8", "C5");
        s.addBond(BondType.SINGLE, "H9", "C6");
        s.addBond(BondType.SINGLE, "H10", "C6");
        s.addBond(BondType.DOUBLE, "C1", "C2");
        s.addBond(BondType.SINGLE, "C2", "C3");
        s.addBond(BondType.SINGLE, "C2", "C4");
        s.addBond(BondType.SINGLE, "C4", "C5");
        s.addBond(BondType.DOUBLE, "C4", "C6");
        return s;
    }
    
    public static Species getC4H4() {
        Species s = new Species("C4H4");
        for (int i = 0; i < 4; i++) {
            s.addAtom("C" + (i + 1), "C");
        }
        for (int i = 0; i < 4; i++) {
            s.addAtom("H" + (i + 1), "H");
        }
        s.addBond(BondType.SINGLE, "H1", "C1");
        s.addBond(BondType.SINGLE, "H2", "C3");
        s.addBond(BondType.SINGLE, "H3", "C4");
        s.addBond(BondType.SINGLE, "H4", "C4");
        s.addBond(BondType.TRIPLE, "C1", "C2");
        s.addBond(BondType.SINGLE, "C2", "C3");
        s.addBond(BondType.DOUBLE, "C3", "C4");
        return s;
    }
    
    public static Species getC2H4() {
        Species s = new Species("C2H4");
        for (int i = 0; i < 2; i++) {
            s.addAtom("C" + (i + 1), "C");
        }
        for (int i = 0; i < 4; i++) {
            s.addAtom("H" + (i + 1), "H");
        }
        s.addBond(BondType.SINGLE, "H1", "C1");
        s.addBond(BondType.SINGLE, "H2", "C1");
        s.addBond(BondType.SINGLE, "H3", "C2");
        s.addBond(BondType.SINGLE, "H4", "C2");
        s.addBond(BondType.DOUBLE, "C1", "C2");
        return s;
    }
}
