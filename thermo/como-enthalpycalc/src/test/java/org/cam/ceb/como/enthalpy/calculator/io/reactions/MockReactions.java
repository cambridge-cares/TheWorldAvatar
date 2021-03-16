/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.reactions;

import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class MockReactions {
    
    public static Reaction getReactionC2H6O() {
        Reaction r = new Reaction(MockSpecies.getC2H6O());
        r.addReactant(MockSpecies.getC2H6O(), 1.0);
        r.addReactant(MockSpecies.getCH4(), 1.0);
        r.addProduct(MockSpecies.getC2H6(), 1.0);
        r.addProduct(MockSpecies.getCH4O(), 1.0);
        return r;
    }
    
    public static Reaction getReactionC2H4() {
        Reaction r = new Reaction(MockSpecies.getC2H4());
        r.addReactant(MockSpecies.getC2H4(), 1.0);
        r.addReactant(MockSpecies.getCH4O(), 1.0);
        r.addProduct(MockSpecies.getCH4(), 1.0);
        r.addProduct(MockSpecies.getC2H4O(), 1.0);
        return r;
    }
    
    public static Reaction getReactionC3H6() {
        Reaction r = new Reaction(MockSpecies.getC3H6());
        r.addReactant(MockSpecies.getC3H6(), 1.0);
        r.addReactant(MockSpecies.getCH4(), 1.0);
        r.addProduct(MockSpecies.getC2H6(), 1.0);
        r.addProduct(MockSpecies.getC2H4(), 1.0);
        return r;
    }
    
    public static Reaction getReactionCH4O() {
        Reaction r = new Reaction(MockSpecies.getCH4O());
        r.addReactant(MockSpecies.getCH4O(), 1.0);
        r.addReactant(MockSpecies.getC2H4(), 1.0);
        r.addProduct(MockSpecies.getC2H4O(), 1.0);
        r.addProduct(MockSpecies.getCH4(), 1.0);
        return r;
    }
    
    public static Reaction getReactionCH4() {
        Reaction r = new Reaction(MockSpecies.getCH4());
        r.addReactant(MockSpecies.getCH4(), 1.0);
        r.addReactant(MockSpecies.getC2H4O(), 1.0);
        r.addProduct(MockSpecies.getC2H4(), 1.0);
        r.addProduct(MockSpecies.getCH4O(), 1.0);
        return r;
    }
    
    public static Reaction getReactionC2H4O() {
        Reaction r = new Reaction(MockSpecies.getC2H4O());
        r.addReactant(MockSpecies.getC2H4O(), 1.0);
        r.addReactant(MockSpecies.getCH4(), 1.0);
        r.addProduct(MockSpecies.getC2H4(), 1.0);
        r.addProduct(MockSpecies.getCH4O(), 1.0);
        return r;
    }
    
    public static Reaction getReactionC2H4Modified() {
        Reaction r = new Reaction(MockSpecies.getC2H4());
        r.addReactant(MockSpecies.getC2H4(), 0.25);
        r.addReactant(MockSpecies.getCH4O(), 0.25);
        r.addProduct(MockSpecies.getCH4(), 0.25);
        r.addProduct(MockSpecies.getC2H4O(), 0.25);
        return r;
    }
    
    public static ReactionList getCompleteList() {
        ReactionList rList = new ReactionList();
        rList.add(MockReactions.getReactionC2H4());
        rList.add(MockReactions.getReactionC2H4O());
        rList.add(MockReactions.getReactionC2H6O());
        rList.add(MockReactions.getReactionC3H6());
        rList.add(MockReactions.getReactionCH4());
        rList.add(MockReactions.getReactionCH4O());
        return rList;
    }
}
