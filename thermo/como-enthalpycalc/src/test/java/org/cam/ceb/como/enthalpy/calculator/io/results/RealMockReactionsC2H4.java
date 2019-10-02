/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.species.Species;

/**
 *
 * @author pb556
 */
public class RealMockReactionsC2H4 {
    
    protected static Species C2H4 = MockSpecies.getC2H4();
    
    public static Reaction getSolution1() {
        Reaction r = new Reaction(C2H4);
        r.addReactant(C2H4, 1.0);
        r.addReactant(MockSpecies.getCH4O(), 1.0);
        r.addProduct(MockSpecies.getC2H4O(), 1.0);
        r.addProduct(MockSpecies.getCH4(), 1.0);
        return r;
    }
    
    public static Reaction getSolution2() {
        Reaction r = new Reaction(C2H4);
        r.addReactant(C2H4, 1.0);
        r.addReactant(MockSpecies.getC2H6O(), 1.0);
        r.addProduct(MockSpecies.getC2H4O(), 1.0);
        r.addProduct(MockSpecies.getC2H6(), 1.0);
        return r;
    }
    
    public static Reaction getSolution3() {
        Reaction r = new Reaction(C2H4);
        r.addReactant(C2H4, 1.0);
        r.addReactant(MockSpecies.getCH4O(), 1.0);
        r.addReactant(MockSpecies.getC3H8(), 1.0);
        r.addProduct(MockSpecies.getC2H4O(), 1.0);
        r.addProduct(MockSpecies.getC2H6(), 2.0);
        return r;
    }
    
    public static Reaction getSolution4() {
        Reaction r = new Reaction(C2H4);
        r.addReactant(C2H4, 1.0);
        r.addReactant(MockSpecies.getC2H6O(), 2.0);
        r.addProduct(MockSpecies.getC2H4O(), 1.0);
        r.addProduct(MockSpecies.getCH4O(), 1.0);
        r.addProduct(MockSpecies.getC3H8(), 1.0);
        return r;
    }
    
    public static Reaction getSolution5() {
        Reaction r = new Reaction(C2H4);
        r.addReactant(C2H4, 2.0);
        r.addReactant(MockSpecies.getC2H6O(), 2.0);
        r.addProduct(MockSpecies.getC2H4O(), 2.0);
        r.addProduct(MockSpecies.getCH4(), 1.0);
        r.addProduct(MockSpecies.getC3H8(), 1.0);
        return r;
    }
    
}
