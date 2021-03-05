/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.reactions;

import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ReactionTest {
    
    @Test
    public void reactionTest1() {
        Species c2h4 = MockSpecies.getC2H4();
        
        Reaction reaction = new Reaction(c2h4);
        reaction.addReactant(c2h4, 1);
        reaction.addReactant(MockSpecies.getCH4O(), 1);
        reaction.addProduct(MockSpecies.getC2H4O(), 1);
        reaction.addProduct(MockSpecies.getCH4(), 1);
        
        System.out.println("1: " + reaction.toString() + ": " + reaction.calculateHf() + " (" + (reaction.getSpecies().getHf() - reaction.calculateHf()) + ")");

        reaction = new Reaction(c2h4);
        reaction.addReactant(c2h4, 1);
        reaction.addReactant(MockSpecies.getC2H6O(), 1);
        reaction.addProduct(MockSpecies.getC2H4O(), 1);
        reaction.addProduct(MockSpecies.getC2H6(), 1);
        
        System.out.println("2: " + reaction.toString() + ": " + reaction.calculateHf() + " (" + (reaction.getSpecies().getHf() - reaction.calculateHf()) + ")");

        reaction = new Reaction(c2h4);
        reaction.addReactant(c2h4, 1);
        reaction.addReactant(MockSpecies.getCH4O(), 1);
        reaction.addReactant(MockSpecies.getC3H8(), 1);
        reaction.addProduct(MockSpecies.getC2H4O(), 1);
        reaction.addProduct(MockSpecies.getC2H6(), 2);
        
        System.out.println("3: " + reaction.toString() + ": " + reaction.calculateHf() + " (" + (reaction.getSpecies().getHf() - reaction.calculateHf()) + ")");

        reaction = new Reaction(c2h4);
        reaction.addReactant(c2h4, 1);
        reaction.addReactant(MockSpecies.getC2H6O(), 2);
        reaction.addProduct(MockSpecies.getC2H4O(), 1);
        reaction.addProduct(MockSpecies.getCH4O(), 1);
        reaction.addProduct(MockSpecies.getC3H8(), 1);
        
        System.out.println("4: " + reaction.toString() + ": " + reaction.calculateHf() + " (" + (reaction.getSpecies().getHf() - reaction.calculateHf()) + ")");

        
        reaction = new Reaction(c2h4);
        reaction.addReactant(c2h4, 2);
        reaction.addReactant(MockSpecies.getC2H6O(), 2);
        reaction.addProduct(MockSpecies.getC2H4O(), 2);
        reaction.addProduct(MockSpecies.getCH4(), 1);
        reaction.addProduct(MockSpecies.getC3H8(), 1);
        
        System.out.println("5: " + reaction.toString() + ": " + reaction.calculateHf() + " (" + (reaction.getSpecies().getHf() - reaction.calculateHf()) + ")");
    }
    
}
