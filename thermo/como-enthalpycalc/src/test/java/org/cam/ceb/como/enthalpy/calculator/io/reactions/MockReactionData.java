/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.reactions;

import java.util.HashMap;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;

/**
 *
 * @author pb556
 */
public class MockReactionData {
    
    public static ReactionData getC2H6O() {
        HashMap<String, Double> reactants = new HashMap<String, Double>();
        HashMap<String, Double> products = new HashMap<String, Double>();
        
        reactants.put(MockSpecies.getC2H6O().getRef(), 1.0);
        reactants.put(MockSpecies.getCH4().getRef(), 1.0);
        products.put(MockSpecies.getC2H6().getRef(), 1.0);
        products.put(MockSpecies.getCH4O().getRef(), 1.0);
        
        return new ReactionData(reactants, products, MockSpecies.getC2H6O().getRef());
    }
    
    public static ReactionData getC2H4() {
        HashMap<String, Double> reactants = new HashMap<String, Double>();
        HashMap<String, Double> products = new HashMap<String, Double>();
        
        reactants.put(MockSpecies.getC2H4().getRef(), 1.0);
        reactants.put(MockSpecies.getCH4O().getRef(), 1.0);
        products.put(MockSpecies.getCH4().getRef(), 1.0);
        products.put(MockSpecies.getC2H4O().getRef(), 1.0);
        
        return new ReactionData(reactants, products, MockSpecies.getC2H4().getRef());
    }
    
    public static ReactionData getC3H6() {
        HashMap<String, Double> reactants = new HashMap<String, Double>();
        HashMap<String, Double> products = new HashMap<String, Double>();
        
        reactants.put(MockSpecies.getC3H6().getRef(), 1.0);
        reactants.put(MockSpecies.getCH4().getRef(), 1.0);
        products.put(MockSpecies.getC2H6().getRef(), 1.0);
        products.put(MockSpecies.getC2H4().getRef(), 1.0);
        
        return new ReactionData(reactants, products, MockSpecies.getC3H6().getRef());
    }
    
    public static ReactionData getCH4O() {
        HashMap<String, Double> reactants = new HashMap<String, Double>();
        HashMap<String, Double> products = new HashMap<String, Double>();
        
        reactants.put(MockSpecies.getCH4O().getRef(), 1.0);
        reactants.put(MockSpecies.getC2H4().getRef(), 1.0);
        products.put(MockSpecies.getC2H4O().getRef(), 1.0);
        products.put(MockSpecies.getCH4().getRef(), 1.0);
        
        return new ReactionData(reactants, products, MockSpecies.getCH4O().getRef());
    }
    
    public static ReactionData getCH4() {
        HashMap<String, Double> reactants = new HashMap<String, Double>();
        HashMap<String, Double> products = new HashMap<String, Double>();
        
        reactants.put(MockSpecies.getCH4().getRef(), 1.0);
        reactants.put(MockSpecies.getC2H4O().getRef(), 1.0);
        products.put(MockSpecies.getC2H4().getRef(), 1.0);
        products.put(MockSpecies.getCH4O().getRef(), 1.0);
        
        return new ReactionData(reactants, products, MockSpecies.getCH4().getRef());
    }
    
    public static ReactionData getC2H4O() {
        HashMap<String, Double> reactants = new HashMap<String, Double>();
        HashMap<String, Double> products = new HashMap<String, Double>();
        
        reactants.put(MockSpecies.getC2H4O().getRef(), 1.0);
        reactants.put(MockSpecies.getCH4().getRef(), 1.0);
        products.put(MockSpecies.getC2H4().getRef(), 1.0);
        products.put(MockSpecies.getCH4O().getRef(), 1.0);
        
        return new ReactionData(reactants, products, MockSpecies.getC2H4O().getRef());
    }
    
    public static ReactionData getC2H4Modified() {
        HashMap<String, Double> reactants = new HashMap<String, Double>();
        HashMap<String, Double> products = new HashMap<String, Double>();
        
        reactants.put(MockSpecies.getC2H4().getRef(), 0.25);
        reactants.put(MockSpecies.getCH4O().getRef(), 0.25);
        products.put(MockSpecies.getCH4().getRef(), 0.25);
        products.put(MockSpecies.getC2H4O().getRef(), 0.25);
        
        return new ReactionData(reactants, products, MockSpecies.getC2H4().getRef());
    }
    
    public static ReactionDataList getCompleteList() {
        ReactionDataList data = new ReactionDataList();
        data.add(getC2H4());
        data.add(getC2H4Modified());
        data.add(getC2H4O());
        data.add(getC2H6O());
        data.add(getC3H6());
        data.add(getCH4());
        data.add(getCH4O());
        return data;
    }
}
