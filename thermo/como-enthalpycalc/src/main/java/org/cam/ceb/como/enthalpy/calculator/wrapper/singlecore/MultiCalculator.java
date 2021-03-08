/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.species.Species;

/**
 *
 * @author pb556
 */
public class MultiCalculator extends ObjectPoolCalculator {

    // add ReactionSelector to it!!!
    // support a list of calculators!!!
    // use MultiSpeciesFilter class as template
    protected List<ObjectPoolCalculator> calculators = new ArrayList<ObjectPoolCalculator>();
    private Logger logger = Logger.getLogger(MultiCalculator.class);
    
    protected Map<ObjectPoolCalculator, Object> results = new HashMap<ObjectPoolCalculator, Object>();
    
    // return: 
    // first
    // mean
    // median
    // by calculator
    
    public MultiCalculator() {
        super();
    }
    
    public MultiCalculator(List<ObjectPoolCalculator> calculators) {
        super();
        this.calculators = calculators;
    }
    
    public List<ObjectPoolCalculator> getOrderedListOfCalculators() {
        return calculators;
    }
    
    public void set(List<ObjectPoolCalculator> calculators) {
        this.calculators = calculators;
    }
    
    public boolean add(ObjectPoolCalculator calculator) {
        return calculators.add(calculator);
    }
    
    public void add(int index, ObjectPoolCalculator calculator) {
        calculators.add(index, calculator);
    }
    
    public boolean addAll(ArrayList<ObjectPoolCalculator> calculators) {
        return calculators.addAll(calculators);
    }
    
    public boolean addAll(int index, ArrayList<ObjectPoolCalculator> calculators) {
        return calculators.addAll(index, calculators);
    }
    
    public boolean remove(ObjectPoolCalculator calculator) {
        return calculators.remove(calculator);
    }
    
    public ObjectPoolCalculator remove(int index) {
        return calculators.remove(index);
    }
    
    public boolean removeAll(ArrayList<ObjectPoolCalculator> calculators) {
        return calculators.removeAll(calculators);
    }
    
    @Override
    public void calculate(Collection<Species> targetSpecies) throws Exception {
        for (ObjectPoolCalculator calculator : calculators) {
            calculator.calculate(targetSpecies);
            results.put(calculator, calculator.get());
        }
    }

    @Override
    public void calculate(Species targetSpecies) throws Exception {
        for (ObjectPoolCalculator calculator : calculators) {
            calculator.calculate(targetSpecies);
            results.put(calculator, calculator.get());
        }
    }

    @Override
    public void calculate() throws Exception {
        for (ObjectPoolCalculator calculator : calculators) {
            calculator.set(pool);
            calculator.calculate();
            results.put(calculator, calculator.get());
        }
    }

    @Override
    public Object get() {
        return results;
    }
    
}
