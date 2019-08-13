/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.filter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.cam.ceb.como.enthalpy.calculator.io.pool.CSVParser;
import org.cam.ceb.como.enthalpy.calculator.io.pool.SpeciesPoolParser;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ValidationFilterTest {
    
    @Test
    public void filterTest() throws FileNotFoundException, IOException, Exception {
        
    	CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/issues/pool_hydrocarbons_12595-82-3.csv"));
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        Collection<Species> soi = parser.getSpeciesOfInterest();

        SpeciesFilter f = new RadicalsFilter(0, 1);
        f.set(ref);
        f.filter();
        
        ref = f.getValidSpecies();
        
        Collections.shuffle((List<Species>) ref);
        
        ValidationFilter redFilter = new ValidationFilter();
        redFilter.set(ref);
        redFilter.filter();
        
        Map<Species, Reaction> invalid = redFilter.getDetailedInvalidSpecies();
        Map<Species, Reaction> valid = redFilter.getDetailedValidSpecies();
        
        System.out.println("Invalid:");        
        for (Species s : invalid.keySet()) {
            System.out.println(s.getRef() + 
                    " (" + Math.abs(invalid.get(s).calculateHf() - s.getHf()) + ")" + 
                    ": " + invalid.get(s).toString());
        }
        
        System.out.println();
        
        System.out.println("Valid:"); 
        
        for (Species s : valid.keySet()) {
            System.out.println(s.getRef() + 
                    " (" + Math.abs(valid.get(s).calculateHf() - s.getHf()) + ")" + 
                    ": " + valid.get(s).toString());
        }
    }
}
