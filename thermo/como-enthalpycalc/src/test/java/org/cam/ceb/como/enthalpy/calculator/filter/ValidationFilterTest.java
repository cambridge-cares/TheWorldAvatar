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
 * 
 */

public class ValidationFilterTest {
    
    @Test
    public void filterTest() throws FileNotFoundException, IOException, Exception {
        
//    	CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/issues/pool_hydrocarbons_12595-82-3.csv"));
    	CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/issues/ref_scaled_kJperMols_v8.csv")); // Check whether enthalpy of formation     	
//    	CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/issues/ref-enthalpy_scaled_kJperMol.csv"));
    	
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        
        Collection<Species> soi = parser.getSpeciesOfInterest();
        
        int i = 0;
        
        for(Species s: ref) {
        
        System.out.println(i++ + " : "+ "s.getRef(): " + s.getRef());
        
        }
        
        /**
         * 
         * Q: What is the meaning of (0,1) is RadicalsFilter.
         * Comment: It uses RadicalFilter(0,1) to reduce the number of species in cross validation.
         * If we change 0->2 and 0 -> 2 then number of species to validate will be 65.
         * If we set RadicalsFilter(0,11) then 248 species will be validated. If we set RadicalsFilter(0,12) then 250 species will be validated.
         * Calls three time SpeciesCrossValidation class - ask Angiras why it is happening?
         * Does not complete validation for ISD type of reaction. For example, in case of RadicalsFilter(0,1) or RadicalsFilter(0,12, sometimes it blocks, but sometimes it generates results for only a few species.
         * 
         */

      SpeciesFilter f = new RadicalsFilter(0,12); // RadicalsFilter(0,12) -  generates/validates 250 species. RadicalsFilter(0,1)  -generates/validates 79 species. RadicalsFilter(o,numberOfRadicals)
      
      f.set(ref);
      
      f.filter();
      
        /**
         * 
         * Validates reference species.
         * 
         */
        
      ref = f.getValidSpecies();
        
      Collections.shuffle((List<Species>) ref);
        
      ValidationFilter redFilter = new ValidationFilter();
        
      redFilter.set(ref);
      
      /**
       * 
       * Inside filter method it compares enthalpies with maxErr.
       *  
       */
      redFilter.filter();
        
      Map<Species, Reaction> invalid = redFilter.getDetailedInvalidSpecies();
      
      Map<Species, Reaction> valid = redFilter.getDetailedValidSpecies();
        
      System.out.println("Invalid:");
        
      for (Species s : invalid.keySet()) {
        
      System.out.println(s.getRef() + " (" + Math.abs(invalid.get(s).calculateHf() - s.getHf()) + ")" + ": " + invalid.get(s).toString());
        
      }
       
//    System.out.println();
        
      System.out.println("Valid:"); 
        
      for (Species s : valid.keySet()) {
        
      System.out.println(s.getRef() + " (" + Math.abs(valid.get(s).calculateHf() - s.getHf()) + ")" + " : " + valid.get(s).toString());
            
        }
    }
}