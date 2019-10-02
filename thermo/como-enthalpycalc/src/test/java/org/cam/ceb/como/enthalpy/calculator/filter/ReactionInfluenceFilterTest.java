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

import org.cam.ceb.como.enthalpy.calculator.io.pool.CSVParser;
import org.cam.ceb.como.enthalpy.calculator.io.pool.SpeciesPoolParser;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ReactionInfluenceFilterTest {
    
    @Test
//    @Ignore
    public void filterTest1() throws FileNotFoundException, IOException, Exception {
        CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/pool_hydrocarbons_complete.csv"));
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        //Collection<Species> soi = parser.getSpeciesOfInterest();

        SpeciesFilter f = new RadicalsFilter(0, 1);
        f.set(ref);
        f.filter();
        ref = f.getValidSpecies();
        Collections.shuffle((List<Species>) ref);
        
//      ReactionInfluenceFilter2 redFilter = new ReactionInfluenceFilter2();
//      redFilter.set(ref);
//      redFilter.filter();
//        
//       Map<Species, Reaction> invalid = redFilter.getDetailedInvalidSpecies();
//       Map<Species, Reaction> valid = redFilter.getDetailedValidSpecies();
//       Map<Species, Reaction> marked = redFilter.getDetailedMarkedSpecies();
//        
//       Map<Species, Double> markedErr = redFilter.getAvgErrorMarkedSpecies();
//       Map<Species, Double> invalidErr = redFilter.getAvgErrorInvalidSpecies();
//       Map<Species, Double> validErr = redFilter.getAvgErrorValidSpecies();
//        
//       System.out.println("Invalid:");        
//       for (Species s : invalid.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + Math.abs(invalid.get(s).calculateHf() - s.getHf()) + ")" + 
//                    ": " + invalid.get(s).toString());
//       }
//        
//       for (Species s : invalidErr.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + invalidErr.get(s) + ")");
//      }
//        
//      System.out.println("Marked:");        
//      for (Species s : marked.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + Math.abs(marked.get(s).calculateHf() - s.getHf()) + ")" + 
//                    ": " + marked.get(s).toString());
//      }
//        
//      for (Species s : markedErr.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + markedErr.get(s) + ")");
//        }
//        
//        System.out.println();
//        System.out.println("Valid:");        
//        for (Species s : valid.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + Math.abs(valid.get(s).calculateHf() - s.getHf()) + ")" + 
//                    ": " + valid.get(s).toString());
//        }
//        
//        for (Species s : validErr.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + validErr.get(s) + ")");
//        }
    }
    
    @Test
    @Ignore
    public void filterTest() throws FileNotFoundException, IOException, Exception {
    	
        CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/issues/pool_hydrocarbons.csv"));
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        //Collection<Species> soi = parser.getSpeciesOfInterest();

        SpeciesFilter f = new RadicalsFilter(0, 1);
        f.set(ref);
        f.filter();
        
        ref = f.getValidSpecies();
        Collections.shuffle((List<Species>) ref);
        
//        ReactionInfluenceFilter redFilter = new ReactionInfluenceFilter();
//        redFilter.set(ref);
//        redFilter.filter();
//        
//        Map<Species, Reaction> invalid = redFilter.getDetailedInvalidSpecies();
//        Map<Species, Reaction> valid = redFilter.getDetailedValidSpecies();
//        
//        System.out.println("Invalid:");        
//        for (Species s : invalid.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + Math.abs(invalid.get(s).calculateHf() - s.getHf()) + ")" + 
//                    ": " + invalid.get(s).toString());
//        }
//        
//        System.out.println();
//        System.out.println("Valid:");        
//        for (Species s : valid.keySet()) {
//            System.out.println(s.getRef() + 
//                    " (" + Math.abs(valid.get(s).calculateHf() - s.getHf()) + ")" + 
//                    ": " + valid.get(s).toString());
//        }
    }
}
