/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.SpeciesFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.ElementFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import java.util.ArrayList;
import java.util.Collection;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ElementFilterTest {
    
    @Test
    public void completeISDTest() {
        ArrayList<Element> elements = new ArrayList<Element>();
        elements.add(PeriodicTable.getElementBySymbol("C"));
        elements.add(PeriodicTable.getElementBySymbol("H"));
        
        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());
        
        SpeciesFilter f = new ElementFilter(elements, species, ElementFilter.RULE.COMPLETE);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 5);
        assert(vSpecies.size() == 5);
        assert(invalidSpecies.size() == 3);
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
        
        elements.add(PeriodicTable.getElementBySymbol("O"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.COMPLETE);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 8);
        assert(vSpecies.size() == 8);
        assert(invalidSpecies.isEmpty());
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
    }
    
    @Test
    public void subsetISDTest() {
        ArrayList<Element> elements = new ArrayList<Element>();
        elements.add(PeriodicTable.getElementBySymbol("C"));
        elements.add(PeriodicTable.getElementBySymbol("H"));
        
        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());
        
        SpeciesFilter f = new ElementFilter(elements, species, ElementFilter.RULE.SUBSET);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 8);
        assert(vSpecies.size() == 8);
        assert(invalidSpecies.isEmpty());
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
        
        elements.clear();
        elements.add(PeriodicTable.getElementBySymbol("Al"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.SUBSET);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.isEmpty());
        assert(vSpecies.isEmpty());
        assert(invalidSpecies.size() == 8);
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
    }
    
    @Test
    public void othersAllowedISDTest() {
        ArrayList<Element> elements = new ArrayList<Element>();
        elements.add(PeriodicTable.getElementBySymbol("C"));
        elements.add(PeriodicTable.getElementBySymbol("H"));
        
        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());
        
        SpeciesFilter f = new ElementFilter(elements, species, ElementFilter.RULE.OTHERS_ALLOWED);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 8);
        assert(vSpecies.size() == 8);
        assert(invalidSpecies.isEmpty());
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
        
        elements.clear();
        elements.add(PeriodicTable.getElementBySymbol("Al"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.OTHERS_ALLOWED);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.isEmpty());
        assert(vSpecies.isEmpty());
        assert(invalidSpecies.size() == 8);
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        elements.add(PeriodicTable.getElementBySymbol("O"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.OTHERS_ALLOWED);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 3);
        assert(vSpecies.size() == 3);
        assert(invalidSpecies.size() == 5);
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
    }
    
    @Test
    public void completeISGTest() {
        ArrayList<Element> elements = new ArrayList<Element>();
        elements.add(PeriodicTable.getElementBySymbol("C"));
        elements.add(PeriodicTable.getElementBySymbol("H"));
        
        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());
        
        SpeciesFilter f = new ElementFilter(elements, species, ElementFilter.RULE.COMPLETE);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 5);
        assert(vSpecies.size() == 5);
        assert(invalidSpecies.size() == 3);
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
        
        elements.add(PeriodicTable.getElementBySymbol("O"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.COMPLETE);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 8);
        assert(vSpecies.size() == 8);
        assert(invalidSpecies.isEmpty());
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
    }
    
    @Test
    public void subsetISGTest() {
        ArrayList<Element> elements = new ArrayList<Element>();
        elements.add(PeriodicTable.getElementBySymbol("C"));
        elements.add(PeriodicTable.getElementBySymbol("H"));
        
        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());
        
        SpeciesFilter f = new ElementFilter(elements, species, ElementFilter.RULE.SUBSET);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 8);
        assert(vSpecies.size() == 8);
        assert(invalidSpecies.isEmpty());
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
        
        elements.clear();
        elements.add(PeriodicTable.getElementBySymbol("Al"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.SUBSET);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.isEmpty());
        assert(vSpecies.isEmpty());
        assert(invalidSpecies.size() == 8);
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
    }
    
    @Test
    public void othersAllowedISGTest() {
        ArrayList<Element> elements = new ArrayList<Element>();
        elements.add(PeriodicTable.getElementBySymbol("C"));
        elements.add(PeriodicTable.getElementBySymbol("H"));
        
        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());
        
        SpeciesFilter f = new ElementFilter(elements, species, ElementFilter.RULE.OTHERS_ALLOWED);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 8);
        assert(vSpecies.size() == 8);
        assert(invalidSpecies.isEmpty());
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
        
        elements.clear();
        elements.add(PeriodicTable.getElementBySymbol("Al"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.OTHERS_ALLOWED);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.isEmpty());
        assert(vSpecies.isEmpty());
        assert(invalidSpecies.size() == 8);
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        elements.add(PeriodicTable.getElementBySymbol("O"));
        f = new ElementFilter(elements, species, ElementFilter.RULE.OTHERS_ALLOWED);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();
        
        assert(validSpecies.size() == 3);
        assert(vSpecies.size() == 3);
        assert(invalidSpecies.size() == 5);
        
        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false) ||
                    s.equals(MockSpecies.getC2H6(), false) ||
                    s.equals(MockSpecies.getC3H6(), false) ||
                    s.equals(MockSpecies.getC3H8(), false) ||
                    s.equals(MockSpecies.getCH4(), false));
        }
        
        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4O(), false) ||
                    s.equals(MockSpecies.getC2H6O(), false) ||
                    s.equals(MockSpecies.getCH4O(), false));
        }
        
        for (Species s : validSpecies) {
            assert(vSpecies.contains(s));
        }
    }
}
