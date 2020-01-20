/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.junit.Test;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */
public class StoichiometryFilterTest {

    @Test
    public void completeISDTest() {
        HashMap<Element, Integer> elements = new HashMap<Element, Integer>();
        elements.put(PeriodicTable.getElementBySymbol("C"), 2);
        elements.put(PeriodicTable.getElementBySymbol("H"), 4);

        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());

        SpeciesFilter f = new StoichiometryFilter(elements, species, StoichiometryFilter.RULE.EXACT);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();

        assert (validSpecies.size() == 1);
        assert (vSpecies.size() == 1);
        assert (invalidSpecies.size() == 7);

        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false));
        }

        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }

        for (Species s : validSpecies) {
            assert (vSpecies.contains(s));
        }

        elements.put(PeriodicTable.getElementBySymbol("O"), 1);
        f = new StoichiometryFilter(elements, species, StoichiometryFilter.RULE.EXACT);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();

        assert (validSpecies.size() == 2);
        assert (vSpecies.size() == 2);
        assert (invalidSpecies.size() == 6);

        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false));
        }

        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }

        for (Species s : validSpecies) {
            assert (vSpecies.contains(s));
        }
    }

    @Test
    public void subsetISDTest() {
        HashMap<Element, Integer> elements = new HashMap<Element, Integer>();
        elements.put(PeriodicTable.getElementBySymbol("C"), 2);
        elements.put(PeriodicTable.getElementBySymbol("H"), 4);

        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());

        SpeciesFilter f = new StoichiometryFilter(elements, species, StoichiometryFilter.RULE.LOWER_THRESHOLD);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();

        assert (validSpecies.size() == 4);
        assert (vSpecies.size() == 4);
        assert (invalidSpecies.size() == 4);

        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false));
        }

        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getCH4O(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6O(), false));
        }

        for (Species s : validSpecies) {
            assert (vSpecies.contains(s));
        }

        elements.clear();
        elements.put(PeriodicTable.getElementBySymbol("C"), 3);
        elements.put(PeriodicTable.getElementBySymbol("H"), 4);
        f = new StoichiometryFilter(elements, species, StoichiometryFilter.RULE.LOWER_THRESHOLD);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();

        assert (validSpecies.size() == 2);
        assert (vSpecies.size() == 2);
        assert (invalidSpecies.size() == 6);

        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false));
        }

        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getCH4O(), false)
                    || s.equals(MockSpecies.getCH4(), false));
        }
    }

    @Test
    public void othersAllowedISDTest() {
        HashMap<Element, Integer> elements = new HashMap<Element, Integer>();
        elements.put(PeriodicTable.getElementBySymbol("C"), 2);
        elements.put(PeriodicTable.getElementBySymbol("H"), 4);

        ArrayList<Species> species = new ArrayList<Species>();
        species.add(MockSpecies.getC2H4());
        species.add(MockSpecies.getC2H4O());
        species.add(MockSpecies.getC2H6());
        species.add(MockSpecies.getC2H6O());
        species.add(MockSpecies.getC3H6());
        species.add(MockSpecies.getC3H8());
        species.add(MockSpecies.getCH4());
        species.add(MockSpecies.getCH4O());

        SpeciesFilter f = new StoichiometryFilter(elements, species, StoichiometryFilter.RULE.UPPER_THRESHOLD);
        Collection<Species> validSpecies = f.filter();
        Collection<Species> invalidSpecies = f.getInvalidSpecies();
        Collection<Species> vSpecies = f.getValidSpecies();

        assert (validSpecies.size() == 2);
        assert (vSpecies.size() == 2);
        assert (invalidSpecies.size() == 6);

        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getCH4(), false));
        }

        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getCH4O(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false));
        }

        for (Species s : validSpecies) {
            assert (vSpecies.contains(s));
        }

        elements.clear();
        elements.put(PeriodicTable.getElementBySymbol("C"), 3);
        elements.put(PeriodicTable.getElementBySymbol("H"), 6);
        f = new StoichiometryFilter(elements, species, StoichiometryFilter.RULE.UPPER_THRESHOLD);
        validSpecies = f.filter();
        invalidSpecies = f.getInvalidSpecies();
        vSpecies = f.getValidSpecies();

        assert (validSpecies.size() == 4);
        assert (vSpecies.size() == 4);
        assert (invalidSpecies.size() == 4);

        for (Species s : validSpecies) {
            assert (s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getCH4(), false));
        }

        for (Species s : invalidSpecies) {
            assert (s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
    }
}