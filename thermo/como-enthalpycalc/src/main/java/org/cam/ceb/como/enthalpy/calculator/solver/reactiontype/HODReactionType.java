/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver.reactiontype;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.cam.ceb.como.enthalpy.calculator.species.Bond;
import org.cam.ceb.como.enthalpy.calculator.species.BondType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.species.homodesmotic.HypohomodesmoticAtom;
import org.cam.ceb.como.enthalpy.calculator.species.homodesmotic.HypohomodesmoticBond;
import org.cam.ceb.como.enthalpy.calculator.variable.Variable;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableSet;
import org.cam.ceb.como.tools.periodictable.PeriodicTable;

/**
 *
 * @author pb556
 */
public class HODReactionType extends ReactionType {

    @Override
    public Collection<String> getAllConservationTypes(Species species) {
        Set<String> bonds = new HashSet<String>();
        Multiset<HypohomodesmoticAtom> bSpecies = getHypoomodesmoticOrbitalHybridisation(species);
        for (HypohomodesmoticAtom b : bSpecies) {
            bonds.add(b.toString());
        }
        return bonds;
    }

    @Override
    public Collection<String> getAllConservationTypes(Collection<Species> species) {
        Set<String> bonds = new HashSet<String>();
        for (Species s : species) {
            Multiset<HypohomodesmoticAtom> bSpecies = getHypoomodesmoticOrbitalHybridisation(s);
            for (HypohomodesmoticAtom b : bSpecies) {
                bonds.add(b.toString());
            }
        }
        return bonds;
    }

    @Override
    public HashMap<Variable, List<Integer>> getConservationTypeConstraints(Collection<Species> species, VariableSet vSet) {
        Collection<String> bondTypes = getAllConservationTypes(species);
        return getConservationConstraints(species, vSet, bondTypes, new HODConservationConstraintMultisetSelector());
    }

    public Multiset<HypohomodesmoticAtom> getHypoomodesmoticOrbitalHybridisation(Species s) {
        Multiset<HypohomodesmoticAtom> bonds = HashMultiset.create();
        for (String id : s.getAtomMap().keySet()) {
            if (s.getAtomMap().get(id).equals("H")) {
                continue;
            }
            
            // find all the other bonds
            Multiset<HypohomodesmoticBond> ms = HashMultiset.create();
            for (Bond b : s.getBondMap()) {
                if (b.getRefAtomA().equals(id)) { // || b.getRefAtomB().equals(id)) {
                    HypohomodesmoticBond key = null;
                    HypohomodesmoticBond newBond = new HypohomodesmoticBond(PeriodicTable.getElementBySymbol(s.getAtomMap().get(b.getRefAtomB().toString())), b.getBondType());
                    for (HypohomodesmoticBond hodB : ms) {
                        if (hodB.equals(newBond)) {
                            key = hodB;
                            break;
                        }
                    }
                    if (key == null) {
                        ms.add(newBond, 1);
                    } else {
                        ms.add(key, 1);
                    }
                }
                if (b.getRefAtomB().equals(id)) { // || b.getRefAtomB().equals(id)) {
                    HypohomodesmoticBond key = null;
                    HypohomodesmoticBond newBond = new HypohomodesmoticBond(PeriodicTable.getElementBySymbol(s.getAtomMap().get(b.getRefAtomA().toString())), b.getBondType());
                    for (HypohomodesmoticBond hodB : ms) {
                        if (hodB.equals(newBond)) {
                            key = hodB;
                            break;
                        }
                    }
                    if (key == null) {
                        ms.add(newBond, 1);
                    } else {
                        ms.add(key, 1);
                    }
                }
            }

            // check if it is already available or not
            HypohomodesmoticAtom atom = new HypohomodesmoticAtom(PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)), ms);
            HypohomodesmoticAtom key = null;
            for (HypohomodesmoticAtom a : bonds) {
                if (a.equals(atom)) {
                    key = a;
                    break;
                }
            }
            if (key == null) {
                bonds.add(atom, 1);
            } else {
                bonds.add(key, 1);
            }
        }
        return bonds;
    }

    protected class HODConservationConstraintMultisetSelector implements MultisetSelector {

        @Override
        public Multiset<String> getMultiset(Species sp) {
            Multiset<String> ms = HashMultiset.create();
            Multiset<HypohomodesmoticAtom> atoms = getHypoomodesmoticOrbitalHybridisation(sp);
            for (HypohomodesmoticAtom b : atoms) {
                ms.add(b.toString(), 1);
            }
            return ms;
        }
    }
}
