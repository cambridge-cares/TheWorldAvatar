/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;

import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Bond;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.homodesmotic.HypohomodesmoticAtom;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.homodesmotic.HypohomodesmoticBond;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

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
