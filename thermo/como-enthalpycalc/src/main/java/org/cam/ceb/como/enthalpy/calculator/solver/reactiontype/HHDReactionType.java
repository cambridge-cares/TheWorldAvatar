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
import org.cam.ceb.como.enthalpy.calculator.species.homodesmotic.HyperhomodesmoticAtom;
import org.cam.ceb.como.enthalpy.calculator.species.homodesmotic.HyperhomodesmoticBond;
import org.cam.ceb.como.enthalpy.calculator.variable.Variable;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableSet;
import org.cam.ceb.como.tools.periodictable.PeriodicTable;

/**
 *
 * @author pb556
 */
public class HHDReactionType extends ReactionType {

    @Override
    public Collection<String> getAllConservationTypes(Species species) {
        Set<String> bonds = new HashSet<String>();
        Multiset<HyperhomodesmoticBond> bSpecies = getHyperhomodesmoticOrbitalHybridisation(species);
        for (HyperhomodesmoticBond b : bSpecies) {
            bonds.add(b.toString());
        }
        return bonds;
    }
    
    @Override
    public Collection<String> getAllConservationTypes(Collection<Species> species) {
        Set<String> bonds = new HashSet<String>();
        for (Species s : species) {
            Multiset<HyperhomodesmoticBond> bSpecies = getHyperhomodesmoticOrbitalHybridisation(s);
            for (HyperhomodesmoticBond b : bSpecies) {
                bonds.add(b.toString());
            }
        }
        return bonds;
    }

    @Override
    public HashMap<Variable, List<Integer>> getConservationTypeConstraints(Collection<Species> species, VariableSet vSet) {
        Collection<String> bondTypes = getAllConservationTypes(species);
        return getConservationConstraints(species, vSet, bondTypes, new HHDConservationConstraintMultisetSelector());
    }
    
    public Multiset<HyperhomodesmoticBond> getHyperhomodesmoticOrbitalHybridisation(Species s) {
        Multiset<HyperhomodesmoticBond> bonds = HashMultiset.create();
        for (Bond b : s.getBondMap()) {
            HyperhomodesmoticAtom atomA = null;
            HyperhomodesmoticAtom atomB = null;
            HashMap<HyperhomodesmoticAtom, BondType> bondsA = new HashMap<HyperhomodesmoticAtom, BondType>();
            HashMap<HyperhomodesmoticAtom, BondType> bondsB = new HashMap<HyperhomodesmoticAtom, BondType>();
            for (String id : s.getAtomMap().keySet()) {
                if (b.getRefAtomA().equals(id)) {
                    atomA = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
                    continue;
                }
                if (b.getRefAtomB().equals(id)) {
                    atomB = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
                }
            }
            if (atomA == null || atomB == null) {
                // error
                System.out.println("INVALID SPECIES!");
            } else {
                for (Bond b1 : s.getBondMap()) {
                    if (b.getRefAtomA().equals(b1.getRefAtomA()) && b.getRefAtomB().equals(b1.getRefAtomB())) {
                        continue;
                    }
                    if (b1.getRefAtomA().equals(b.getRefAtomA()) || b1.getRefAtomB().equals(b.getRefAtomA())) {
                        // add b1 bond to A side
                        // identify opponent atom from b1
                        HyperhomodesmoticAtom opponent = null;
                        if (b1.getRefAtomA().equals(b.getRefAtomA())) {
                            // opponent atom is B
                            for (String id : s.getAtomMap().keySet()) {
                                if (id.equals(b1.getRefAtomB())) {
                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
                                    break;
                                }
                            }
                        } else {
                            // opponent atom is A
                            for (String id : s.getAtomMap().keySet()) {
                                if (id.equals(b1.getRefAtomA())) {
                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
                                    break;
                                }
                            }
                        }
                        if (opponent != null) {
                            bondsA.put(opponent, b1.getBondType());
                        }
                    } else if (b1.getRefAtomA().equals(b.getRefAtomB()) || b1.getRefAtomB().equals(b.getRefAtomB())) {
                        // add b1 bond to B side
                        // identify opponent atom from b1
                        HyperhomodesmoticAtom opponent = null;
                        if (b1.getRefAtomA().equals(b.getRefAtomB())) {
                            // opponent atom is B
                            for (String id : s.getAtomMap().keySet()) {
                                if (id.equals(b1.getRefAtomB())) {
                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
                                    break;
                                }
                            }
                        } else {
                            // opponent atom is A
                            for (String id : s.getAtomMap().keySet()) {
                                if (id.equals(b1.getRefAtomA())) {
                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
                                    break;
                                }
                            }
                        }
                        if (opponent != null) {
                            bondsB.put(opponent, b1.getBondType());
                        }
                    }
                }

                // check if bond is already added
                HyperhomodesmoticBond key = null;
                HyperhomodesmoticBond newBond = new HyperhomodesmoticBond(b.getBondType(), atomA, atomB, bondsA, bondsB);
                for (HyperhomodesmoticBond bondKey : bonds) {
                    if (bondKey.equals(newBond)) {
                        key = bondKey;
                        break;
                    }
                }
                if (key != null) {
                    bonds.add(key, 1);
                } else {
                    bonds.add(newBond, 1);
                }
            }
        }
        return bonds;
    }
    
    protected class HHDConservationConstraintMultisetSelector implements MultisetSelector {

        @Override
        public Multiset<String> getMultiset(Species sp) {
            Multiset<String> ms = HashMultiset.create();
            Multiset<HyperhomodesmoticBond> bonds = getHyperhomodesmoticOrbitalHybridisation(sp);
            for (HyperhomodesmoticBond b : bonds) {
                ms.add(b.toString(), 1);
            }
            return ms;
        }
    }
}
