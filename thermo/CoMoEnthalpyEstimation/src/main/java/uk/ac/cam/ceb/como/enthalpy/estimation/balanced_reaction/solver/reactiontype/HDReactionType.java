/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
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
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.BondType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.homodesmotic.HomodesmoticBond;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 * 
 */

public class HDReactionType extends ReactionType {

    @Override
    public Collection<String> getAllConservationTypes(Species species) {
    	
        Set<String> bonds = new HashSet<String>();
        
        Multiset<HomodesmoticBond> bSpecies = getHomodesmoticOrbitalHybridisation(species);
        
        for (HomodesmoticBond b : bSpecies) {
        	
            bonds.add(b.toString());
        }
        
        return bonds;
    }

    @Override
    public Collection<String> getAllConservationTypes(Collection<Species> species) {
    	
        Set<String> bonds = new HashSet<String>();
        
        for (Species s : species) {
        	
            Multiset<HomodesmoticBond> bSpecies = getHomodesmoticOrbitalHybridisation(s);
            
            for (HomodesmoticBond b : bSpecies) {
            	
                bonds.add(b.toString());
            }
        }
        
        return bonds;
    }

    @Override
    public HashMap<Variable, List<Integer>> getConservationTypeConstraints(Collection<Species> species, VariableSet vSet) {
        Collection<String> bondTypes = getAllConservationTypes(species);
        return getConservationConstraints(species, vSet, bondTypes, new HDConservationConstraintMultisetSelector());
    }

    public Multiset<HomodesmoticBond> getHomodesmoticOrbitalHybridisation(Species s){
        
    	Multiset<HomodesmoticBond> bonds = HashMultiset.create();
        
        for(Bond b : s.getBondMap()) {
        	
            String elementA = null;
            String elementB = null;
            
            String idA = null;
            String idB = null;
            
            for (String id : s.getAtomMap().keySet()) {
            	
                if (b.getRefAtomA().equals(id)) {
                	
                    elementA = s.getAtomMap().get(id);
                    
                    idA = id;
                    
                    continue;
                }
                
                if (b.getRefAtomB().equals(id)) {
                	
                    elementB = s.getAtomMap().get(id);
                    
                    idB = id;
                }
            }
            
            if (elementA == null || elementB == null) {
                
            	// error
                System.out.println("INVALID SPECIES!");
                
            } else {
                
            	int numBondsA = 0;
                int numBondsB = 0;
                
                for (Bond b1 : s.getBondMap()) {
                    
                	if (b.getRefAtomA().equals(b1.getRefAtomA()) && b.getRefAtomB().equals(b1.getRefAtomB())) {
                        continue;
                    }
                    
                    if (b1.contains(b.getRefAtomA())) {
                    	
                        if (b1.getBondType() == BondType.SINGLE || b1.getBondType() == BondType.AROMATIC) {
                            numBondsA++;
                        }
                        if (b1.getBondType() == BondType.DOUBLE) {
                            numBondsA += 2;
                        }
                        if (b1.getBondType() == BondType.TRIPLE) {
                            numBondsA += 3;
                        }
                    }
                    
                    if (b1.contains(b.getRefAtomB())) {
                    	
                        if (b1.getBondType() == BondType.SINGLE || b1.getBondType() == BondType.AROMATIC) {
                            numBondsB++;
                        }
                        if (b1.getBondType() == BondType.DOUBLE) {
                            numBondsB += 2;
                        }
                        if (b1.getBondType() == BondType.TRIPLE) {
                            numBondsB += 3;
                        }
                    }
                }

                // check if bond is already added
                HomodesmoticBond key = null;
                HomodesmoticBond newBond = new HomodesmoticBond(b.getBondType(),
                        PeriodicTable.getElementBySymbol(s.getAtomMap().get(idA)),
                        PeriodicTable.getElementBySymbol(s.getAtomMap().get(idB)),
                        numBondsA, numBondsB);
                for (HomodesmoticBond bondKey : bonds) {
                    if (bondKey.equals(newBond)) {
                        key = bondKey;
                        break;
                    }
                }
                if (key != null) {
                    bonds.add(key, +1);
                } else {
                    bonds.add(newBond, 1);
                }
            }
        }
     
        Multiset<HomodesmoticBond> filtered = HashMultiset.create();
        for (HomodesmoticBond b : bonds) {
            if (b.getElementA().getSymbol().equalsIgnoreCase("H")
                    || b.getElementB().getSymbol().equalsIgnoreCase("H")) {
                continue;
            } else {
                filtered.add(b, 1);
            }
        }
        return filtered;
    }

    protected class HDConservationConstraintMultisetSelector implements MultisetSelector {

        @Override
        public Multiset<String> getMultiset(Species sp) {
            Multiset<String> ms = HashMultiset.create();
            Multiset<HomodesmoticBond> bonds = getHomodesmoticOrbitalHybridisation(sp);
            for (HomodesmoticBond b : bonds) {
                ms.add(b.toString(), 1);
            }
            
            return ms;
        }
    }
}