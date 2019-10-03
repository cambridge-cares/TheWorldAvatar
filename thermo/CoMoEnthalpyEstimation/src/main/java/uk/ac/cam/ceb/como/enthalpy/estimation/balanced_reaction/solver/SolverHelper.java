/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Bond;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.BondType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 * 
 */
public class SolverHelper {

    private static Map<String, Integer[]> isgMap = new HashMap<String, Integer[]>();
    
    public static Collection<Element> getAllElements(Collection<Species> species) {
        Collection<String> elementSymbols = getAllElementSymbols(species);
        Set<Element> elements = new HashSet<Element>();
        for (String eStr : elementSymbols) {
            elements.add(PeriodicTable.getElementBySymbol(eStr));
        }
        return elements;
    }

    public static Collection<String> getAllElementSymbols(Collection<Species> species) {
        Collection elements = new HashSet();
        for (Species s : species) {
            for (Element e : s.getAtomMultiset().elementSet()) {
                elements.add(e.getSymbol());
            }
        }
        return elements;
    }

    public static Collection<Element> getAllElements(Species species) {
        Collection elements = new HashSet();
        for (Element e : species.getAtomMultiset().elementSet()) {
            elements.add(e);
        }
        return elements;
    }

    public static Collection<String> getAllElementSymbols(Species species) {
        Collection elements = new HashSet();
        for (Element e : species.getAtomMultiset().elementSet()) {
            elements.add(e.getSymbol());
        }
        return elements;
    }

    public static List<Species> combine(Species targetSpecies, Collection<Species> refSpecies) {
        List<Species> fullSpeciesList = new ArrayList<Species>(refSpecies);
        // target species must be the first in the list
        fullSpeciesList.add(0, targetSpecies);
        return fullSpeciesList;
    }

    public static String concatinate(List<String> terms, String beginning, String delimiter, String ending) {
    	
        StringBuilder objSb = new StringBuilder(beginning);
        
        // concaternate all terms
        for (Iterator<String> it = terms.iterator(); it.hasNext();) {
        	
            String term = it.next();
            
            objSb.append(term);
            
            if (it.hasNext()) {
            	
                objSb.append(delimiter);
                
            } else {
            	
                objSb.append(ending);
            }
        }
        
        return objSb.toString();
    }

    public static void add(Map<String, Integer[]> m) {
        isgMap.putAll(m);
    }
    
    public static int getNumberOfPairedElectrons(Species sp) {
        
        for (String id : isgMap.keySet()) {
            if (id.compareTo(sp.getRef()) == 0) {
                return isgMap.get(id)[0].intValue();
            }
        }
        
        // not valid!!!
        int bondElectrons = 0;
        for (Bond b : sp.getBondMap()) {
            switch (b.getBondType()) {
                case AROMATIC:
                    bondElectrons += 1;
                    break;
                case SINGLE:
                    bondElectrons += 1;
                    break;
                case DOUBLE:
                    bondElectrons += 2;
                    break;
                case TRIPLE:
                    bondElectrons += 3;
                    break;
            }
        }
        
        if (getNumberOfUnpairedOrbitalElectrons(sp) - bondElectrons * 2 < 0) {
        	
            return getNumberOfPairedOrbitalElectrons(sp) + 2 * bondElectrons +  2 * (getNumberOfUnpairedOrbitalElectrons(sp) - 2 * bondElectrons);   // electron promotion is regarded
        }
        
        return getNumberOfPairedOrbitalElectrons(sp) + 2 * bondElectrons;   // + getNumberOfLonePairs(sp);
    }

    public static int getNumberOfUnpairedElectrons(Species sp) {
        
        for (String id : isgMap.keySet()) {
        	
            if (id.compareTo(sp.getRef()) == 0) {
            	
                return isgMap.get(id)[1].intValue();
            }
        }
        
        int bondElectrons = 0;
        
        for (Bond b : sp.getBondMap()) {
        	
            switch (b.getBondType()) {
                case AROMATIC:
                    bondElectrons += 1;
                    break;
                case SINGLE:
                    bondElectrons += 1;
                    break;
                case DOUBLE:
                    bondElectrons += 2;
                    break;
                case TRIPLE:
                    bondElectrons += 3;
                    break;
            }
        }
        
        getNumberOfUnpairedOrbitalElectrons(sp);
        
        return Math.abs(getNumberOfUnpairedOrbitalElectrons(sp) - bondElectrons * 2); // example C - 2 unpaired but can make 4 bonds CH3 would have -1 unpaired electron otherwise instead of 1 (known as electron promotion)
        
    }
    
    public static int getNumberOfUnpairedOrbitalElectrons(Species sp) {
        int num = 0;
        for (Object ref : sp.getElementMap().keySet()) {
            Element element = (Element) sp.getElementMap().get(ref.toString());
            num += element.getNumberOfUnpairedElectrons();
        }
        return num;
    }

    public static int getNumberOfPairedOrbitalElectrons(Species sp) {
        int num = 0;
        for (Object ref : sp.getElementMap().keySet()) {
            Element element = (Element) sp.getElementMap().get(ref.toString());
            num += element.getNumberOfPairedElectrons();
        }
        return num;
    }

    public static int getNumberOfLonePairs(Species sp) {
        int num = 0;
        int numBonds;
        for (Object ref : sp.getElementMap().keySet()) {
            numBonds = 0;
            for (Object o : sp.getBondMap()) {
                Bond b = (Bond) o;
                if (b.contains(ref)) {
                    numBonds++;
                }
            }
            num += PeriodicTable.getNumberOfLonePairs((Element) sp.getElementMap().get(ref.toString()), numBonds);
        }
        return num;
    }

    public static int getNumberOfBondPairs(Species sp, String ref) {
//        int num = 0;
//        for (Object ref : sp.getAtomMap().keySet()) {
//            for (Bond b : sp.getBondMap()) {
//                if (b.contains(ref)) {
//                    num++;
//                }
//            }
//        }
        int ctr = 0;
        for (Bond b : sp.getBondMap()) {
            if (b.getRefAtomA().equals(ref) || b.getRefAtomB().equals(ref)) {
                if (b.getBondType() == BondType.SINGLE) {
                    ctr += 1;
                }
                if (b.getBondType() == BondType.DOUBLE) {
                    ctr += 2;
                }
                if (b.getBondType() == BondType.TRIPLE) {
                    ctr += 3;
                }
            }
        }
        return ctr;
    }

    public static int countBondType(String bondType, Species sp) {
        int ctr = 0;
        for (String b : sp.getBondTypeMultiset()) {
            if (b.equals(bondType)) {
                ctr++;
            }
        }
        return ctr;
    }

    public synchronized static ObjectPool clone(ObjectPool pool) {
    	
        ObjectPool<Species> clone = new ObjectPool<Species>();
        
        for (Object o : pool.getValidatedObjects()) {
        	
            Species s = (Species) o;
            
            clone.add(s.clone(), Boolean.FALSE);
            
//          System.out.println("SolverHelper class: ObjectPool clone() method: " + s.getRef() + " , " + s.getHf() + " s.clone().getRef(): " + s.clone().getRef());
            
        }
        
        for (Object o : pool.getInvalidatedObjects()) {
        	
            Species s = (Species) o;
            
            clone.add(s.clone(), Boolean.TRUE);
        }
        
        return clone;
    }
//    public static Set<String> getAllBondTypes(Species targetSpecies, Set<Species> refSpecies) {
//        Set<String> elements = new HashSet<String>();
////        for (Bond b : targetSpecies.getBondTypeMultiset().elementSet()) {
////            elements.add(b);
////        }
//        //elements.addAll(targetSpecies.getBondTypeMultiset().elementSet());
//        for (Species sp : refSpecies) {
//            for (String b : sp.getBondTypeMultiset().elementSet()) {
//                elements.add(b);
//            }
//            //elements.addAll(sp.getBondTypeMultiset().elementSet());
//        }
//        for (String b : targetSpecies.getBondTypeMultiset().elementSet()) {
//            elements.add(b);
//        }
//        return elements;
//    }
//    private static interface MultisetSelector {
//
//        public Multiset getMultiset(Species sp);
//    }
//
//    private static class AtomMultisetSelector implements MultisetSelector {
//
//        @Override
//        public Multiset<Element> getMultiset(Species sp) {
//            return sp.getAtomMultiset();
//        }
//    }
//
//    private static class BondMultisetSelector implements MultisetSelector {
//
//        @Override
//        public Multiset<String> getMultiset(Species sp) {
//            return sp.getBondTypeMultiset();
//        }
//    }
//
//    private static class HODBondMultisetSelector implements MultisetSelector {
//
//        @Override
//        public Multiset<String> getMultiset(Species sp) {
//            Multiset<String> ms = HashMultiset.create();
//            Map<HypoomodesmoticAtom, Integer> atoms = getHypoomodesmoticOrbitalHybridisation(sp);
//            for (HypoomodesmoticAtom b : atoms.keySet()) {
//                ms.add(b.toString(), atoms.get(b));
//            }
//            return ms;
//        }
//    }
//    
//    private static class HDBondMultisetSelector implements MultisetSelector {
//
//        @Override
//        public Multiset<String> getMultiset(Species sp) {
//            Multiset<String> ms = HashMultiset.create();
//            Map<HomodesmoticBond, Integer> bonds = getHomodesmoticOrbitalHybridisation(sp);
//            for (HomodesmoticBond b : bonds.keySet()) {
//                ms.add(b.toString(), bonds.get(b));
//            }
//            return ms;
//        }
//    }
//
//    private static class HHDBondMultisetSelector implements MultisetSelector {
//
//        @Override
//        public Multiset<String> getMultiset(Species sp) {
//            Multiset<String> ms = HashMultiset.create();
//            Map<HyperhomodesmoticBond, Integer> bonds = getHyperhomodesmoticOrbitalHybridisation(sp);
//            for (HyperhomodesmoticBond b : bonds.keySet()) {
//                ms.add(b.toString(), bonds.get(b));
//            }
//            return ms;
//        }
//    }
//
//    public static HashMap<Variable, List<Integer>> getMassBalanceConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet) {
//        Set<String> elements = SolverHelper.getAllElements(targetSpecies, speciesSet);
//        return getConservationConstraints(targetSpecies, speciesSet, vSet, elements, new AtomMultisetSelector());
//    }
//
//    public static HashMap<Variable, List<Integer>> getBondTypeConvervationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet) {
//        Set<String> bondTypes = SolverHelper.getAllBondTypes(targetSpecies, speciesSet);
//        return getConservationConstraints(targetSpecies, speciesSet, vSet, bondTypes, new BondMultisetSelector());
//    }
//
//    public static HashMap<Variable, List<Integer>> getHDBondTypeConvervationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet) {
//        Set<String> bondTypes = SolverHelper.getAllHDBondTypes(targetSpecies, speciesSet);
//        return getConservationConstraints(targetSpecies, speciesSet, vSet, bondTypes, new HDBondMultisetSelector());
//    }
//
//    public static HashMap<Variable, List<Integer>> getHHDBondTypeConvervationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet) {
//        Set<String> bondTypes = SolverHelper.getAllHHDBondTypes(targetSpecies, speciesSet);
//        return getConservationConstraints(targetSpecies, speciesSet, vSet, bondTypes, new HHDBondMultisetSelector());
//    }
//    
//    public static HashMap<Variable, List<Integer>> getHODBondTypeConvervationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet) {
//        Set<String> bondTypes = SolverHelper.getAllHODBondTypes(targetSpecies, speciesSet);
//        return getConservationConstraints(targetSpecies, speciesSet, vSet, bondTypes, new HODBondMultisetSelector());
//    }
//
//    private static HashMap<Variable, List<Integer>> getConservationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet, Set constraintSet, MultisetSelector ms) {
//        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);
//        HashMap<Variable, List<Integer>> constraintBalance = new HashMap<Variable, List<Integer>>();
//        for (Object constraint : constraintSet) {
//            //logger.trace("Writing convervation equation for constraint of type : " + constraint);
//            // make a string of each term
//            for (Iterator<Species> it = allSpeciesList.iterator(); it.hasNext();) {
//                Species sp = it.next();
//                Variable variable = vSet.getVariableOf(sp);
//                int count = ms.getMultiset(sp).count(constraint);
//                if (!constraintBalance.containsKey(variable)) {
//                    constraintBalance.put(variable, new ArrayList<Integer>());
//                }
//                constraintBalance.get(variable).add(count);
//            }
//        }
//        return constraintBalance;
//    }
//
//    public static HashMap<Variable, List<Integer>> getPairedElectronenConvervationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet) {
//        return getElectronenPairConvervationConstraints(targetSpecies, speciesSet, vSet, true);
//    }
//
//    public static HashMap<Variable, List<Integer>> getUnpairedElectronenConvervationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet) {
//        return getElectronenPairConvervationConstraints(targetSpecies, speciesSet, vSet, false);
//    }
//
//    private static HashMap<Variable, List<Integer>> getElectronenPairConvervationConstraints(Species targetSpecies, Set<Species> speciesSet, VariableSet vSet, boolean paired) {
//        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);
//        HashMap<Variable, List<Integer>> constraintBalance = new HashMap<Variable, List<Integer>>();
//        //ISGMPSFormat1.MultisetSelector ms = new ISGMPSFormat1.AtomMultisetSelector();
//        //logger.trace("Writing convervation equation for constraint of type : " + constraint);
//        // make a string of each term
//        for (Iterator<Species> it = allSpeciesList.iterator(); it.hasNext();) {
//            Species sp = it.next();
//            Variable variable = vSet.getVariableOf(sp);
//            int count = getNumberOfPairedElectrons((ISDSpecies) sp);
//            if (!paired) {
//                count = getNumberOfUnpairedElectrons((ISDSpecies) sp);
//            }
//            if (!constraintBalance.containsKey(variable)) {
//                constraintBalance.put(variable, new ArrayList<Integer>());
//            }
//            constraintBalance.get(variable).add(count);
//        }
//        return constraintBalance;
//    }
    // this is one species
//    public static boolean compare(Map<HomodesmoticCentre, Double> sideA, Map<HomodesmoticCentre, Double> sideB) {
//        if (sideA.size() == sideB.size()) {
//            for (HomodesmoticCentre aA : sideA.keySet()) {
//                boolean identified = false;
//                for (HomodesmoticCentre aB : sideB.keySet()) {
//                    if (aA.equals(aB)) {
//                        identified = true;
//                        if (Math.abs(sideA.get(aA) - sideB.get(aB)) > 0.01) {
//                            return false;
//                        }
//                    }
//                }
//                if (!identified) {
//                    return false;
//                }
//            }
//            return true;
//        }
//        return false;
//    }
//    public static Set<String> getAllHODBondTypes(Species targetSpecies, Set<Species> refSpecies) {
//        Set<String> bonds = new HashSet<String>();
//        for (Species s : refSpecies) {
//            Map<HypoomodesmoticAtom, Integer> bSpecies = getHypoomodesmoticOrbitalHybridisation(s);
//            for (HypoomodesmoticAtom b : bSpecies.keySet()) {
//                bonds.add(b.toString());
//            }
//        }
//        Map<HypoomodesmoticAtom, Integer> bSpecies = getHypoomodesmoticOrbitalHybridisation(targetSpecies);
//        for (HypoomodesmoticAtom b : bSpecies.keySet()) {
//            bonds.add(b.toString());
//        }
//        return bonds;
//    }
//
//    public static Set<String> getAllHODBondTypes(Species targetSpecies) {
//        Set<String> bonds = new HashSet<String>();
//        Map<HypoomodesmoticAtom, Integer> bSpecies = getHypoomodesmoticOrbitalHybridisation(targetSpecies);
//        for (HypoomodesmoticAtom b : bSpecies.keySet()) {
//            bonds.add(b.toString());
//        }
//        return bonds;
//    }
//
//    public static Set<String> getAllHDBondTypes(Species targetSpecies, Set<Species> refSpecies) {
//        Set<String> bonds = new HashSet<String>();
//        for (Species s : refSpecies) {
//            Map<HomodesmoticBond, Integer> bSpecies = getHomodesmoticOrbitalHybridisation(s);
//            for (HomodesmoticBond b : bSpecies.keySet()) {
//                bonds.add(b.toString());
//            }
//        }
//        Map<HomodesmoticBond, Integer> bSpecies = getHomodesmoticOrbitalHybridisation(targetSpecies);
//        for (HomodesmoticBond b : bSpecies.keySet()) {
//            bonds.add(b.toString());
//        }
//        return bonds;
//    }
//
//    public static Set<String> getAllHDBondTypes(Species targetSpecies) {
//        Set<String> bonds = new HashSet<String>();
//        Map<HomodesmoticBond, Integer> bSpecies = getHomodesmoticOrbitalHybridisation(targetSpecies);
//        for (HomodesmoticBond b : bSpecies.keySet()) {
//            bonds.add(b.toString());
//        }
//        return bonds;
//    }
//
//    public static Set<String> getAllHHDBondTypes(Species targetSpecies, Set<Species> refSpecies) {
//        Set<String> bonds = new HashSet<String>();
//        for (Species s : refSpecies) {
//            Map<HyperhomodesmoticBond, Integer> bSpecies = getHyperhomodesmoticOrbitalHybridisation(s);
//            for (HyperhomodesmoticBond b : bSpecies.keySet()) {
//                bonds.add(b.toString());
//            }
//        }
//        Map<HyperhomodesmoticBond, Integer> bSpecies = getHyperhomodesmoticOrbitalHybridisation(targetSpecies);
//        for (HyperhomodesmoticBond b : bSpecies.keySet()) {
//            bonds.add(b.toString());
//        }
//        return bonds;
//    }
//
//    public static Set<String> getAllHHDBondTypes(Species targetSpecies) {
//        Set<String> bonds = new HashSet<String>();
//        Map<HyperhomodesmoticBond, Integer> bSpecies = getHyperhomodesmoticOrbitalHybridisation(targetSpecies);
//        for (HyperhomodesmoticBond b : bSpecies.keySet()) {
//            bonds.add(b.toString());
//        }
//        return bonds;
//    }
//
//    public static boolean compare(Map<HomodesmoticBond, Integer> sideA, Map<HomodesmoticBond, Integer> sideB) {
//        if (sideA.size() == sideB.size()) {
//            for (HomodesmoticBond aA : sideA.keySet()) {
//                boolean identified = false;
//                for (HomodesmoticBond aB : sideB.keySet()) {
//                    if (aA.equals(aB)) {
//                        identified = true;
//                        if (Math.abs(sideA.get(aA) - sideB.get(aB)) != 0) {
//                            return false;
//                        }
//                    }
//                }
//                if (!identified) {
//                    return false;
//                }
//            }
//            return true;
//        }
//        return false;
//    }
//
//    public static Map<HomodesmoticBond, Integer> getHomodesmoticHybridisation(Collection<Species> species) {
//        Map<HomodesmoticBond, Integer> hybridisation = new HashMap<HomodesmoticBond, Integer>();
//        for (Species s : species) {
//            Map<HomodesmoticBond, Integer> bonds = getHomodesmoticOrbitalHybridisation(s);
//            for (HomodesmoticBond b : bonds.keySet()) {
//                boolean identified = false;
//                for (HomodesmoticBond combinedBond : hybridisation.keySet()) {
//                    if (b.equals(combinedBond)) {
//                        identified = true;
//                        hybridisation.put(combinedBond, hybridisation.get(combinedBond) + bonds.get(b));
//                    }
//                }
//                if (!identified) {
//                    hybridisation.put(b, bonds.get(b));
//                }
//            }
//        }
//        return hybridisation;
//    }
//
//    public static Map<HomodesmoticBond, Integer> getHomodesmoticOrbitalHybridisation(Species s) {
//        Map<HomodesmoticBond, Integer> bonds = new HashMap<HomodesmoticBond, Integer>();
//        for (Bond b : s.getBondMap()) {
//            String elementA = null;
//            String elementB = null;
//            String idA = null;
//            String idB = null;
//            for (String id : s.getAtomMap().keySet()) {
//                if (b.getRefAtomA().equals(id)) {
//                    elementA = s.getAtomMap().get(id);
//                    idA = id;
//                    continue;
//                }
//                if (b.getRefAtomB().equals(id)) {
//                    elementB = s.getAtomMap().get(id);
//                    idB = id;
//                }
//            }
//            if (elementA == null || elementB == null) {
//                // error
//                System.out.println("INVALID SPECIES!");
//            } else {
////                if (elementA.equals("H") || elementB.equals("H")) {
////                    continue;
////                }
//                int numBondsA = 0;
//                int numBondsB = 0;
//                for (Bond b1 : s.getBondMap()) {
//                    if (b.getRefAtomA().equals(b1.getRefAtomA()) && b.getRefAtomB().equals(b1.getRefAtomB())) {
//                        continue;
//                    }
//                    if (b1.contains(b.getRefAtomA())) {
//                        if (b1.getBondType() == BondType.SINGLE || b1.getBondType() == BondType.AROMATIC) {
//                            numBondsA++;
//                        }
//                        if (b1.getBondType() == BondType.DOUBLE) {
//                            numBondsA += 2;
//                        }
//                        if (b1.getBondType() == BondType.TRIPLE) {
//                            numBondsA += 3;
//                        }
//                    }
//                    if (b1.contains(b.getRefAtomB())) {
//                        if (b1.getBondType() == BondType.SINGLE || b1.getBondType() == BondType.AROMATIC) {
//                            numBondsB++;
//                        }
//                        if (b1.getBondType() == BondType.DOUBLE) {
//                            numBondsB += 2;
//                        }
//                        if (b1.getBondType() == BondType.TRIPLE) {
//                            numBondsB += 3;
//                        }
//                    }
//                }
//
//                // check if bond is already added
//                HomodesmoticBond key = null;
//                HomodesmoticBond newBond = new HomodesmoticBond(b.getBondType(),
//                        PeriodicTable.getElementBySymbol(s.getAtomMap().get(idA)),
//                        PeriodicTable.getElementBySymbol(s.getAtomMap().get(idB)),
//                        numBondsA, numBondsB);
//                for (HomodesmoticBond bondKey : bonds.keySet()) {
//                    if (bondKey.equals(newBond)) {
//                        key = bondKey;
//                        break;
//                    }
//                }
//                if (key != null) {
//                    bonds.put(key, bonds.get(key) + 1);
//                } else {
//                    bonds.put(newBond, 1);
//                }
//            }
//        }
//        Map<HomodesmoticBond, Integer> filtered = new HashMap<HomodesmoticBond, Integer>();
//        for (HomodesmoticBond b : bonds.keySet()) {
//            if (b.getElementA().getSymbol().equalsIgnoreCase("H")
//                    || b.getElementB().getSymbol().equalsIgnoreCase("H")) {
//                continue;
//            } else {
//                filtered.put(b, bonds.get(b));
//            }
//        }
//        return filtered;
//    }
//
//    public static Map<HyperhomodesmoticBond, Integer> getHyperhomodesmoticOrbitalHybridisation(Species s) {
//        Map<HyperhomodesmoticBond, Integer> bonds = new HashMap<HyperhomodesmoticBond, Integer>();
//        for (Bond b : s.getBondMap()) {
//            HyperhomodesmoticAtom atomA = null;
//            HyperhomodesmoticAtom atomB = null;
//            HashMap<HyperhomodesmoticAtom, BondType> bondsA = new HashMap<HyperhomodesmoticAtom, BondType>();
//            HashMap<HyperhomodesmoticAtom, BondType> bondsB = new HashMap<HyperhomodesmoticAtom, BondType>();
//            for (String id : s.getAtomMap().keySet()) {
//                if (b.getRefAtomA().equals(id)) {
//                    atomA = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
//                    continue;
//                }
//                if (b.getRefAtomB().equals(id)) {
//                    atomB = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
//                }
//            }
//            if (atomA == null || atomB == null) {
//                // error
//                System.out.println("INVALID SPECIES!");
//            } else {
//                for (Bond b1 : s.getBondMap()) {
//                    if (b.getRefAtomA().equals(b1.getRefAtomA()) && b.getRefAtomB().equals(b1.getRefAtomB())) {
//                        continue;
//                    }
//                    if (b1.getRefAtomA().equals(b.getRefAtomA()) || b1.getRefAtomB().equals(b.getRefAtomA())) {
//                        // add b1 bond to A side
//                        // identify opponent atom from b1
//                        HyperhomodesmoticAtom opponent = null;
//                        if (b1.getRefAtomA().equals(b.getRefAtomA())) {
//                            // opponent atom is B
//                            for (String id : s.getAtomMap().keySet()) {
//                                if (id.equals(b1.getRefAtomB())) {
//                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
//                                    break;
//                                }
//                            }
//                        } else {
//                            // opponent atom is A
//                            for (String id : s.getAtomMap().keySet()) {
//                                if (id.equals(b1.getRefAtomA())) {
//                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
//                                    break;
//                                }
//                            }
//                        }
//                        if (opponent != null) {
//                            bondsA.put(opponent, b1.getBondType());
//                        }
//                    } else if (b1.getRefAtomA().equals(b.getRefAtomB()) || b1.getRefAtomB().equals(b.getRefAtomB())) {
//                        // add b1 bond to B side
//                        // identify opponent atom from b1
//                        HyperhomodesmoticAtom opponent = null;
//                        if (b1.getRefAtomA().equals(b.getRefAtomB())) {
//                            // opponent atom is B
//                            for (String id : s.getAtomMap().keySet()) {
//                                if (id.equals(b1.getRefAtomB())) {
//                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
//                                    break;
//                                }
//                            }
//                        } else {
//                            // opponent atom is A
//                            for (String id : s.getAtomMap().keySet()) {
//                                if (id.equals(b1.getRefAtomA())) {
//                                    opponent = new HyperhomodesmoticAtom(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)));
//                                    break;
//                                }
//                            }
//                        }
//                        if (opponent != null) {
//                            bondsB.put(opponent, b1.getBondType());
//                        }
//                    }
//                }
//
//                // check if bond is already added
//                HyperhomodesmoticBond key = null;
//                HyperhomodesmoticBond newBond = new HyperhomodesmoticBond(b.getBondType(), atomA, atomB, bondsA, bondsB);
//                for (HyperhomodesmoticBond bondKey : bonds.keySet()) {
//                    if (bondKey.equals(newBond)) {
//                        key = bondKey;
//                        break;
//                    }
//                }
//                if (key != null) {
//                    bonds.put(key, bonds.get(key) + 1);
//                } else {
//                    bonds.put(newBond, 1);
//                }
//            }
//        }
//        return bonds;
//    }
//
//    public static Map<HypoomodesmoticAtom, Integer> getHypoomodesmoticOrbitalHybridisation(Species s) {
//        Map<HypoomodesmoticAtom, Integer> bonds = new HashMap<HypoomodesmoticAtom, Integer>();
//        for (String id : s.getAtomMap().keySet()) {
//            // find all the other bonds
//            Multiset<BondType> ms = HashMultiset.create();
//            for (Bond b : s.getBondMap()) {
//                if (b.getRefAtomA().equals(id) || b.getRefAtomB().equals(id)) {
//                    ms.add(b.getBondType());
//                }
//            }
//
//            // check if it is already available or not
//            HypoomodesmoticAtom atom = new HypoomodesmoticAtom(PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)), ms);
//            HypoomodesmoticAtom key = null;
//            for (HypoomodesmoticAtom a : bonds.keySet()) {
//                if (a.equals(atom)) {
//                    key = a;
//                    break;
//                }
//            }
//            if (key == null) {
//                bonds.put(atom, 1);
//            } else {
//                bonds.put(key, bonds.get(key) + 1);
//            }
//            
//        }
//        return bonds;
//    }
//    public static  Collection<HomodesmoticCentre> getOrbitalHybridisation(Species s) {
//        ArrayList<HomodesmoticCentre> atoms = new ArrayList<HomodesmoticCentre>();
//        for (String id : s.getAtomMap().keySet()) {
//            Map<HomodesmoticBond, Double> bonds = new HashMap<HomodesmoticBond, Double>();
//            for (Bond b : s.getBondMap()) {
//                if (b.getRefAtomA().equals(id) || b.getRefAtomB().equals(id)) {
//                    String opponentId = b.getRefAtomB().toString();
//                    if (b.getRefAtomB().equals(id)) {
//                        opponentId = b.getRefAtomA().toString();
//                    }
//                    
//                    // using this results in hyperhomodesmic reactions!!!
//                    if (s.getAtomMap().get(opponentId).equals("H")) {
//                        
//                    } else {
//                        HomodesmoticBond key = null;
//                        HomodesmoticBond newBond = new HomodesmoticBond(b.getBondType(), PeriodicTable.getElementBySymbol(s.getAtomMap().get(opponentId)));
//                        for (HomodesmoticBond bondKey : bonds.keySet()) {
//                            if (bondKey.equals(newBond)) {
//                                key = bondKey;
//                                break;
//                            }
//                        }
//                        if (key != null) {
//                            bonds.put(key, bonds.get(key) + 1);
//                        } else {
//                            bonds.put(newBond, 1.0);
//                        }
//                    }
//                }
//            }
//            if (bonds.size() > 1) {
//                atoms.add(new HomodesmoticCentre(id, PeriodicTable.getElementBySymbol(s.getAtomMap().get(id)), bonds));
//            }
//        }
//        return atoms;
//    }
}
