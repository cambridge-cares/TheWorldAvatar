
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.paper.enthalpy.reduction;

import uk.ac.cam.ceb.como.paper.enthalpy.reduction.list_calculator.ListCalculator;
import com.cmclinnovations.data.collections.ObjectPool;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.reduction.SpeciesPoolReduction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.ObjectPoolCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;

/**
 *
 * @author pb556
 */

public class SystematicPoolReductionAlgorithm extends SpeciesPoolReduction {

    protected ListCalculator<Double> listCalculator = null;
    protected double contributionThreshold = 0.2;
    protected double threshold = 10.0;
    protected int maxIteration = 1;
    protected boolean excludeUnusedSpecies = false;

   /**
    * @author nk510
    */
    
   /*
    * Missing default constructor of upper class SpeciesPoolReduction is constructed.
    */
   
    public SystematicPoolReductionAlgorithm() {
    
     }
   
    /**
     * @author nk510
     * @param calculator
     */
    public SystematicPoolReductionAlgorithm(ObjectPoolCalculator calculator) {
        super(calculator);
    }
    
    /**
     * @author nk510
     * @param listCalculator
     */
    
    public void set(ListCalculator<Double> listCalculator) {
        this.listCalculator = listCalculator;
    }
    
    /**
     * @author nk510
     * @param enthalpyThreshold
     * @param contributionThreshold
     */
    
    public void set(double enthalpyThreshold, double contributionThreshold) {
        this.contributionThreshold = contributionThreshold;
        this.threshold = enthalpyThreshold;
    }
    
    /**
     * @author nk510
     * @param maxIteration
     */

    public void set(int maxIteration) {
        this.maxIteration = maxIteration;
    }

    /*
     * (non-Javadoc)
     * @see uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.reduction.SpeciesPoolReduction#getReducedObjectPool()
     * Fixed pool.getValidatedObjects() and pool.invalidate(s) by providing declaration of pool in upper class.
     */
    
    @Override
    public void reduce() throws Exception {
        for (int i = 0; i < maxIteration; i++) {
            Map<Species, Map<Reaction, Boolean>> results = filter(extendResults(leaveOneOutCrossValidation()));
            Set<Species> invalids = filterForInvalidSpecies(results);
            if (excludeUnusedSpecies) {
                Set<Species> unused = filterForUnusedSpecies(results);
                for (Species s : unused) {
                    pool.invalidate(s);
                }
            }
            if (invalids.isEmpty()) {
                break;
            }
            for (Species s : invalids) {
                pool.invalidate(s);
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.reduction.SpeciesPoolReduction#getReducedObjectPool()
     * Fixed pool.getValidatedObjects() by providing declaration of pool in upper class.
     */
    
    protected Set<Species> filterForUnusedSpecies(Map<Species, Map<Reaction, Boolean>> results) {
        Set<Species> used = new HashSet<>();
        Set<Species> unused = new HashSet<>();
        for (Species s : results.keySet()) {
            for (Reaction r : results.get(s).keySet()) {
                Set<Species> species = new HashSet<>();
                species.addAll(r.getProducts().keySet());
                species.addAll(r.getReactants().keySet());
                species.remove(s);
                used.addAll(species);
            }
        }
        for (Species s : pool.getValidatedObjects()) {
            if (!used.contains(s)) {
                unused.add(s);
            }
        }
        return unused;
    }
    
    protected Set<Species> filterForInvalidSpecies(Map<Species, Map<Reaction, Boolean>> results) {
        Set<Species> invalids = new HashSet<>();
        for (Species s : results.keySet()) {
            Map<Reaction, Boolean> map = results.get(s);
            Set<Reaction> problematic = new HashSet<>();
            for (Reaction r : map.keySet()) {
                if (!map.get(r)) {
                    problematic.add(r);
                }
            }
            invalids.addAll(analyseProblematicReactions(problematic));
        }
        return invalids;
    }
    
    protected Set<Species> analyseProblematicReactions(Set<Reaction> reactions) {
        Map<Reaction, Double> map = new HashMap<>();
        double sum = 0.0;
        for (Reaction r : reactions) {
            double v = Math.abs(r.calculateHf() - r.getSpecies().getHf());
            sum += v;
            map.put(r, v);
        }
        Map<Reaction, Double> mapNorm = new HashMap<>();
        for (Reaction r : reactions) {
            mapNorm.put(r, map.get(r) / sum);
        }
        Map<Species, List<Double>> speciesMap = new HashMap<>();
        for (Reaction r : mapNorm.keySet()) {
            double w = mapNorm.get(r);
            sum = 0.0;
            for (Double v : r.getProducts().values()) {
                sum += v;
            }
            for (Double v : r.getReactants().values()) {
                sum += v;
            }
            for (Species s : r.getProducts().keySet()) {
                if (!speciesMap.containsKey(s)) {
                    speciesMap.put(s, new ArrayList<Double>());
                }
                speciesMap.get(s).add(w * r.getProducts().get(s) / sum);
            }
            for (Species s : r.getReactants().keySet()) {
                if (!speciesMap.containsKey(s)) {
                    speciesMap.put(s, new ArrayList<Double>());
                }
                speciesMap.get(s).add(w * r.getReactants().get(s) / sum);
            }
        }
        Set<Species> problematicSpecies = new HashSet<>();
        for (Species s : speciesMap.keySet()) {
            if (listCalculator.calculate(speciesMap.get(s)) >= contributionThreshold) {
                problematicSpecies.add(s);
            }
        }
        return problematicSpecies;
    }
    
    protected Map<Species, Map<Reaction, Boolean>> filter(Map<Species, Collection<Reaction>> results) {
        Map<Species, Map<Reaction, Boolean>> extendedResults = new HashMap<>();
        for (Species s : results.keySet()) {
            for (Reaction r : results.get(s)) {
                if (!extendedResults.containsKey(s)) {
                    extendedResults.put(s, new HashMap<Reaction, Boolean>());
                }
                extendedResults.get(s).put(r, Math.abs(r.calculateHf() - s.getHf()) < threshold);
            }
        }
        return extendedResults;
    }
    
    protected Map<Species, Collection<Reaction>> extendResults(Map<Species, Collection<ReactionList>> results) {
        Map<Species, Collection<Reaction>> extendedResults = new HashMap<>();
        for (Species s : results.keySet()) {
            Collection<ReactionList> rListCollection = results.get(s);
            for (ReactionList rList : rListCollection) {
                for (Reaction r : rList.getCollection()) {
                    if (!extendedResults.containsKey(r.getSpecies())) {
                        extendedResults.put(r.getSpecies(), new HashSet<Reaction>());
                    }
                    extendedResults.get(r.getSpecies()).add(r);
                    Set<Species> allSpecies = new HashSet<>();
                    allSpecies.addAll(r.getProducts().keySet());
                    allSpecies.addAll(r.getReactants().keySet());
                    allSpecies.remove(r.getSpecies());
                    Map<Species, Double> products = r.getProducts();
                    Map<Species, Double> reactants = r.getReactants();
                    for (Species rS : allSpecies) {
                        Reaction rNew = new Reaction(rS);
                        for (Species product : products.keySet()) {
                            rNew.addProduct(product, products.get(product));
                        }
                        for (Species reactant : reactants.keySet()) {
                            rNew.addReactant(reactant, reactants.get(reactant));
                        }
                        if (!extendedResults.containsKey(rS)) {
                            extendedResults.put(rS, new HashSet<Reaction>());
                        }
                        extendedResults.get(rS).add(rNew);
                    }
                }
            }
        }
        return extendedResults;
    }

    /*
     * (non-Javadoc)
     * @see uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.reduction.SpeciesPoolReduction#getReducedObjectPool()
     * Fixed pool.getValidatedObjects() by providing declaration of pool in upper class.
     */
    
    protected Map<Species, Collection<ReactionList>> leaveOneOutCrossValidation() {
        Map<Species, Collection<ReactionList>> results = new HashMap<>();
        int ctr = 1;
        for (Species s : pool.getValidatedObjects()) {
            System.out.println("Processing species " + ctr + " / " + pool.getValidatedObjects().size() + " (" + s.getRef() + ")");
            ctr++;
            ExecutorService executor = Executors.newSingleThreadExecutor();
            EnthalpyEstimationThread t = new EnthalpyEstimationThread(calculator, s, getPool(pool.getValidatedObjects(), true));
            Future<Map<Species, Collection<ReactionList>>> future = executor.submit(t);
            try {
                try {
                    Map<Species, Collection<ReactionList>> r = (Map<Species, Collection<ReactionList>>) future.get(300, TimeUnit.SECONDS);
                    if (r != null) {
                        for (Species sR : r.keySet()) {
                            results.put(s, r.get(sR));
                        }
                    } else {
                        
                    	r = (Map<Species, Collection<ReactionList>>) t.getCalculator().get();
                        
                        if (r != null) {
                            for (Species sR : r.keySet()) {
                                results.put(s, r.get(sR));
                            }
                        } else {
                            results.put(s, null);
                        }
                    }
                } catch (TimeoutException | InterruptedException | ExecutionException e) {
                    System.out.println("Terminated!");
                }
            } catch (OutOfMemoryError e) {
                System.gc();
            }
        }
        return results;
    }

    /*
     * (non-Javadoc)
     * @see uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.reduction.SpeciesPoolReduction#getReducedObjectPool()
     * Fixed pool.getValidatedObjects() by providing declaration of pool in upper class.
     */
    
    @Override
    public ObjectPool<Species> getReducedObjectPool() {
        return getPool(pool.getValidatedObjects(), true);
    }

    protected ObjectPool<Species> getPool(Collection<Species> pool, boolean validatedSpeciesOnly) {
        ObjectPool<Species> newPool = new ObjectPool<>();
        for (Species s : pool) {
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }

}