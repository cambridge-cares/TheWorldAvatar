/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.ticl4.calculation.thermopaper;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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

import com.google.common.collect.Multiset;

import uk.ac.cam.ceb.como.chem.periodictable.Block;
import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MedianReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;
/**
 *
 * @author pb556
 * 
 */

public class TiCl4CalculationISG {
	
    public static void main(String[] args) throws Exception {

//        String srcCompoundsRef = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\final-calculations-publication-used\\esc\\combined\\g09\\";
//        String srcRefPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\reference\\plain-ref_scaled_kJperMols_v8-0p05.csv"; //reference data
//        String srcSoiPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\ti-o-cl-species\\calc-enthalpy_scaled_kJperMol.csv";  // target species
//        String destRList = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\publication-validation\\results\\isg\\";
        
        /**
         * 
         * Explanation:
         * Below are given folder paths to data used in testing this code. 
         * srcCompoundsRef: This is a String that refers to a folder with GAussian files for which Hf and EBRs are estimated. Files are available on Vienna folder: /CoMoCommon/Archive/Projects/Preprints/c4e/c4e-180-pb556-TiCl4/Data/other/initial-calculations/thermo/thermo-calculations/esc/combined/g09/
         * srcRefPool: This is a String that refers to reference data (species) based on which this code estimates Hf and EBRs for target species given as  "String srcSoiPool". These data are available on data are available on Vienna folder: /CoMoCommon/Archive/Projects/Preprints/c4e/c4e-180-pb556-TiCl4/Data/other/initial-calculations/thermo/enthalpy/
         * srcSoiPool: This is a String that refers to target data (species) for which this Java code estimates Hf and EBRs. These data are available on data are available on Vienna folder: /CoMoCommon/Archive/Projects/Preprints/c4e/c4e-180-pb556-TiCl4/Data/other/initial-calculations/thermo/enthalpy/
         * destRList: This is a String that refers to destination folder where output of calculations (Hf, EBRs) are  stored.
         * 
         */
    	
////        String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\g09\\";
////        String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\plain-ref_scaled_kJperMols_v8-0p05.csv"; //171//ref-enthalpy_scaled_kJperMol.csv
//      String srcSoiPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\calc-enthalpy_scaled_kJperMol-test-1-species.csv"; //Target 1 species in first raw. Other species from the list belong to reference species.
////        String srcSoiPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\calc-enthalpy_scaled_kJperMol-test-1-species.csv";
//      String srcSoiPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\calc-enthalpy_scaled_kJperMol-test-10-species.csv"; //Target 10 species from 1st to 10th raw. Other species from the list belong to reference species.
//      String srcSoiPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\calc-enthalpy_scaled_kJperMol-test-no-ref-data.csv"; // There are no reference species that are included in the list of target species. //171//calc-enthalpy_scaled_kJperMol.csv
////        String destRList = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\isg\\";
        
      String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\esc\\g09\\"; 
      String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\ref-enthalpy_scaled_kJperMol.csv";  //171//ref-enthalpy_scaled_kJperMol.csv
      String srcSoiPool = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\calc-enthalpy_scaled_kJperMol-1-species.csv"; //171//calc-enthalpy_scaled_kJperMol.csv
      String destRList = "C:\\Users\\NK\\Documents\\philipp\\\\171-pb556\\hco_isg\\";
        
//        String srcCompoundsRef = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\west-recalc\\all-g09\\";
//        String srcRefPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\reference\\ref_scaled_kJperMols_v8.csv";
//        String srcSoiPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\west-recalc\\calc-enthalpy_scaled_kJperMol-Ti5.csv";
//        String destRList = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\west-recalc\\results\\isg\\";

//        String srcCompoundsRef = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\ti-o-cl-species-missing2\\all-g09\\";
//        String srcRefPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\reference\\ref_scaled_kJperMols_v8.csv";
//        String srcSoiPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\ti-o-cl-species-missing2\\calc-enthalpy_scaled_kJperMol.csv";
//        String destRList = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\ti-o-cl-species-missing2\\results\\isg\\";
//        String srcCompoundsRef = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\west-recalc\\all-g09\\";
//        String srcRefPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\reference\\ref_scaled_kJperMols_v8.csv";
//        String srcSoiPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\west-recalc\\calc-enthalpy_scaled_kJperMol.csv";
//        String destRList = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\west-recalc\\results\\isg\\";
//        String srcCompoundsRef = "W:\\projects\\thermo_calcs\\g09\\";
//        String srcRefPool = "W:\\projects\\thermo_calcs\\references\\new-enthalpy-upd_e_scaled_kJperMols-v2.csv";
//        String srcSoiPool = "W:\\projects\\thermo_calcs\\iteration_1\\smiles-emp1\\can-sep\\calc-enthalpy_scaled_kJperMol.csv";
//        String destRList = "W:\\projects\\thermo_calcs\\iteration_1\\smiles-emp1\\can-sep\\calculation\\hco_isg\\";
//        String srcCompoundsRef = "W:\\projects\\TiCl4_thermo\\esc\\filtered-and-organised\\can-sep\\verify\\freq-success\\g09-combined\\";
//        String srcRefPool = "W:\\projects\\TiCl4_thermo\\thermo-calculations\\enthalpy\\reference\\ref_scaled_kJperMols_v8.csv";
//        String srcSoiPool = "W:\\projects\\TiCl4_thermo\\esc\\filtered-and-organised\\can-sep\\verify\\freq-success\\calc-enthalpy_scaled_kJperMol.csv";
//        String destRList = "W:\\projects\\TiCl4_thermo\\esc\\filtered-and-organised\\can-sep\\verify\\freq-success\\calculation\\hco_isg\\";

        Map<String, Integer[]> mapElPairing = new HashMap<>();
        
        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        
        refParser.parse();
        
        List<Species> refSpecies = new ArrayList<>(refParser.getRefSpecies());

        System.out.println("- - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - ");
        
        System.out.println("refSpecies: ");
        
        for(Species spRef : refSpecies) {
        	
        	System.out.println("spRef.getRef(): " + spRef.getRef());
        	System.out.println("spRef.getHf(): " + spRef.getHf());
        	System.out.println("spRef.getTotalEnergy(): " + spRef.getTotalEnergy());
        	
        	Map<String, String> atomicMap = spRef.getAtomMap();
        	
        	System.out.println("Atomic map:");
        	
        	for(Map.Entry<String, String> atMap:atomicMap.entrySet()) {
        		
            System.out.println("- " + atMap.getKey() + " " + atMap.getValue());
        		
        	}

        	Multiset<Element> multiSet = spRef.getAtomMultiset();
        	
        	System.out.println("Element MultiSet: ");
        	
        	for(Element elMultiSet : multiSet) {
        		
        		System.out.println("elMultiSet.getAtomicNumber(): " + elMultiSet.getAtomicNumber());
        		System.out.println("elMultiSet.getAtomicWeight(): " + elMultiSet.getAtomicWeight());
        		System.out.println("elMultiSet.getGroup(): " + elMultiSet.getGroup());
        		System.out.println("elMultiSet.getMassNumber(): " + elMultiSet.getMassNumber());
        		System.out.println("elMultiSet.getName(): " + elMultiSet.getName());
        		System.out.println("elMultiSet.getNumberOfNeutrons(): " + elMultiSet.getNumberOfNeutrons());
        		System.out.println("elMultiSet.getNumberOfPairedElectrons(): " + elMultiSet.getNumberOfPairedElectrons());
        		System.out.println("elMultiSet.getNumberOfUnpairedElectrons(): " + elMultiSet.getNumberOfUnpairedElectrons());
        		System.out.println("elMultiSet.getPeriod(): " + elMultiSet.getPeriod());
        		System.out.println("elMultiSet.getSymbol(): " + elMultiSet.getSymbol());
        		System.out.println("elMultiSet.getBlock(): " + elMultiSet.getBlock());
        	}
        	
        	Map<String, Element> elementMap = spRef.getElementMap();
        	
        	System.out.println("Element Map: ");
        	
        	for(Map.Entry<String, Element> element: elementMap.entrySet()) {
        		
        		System.out.println("element.getKey(): " + element.getKey());
        		
        		Element elementObject = element.getValue();
        		
        		System.out.println("elementObject.getAtomicNumber(): " + elementObject.getAtomicNumber());
        		System.out.println("elementObject.getAtomicWeight(): " + elementObject.getAtomicWeight());
        		System.out.println("elementObject.getGroup(): " + elementObject.getGroup());
        		System.out.println("elementObject.getMassNumber(): " + elementObject.getMassNumber());
        		System.out.println("elementObject.getName(): " + elementObject.getName());
        		System.out.println("elementObject.getNumberOfNeutrons(): " + elementObject.getNumberOfNeutrons());
        		System.out.println("elementObject.getNumberOfPairedElectrons(): " + elementObject.getNumberOfPairedElectrons());
        		System.out.println("elementObject.getNumberOfUnpairedElectrons(): " + elementObject.getNumberOfUnpairedElectrons());
        		System.out.println("elementObject.getPeriod(): " + elementObject.getPeriod());
        		System.out.println("elementObject.getSymbol(): " + elementObject.getSymbol());
        		        		
        		Block block = elementObject.getBlock();
        		
        		System.out.println("Block: ");
        		
        		System.out.println("block.name(): " + block.name());
        		System.out.println("block.ordinal(): " + block.ordinal());
        		
        	}
        }
        
        System.out.println("- - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - ");
        
        SpeciesPoolParser soiParser = new SpeciesPoolParser(new File(srcSoiPool));
        
        soiParser.parse();
        
        List<Species> soiSpecies = new ArrayList<>(soiParser.getSpeciesOfInterest());

        System.out.println("soiSpecies: ");
        
        for(Species sp: soiSpecies) {
        	
        	System.out.println("species.getRef(): " + sp.getRef());
        	System.out.println("species.getHf(): " + sp.getHf());
        	System.out.println("species.getTotalEnergy(): " + sp.getTotalEnergy());
        	
        	Map<String, String> atomicMap = sp.getAtomMap();
        	
        	System.out.println("Atomic map:");
        	
        	for(Map.Entry<String, String> atMap:atomicMap.entrySet()) {
        		
            System.out.println("- " + atMap.getKey() + " " + atMap.getValue());
        		
        	}

        	Multiset<Element> multiSet = sp.getAtomMultiset();
        	
        	System.out.println("Element MultiSet: ");
        	
        	for(Element elMultiSet : multiSet) {
        		
        		System.out.println("elMultiSet.getAtomicNumber(): " + elMultiSet.getAtomicNumber());
        		System.out.println("elMultiSet.getAtomicWeight(): " + elMultiSet.getAtomicWeight());
        		System.out.println("elMultiSet.getGroup(): " + elMultiSet.getGroup());
        		System.out.println("elMultiSet.getMassNumber(): " + elMultiSet.getMassNumber());
        		System.out.println("elMultiSet.getName(): " + elMultiSet.getName());
        		System.out.println("elMultiSet.getNumberOfNeutrons(): " + elMultiSet.getNumberOfNeutrons());
        		System.out.println("elMultiSet.getNumberOfPairedElectrons(): " + elMultiSet.getNumberOfPairedElectrons());
        		System.out.println("elMultiSet.getNumberOfUnpairedElectrons(): " + elMultiSet.getNumberOfUnpairedElectrons());
        		System.out.println("elMultiSet.getPeriod(): " + elMultiSet.getPeriod());
        		System.out.println("elMultiSet.getSymbol(): " + elMultiSet.getSymbol());
        		System.out.println("elMultiSet.getBlock(): " + elMultiSet.getBlock());
        		
        	}
        	
        	Map<String, Element> elementMap = sp.getElementMap();
        	
        	System.out.println("Element Map: ");
        	
        	for(Map.Entry<String, Element> element: elementMap.entrySet()) {
        		
        		System.out.println("element.getKey(): " + element.getKey());
        		
        		Element elementObject = element.getValue();
        		
        		System.out.println("elementObject.getAtomicNumber(): " + elementObject.getAtomicNumber());
        		System.out.println("elementObject.getAtomicWeight(): " + elementObject.getAtomicWeight());
        		System.out.println("elementObject.getGroup(): " + elementObject.getGroup());
        		System.out.println("elementObject.getMassNumber(): " + elementObject.getMassNumber());
        		System.out.println("elementObject.getName(): " + elementObject.getName());
        		System.out.println("elementObject.getNumberOfNeutrons(): " + elementObject.getNumberOfNeutrons());
        		System.out.println("elementObject.getNumberOfPairedElectrons(): " + elementObject.getNumberOfPairedElectrons());
        		System.out.println("elementObject.getNumberOfUnpairedElectrons(): " + elementObject.getNumberOfUnpairedElectrons());
        		System.out.println("elementObject.getPeriod(): " + elementObject.getPeriod());
        		System.out.println("elementObject.getSymbol(): " + elementObject.getSymbol());
        		        		
        		Block block = elementObject.getBlock();
        		
        		System.out.println("Block: ");
        		
        		System.out.println("block.name(): " + block.name());
        		System.out.println("block.ordinal(): " + block.ordinal());
        		
        	}        	
        }
        
        Collection<Species> invalids = new HashSet<>();
        
        Map<Species, Integer> spinMultiplicity = new HashMap<>();
        
        int ctr = 1;

        Set<Species> all = new HashSet<>();
        
        all.addAll(soiSpecies);
        
        all.addAll(refSpecies);

        for (Species s : all) {
        
        System.out.println("REF: Processing " + ctr + " / " + all.size());
            
        ctr++;
        
        File f = new File(srcCompoundsRef + s.getRef().replace(".g09", "") + ".g09");
            
        if (f.exists()) {
        
        System.out.print(f.getName() + ": ");
        
        try {

            		Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));
                    
            		System.out.println("Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));");
            		
            		for(Integer e1: e) {
            			
            		System.out.println("e1: " + e1);
            		
            		}
            		
            		if (e != null) {
                    
            			spinMultiplicity.put(s, e[1] + 1);
                        
            			mapElPairing.put(s.getRef(), e);
            			
                    } else {
                    	
                        System.out.println("REF: e- pairing could not be determined for " + s.getRef());
                        
                        invalids.add(s);
                        
                    }
            		
            		System.out.println("mapElPairing:");
            		
            		for(Map.Entry<String, Integer[]> mapEl : mapElPairing.entrySet()) {
            			
            			System.out.println(" - mapEl.getKey(): " + mapEl.getKey());
            			
            			Integer[] mapElInt = mapEl.getValue();
            			
            			for(Integer m:mapElInt) {
            				
            				System.out.println(" - m.floatValue(): " +m.floatValue());
            			}
            			
            		}
            		
                } catch (NullPointerException npe) {
                	
                	/**
                	 * 
                	 * Check this line with Angiras.
                	 * 
                	 */
                    if (s.getRef().compareTo("Ti5O6Cl8") == 0) {
                    	
                        spinMultiplicity.put(s, 1);
                        
                    } else {
                    	
                        System.out.println(s.getRef());
                        
                    }
                    
                }
            	
            } else {
            	
                System.out.println("REF: No file found for " + s.getRef());
                
                invalids.add(s);
            }
        }
        
        refSpecies.removeAll(invalids);
        
        all.removeAll(invalids);
        
        soiSpecies.removeAll(invalids);
        
        SolverHelper.add(mapElPairing);

        LPSolver solver = new TerminalGLPKSolver(15000, false, true);
//      solver.setDirectory(new File("C:\\Users\\pb556\\temp2\\"));
        
        solver.setDirectory(new File("D:\\Data-Philip\\LeaveOneOutCrossValidation_temp\\"));

        int[] ctrRuns = new int[]{1};
        
        int[] ctrRes = new int[]{1}; // 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18, - number of reactions //8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40
        
        int[] ctrRadicals = new int[]{100}; // 0, 1, 2, 3, 4, 5

        for (int z = 0; z < ctrRadicals.length; z++) {
        
        	int maxRadical = ctrRadicals[z];
            
            int timeout = 1500;
            
            for (int i = 0; i < ctrRuns.length; i++) {
            
            	for (int k = 0; k < ctrRes.length; k++) {
                
            		String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                    
            		System.out.println("Process configuration " + config);

                    if (new File(destRList + "\\" + config + ".txt").exists()) {
                    
                    	System.out.println("Skipping " + destRList + "\\" + config);
                        
                    	continue;
                    
                    }
                    
                    Collections.shuffle(refSpecies);
                    
                    Collections.shuffle(soiSpecies);
                    
                    ctr = 1;
                    
                    for (Species target : soiSpecies) {

                        Map<Species, Collection<ReactionList>> results = new HashMap<>();
                        
                        System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + soiSpecies.size() + ")");
                        
                        ctr++;

                        if (new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct").exists()) {
                            continue;
                        }

                        List<Species> refPool = new ArrayList<>();
                        
                        refPool.addAll(refSpecies);
                        
                        refPool.remove(target);
                        
                        // filter for radicals
                        for (Species sSpin : spinMultiplicity.keySet()) {
                        	
                            try {
                            	
                                if (spinMultiplicity.get(sSpin) != null && spinMultiplicity.get(sSpin) - 1 > maxRadical) {
                                	
                                    refPool.remove(sSpin);
                                    
                                }
                                
                            } catch (NullPointerException ex) {
                            	
                            }
                        }
                        
                        Collections.shuffle(refPool);
                        
                        ExecutorService executor = Executors.newSingleThreadExecutor();
                        
                        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new ISGReactionType(true))); 
                        
                        poolModCalc.setMaximumSearchDepth(50);
                        
                        MultiRunCalculator c
                                = new MultiRunCalculator(
                                        poolModCalc);
                        
                        c.setNumberOfRuns(ctrRuns[i]);
                        
                        EnthalpyEstimationThread t = new EnthalpyEstimationThread(c, target, EvaluationUtils.getPool(refPool, true));
                        
                        Future<Map<Species, Collection<ReactionList>>> future = executor.submit(t);
                        
                        try {
                        	
                            try {
                            	
                                Map<Species, Collection<ReactionList>> r = (Map<Species, Collection<ReactionList>>) future.get(timeout, TimeUnit.SECONDS);
                                
                                if (r != null) {
                                	
                                    for (Species sR : r.keySet()) {
                                    	
                                        results.put(target, r.get(sR));
                                    }
                                    
                                } else {
                                    r = (Map<Species, Collection<ReactionList>>) t.getCalculator().get();
                                    
                                    if (r != null) {
                                    	
                                        for (Species sR : r.keySet()) {
                                        	
                                            results.put(target, r.get(sR));
                                        }
                                        
                                    } else {
                                    	
                                        results.put(target, null);
                                     
                                    }
                                }
                            } catch (TimeoutException | InterruptedException | ExecutionException e) {
                            	
                                System.out.println("Terminated!");
                                
                                Map<Species, Collection<ReactionList>> re = (Map<Species, Collection<ReactionList>>) t.getCalculator().get();
                                
                                if (re != null) {
                                
                                	for (Species sR : re.keySet()) {
                                    
                                		results.put(target, re.get(sR));
                                    }
                                	
                                } else {
                                	
                                    results.put(target, null);
                                    
                                }
                            }

                            ReactionList completeRList = new ReactionList();
                            
                            Collection<Species> ttipSpecies = new HashSet<>();
                            
                            for (Species s : results.keySet()) {
                                
                            	try {
                                    
                                	ReactionList rList = new ReactionList();
                                    
                                    for (ReactionList l : results.get(s)) {
                                    	
                                        rList.addAll(l);
                                    
                                    }
                                    
                                    completeRList.addAll(rList);
                                    
                                    ReactionSelector selector = new MedianReactionSelector();
                                    
                                    Reaction r = selector.select(rList).get(0);
                                    
                                   Map<Species, Double> reactant = r.getReactants();
                                   
                                   System.out.println("Reactants: ");
                                   
                                   for (Map.Entry<Species, Double> reactants : reactant.entrySet()) {
                                	
                                	   Species key = reactants.getKey();
                                	   
                                	   Double value = reactants.getValue();
                                	    
                                	    System.out.println("key.getRef(): " + key.getRef()+ " , value: " + value);
                                	    
                                	}
                                   
                                   
                                   Map<Species, Double> product = r.getProducts();
                                   
                                   System.out.println("Products: ");
                                   
                                   for(Map.Entry<Species, Double>  products: product.entrySet()) {
                                	   
                                	   Species key = products.getKey();
                                	   
                                	   Double value = products.getValue();
                                	   
                                	   System.out.println("key.getRef(): " + key.getRef() + " ,  value : " + value);
                                   }
                                   
                                   System.out.println("[r.getSpecies().getRef(): " +r.getSpecies().getRef() + "] [r.getSpecies().getHf(): " + r.getSpecies().getHf() + "] [r.getSpecies().getTotalEnergy(): " + r.getSpecies().getTotalEnergy() + "]");
                                    
                                    s.setHf(r.calculateHf());

                                    System.out.println("[s.getRef(): " + s.getRef() + "], [s.getHf():  " + s.getHf() + "]");
                                    
                                    System.out.println("[r.calculateHf(): " + r.calculateHf()+" ]");
                                    
                                    ttipSpecies.add(s);
                                    
                                } catch (ArrayIndexOutOfBoundsException | NullPointerException aioobe) {
                                	
                                    System.out.println("No data were calculated for " + s.getRef());
                                }
                            }

                            if (!new File(destRList + "\\" + target.getRef() + "\\").exists()) {
                            	
                                new File(destRList + "\\" + target.getRef() + "\\").mkdirs();
                                
                            }

                            ReactionListWriter rListWriter = new ReactionListWriter(new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct"));
                            
                            SpeciesPoolWriter spWriter = new SpeciesPoolWriter(new File(destRList + "\\" + target.getRef() + "\\" + config + "_species-pool_median.csv"));

                            if (!completeRList.isEmpty()) {
                            	
                                System.out.println("Writting complete reaction list...");
                                rListWriter.set(completeRList);
                                rListWriter.overwrite(true);
                                rListWriter.write();
                            }

                            if (!ttipSpecies.isEmpty()) {
                                System.out.println("Writting species list...");
                                spWriter.set(ttipSpecies, false);
                                spWriter.write();
                            }
                        } catch (OutOfMemoryError e) {
                            System.gc();
                        }
                    }

                    try {
                        StringWriter writer = new StringWriter();
                        writer.setContent("completed!");
                        writer.overwrite(true);
                        writer.set(destRList + "\\" + config + ".txt");
                        writer.write();
                    } catch (Exception e) {
                    }
                }
            }
        }
    }
}