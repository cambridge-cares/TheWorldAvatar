package uk.ac.cam.ceb.como.paper.enthalpy.estimation;

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
	import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
	import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
	import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
	import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
	import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
	import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
	import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;
	import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;
	
	/**
	 * The generic class for estimating the enthalpy of formation of a set of target species.  
	 * 
	 * @author msff2
	 * @author pb556
	 * 
	 */

	public class EnthalpyEstimation {
		
		/**
		 * Upon receiving the list of parameters, this method calculates<br>
		 * the enthalpy of formation (Hf) and EBRs.   
		 * 
		 * @param srcCompoundsRef refers to the folder with electronic<br>
		 * structure calculations of all species involved in the current<br>
		 * calculation.
		 * @param srcRefPool refers to the file with the experimental/<br>
		 * calculated value of enthalpy of formation, molar connectivity, etc.  
		 * @param srcSoiPool refers to the file containing the target species or<br>
		 * species of interest (soi) for which this method estimates Hf and EBRs.<br>
		 * @param destRList refers to the destination folder where outputs<br>
		 * of calculations (Hf, EBRs) are  stored.
		 * @param ctrRuns indicates the number of runs
		 * @param ctrRes refers to the number of reactions (EBRs) to find
		 * @param ctrRadicals indicates the number of radicals radicals to use
		 * @param reactionType refers to the reaction class (e.g. ISG, ISD or HD)
		 * @param tempFolder indicates the name of the temporary folder 
		 * @throws Exception
		 */
		public void estimateEnthalpy(String srcCompoundsRef, String srcRefPool, String srcSoiPool, String destRList, int[] ctrRuns, int[] ctrRes, int[] ctrRadicals, ReactionType reactionType, String tempFolder) throws Exception {
	        Map<String, Integer[]> mapElPairing = new HashMap<>();
	        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
	        refParser.parse();
	        List<Species> refSpecies = new ArrayList<>(refParser.getRefSpecies());
	        showSpeciesData(refSpecies, "Reference Spcies");
	        SpeciesPoolParser soiParser = new SpeciesPoolParser(new File(srcSoiPool));
	        soiParser.parse();
	        List<Species> soiSpecies = new ArrayList<>(soiParser.getSpeciesOfInterest());
	        showSpeciesData(soiSpecies, "Target/Soi Species");
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
	        solver.setDirectory(new File(System.getProperty("user.home").concat(File.separator)));
	        for (int z = 0; z < ctrRadicals.length; z++) {
	        
	        	int maxRadical = ctrRadicals[z];
	            
	            int timeout = 1500;
	            
	            for (int i = 0; i < ctrRuns.length; i++) {
	            
	            	for (int k = 0; k < ctrRes.length; k++) {
	                
	            		String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
	                    
	            		System.out.println("Process configuration " + config);

	                    if (new File(destRList + File.separator + config + ".txt").exists()) {
	                    
	                    	System.out.println("Skipping " + destRList + File.separator + config);
	                        
	                    	continue;
	                    
	                    }
	                    
	                    Collections.shuffle(refSpecies);
	                    
	                    Collections.shuffle(soiSpecies);
	                    
	                    ctr = 1;
	                    
	                    for (Species target : soiSpecies) {

	                        Map<Species, Collection<ReactionList>> results = new HashMap<>();
	                        
	                        System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + soiSpecies.size() + ")");
	                        
	                        ctr++;

	                        if (new File(destRList + File.separator + target.getRef() + File.separator + config + "_reaction-list.rct").exists()) {
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
	                        
	                        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, reactionType)); 
	                        
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

	                            if (!new File(destRList + File.separator + target.getRef() + File.separator).exists()) {
	                            	
	                                new File(destRList + File.separator + target.getRef() + File.separator).mkdirs();
	                                
	                            }

	                            ReactionListWriter rListWriter = new ReactionListWriter(new File(destRList + File.separator + target.getRef() + File.separator + config + "_reaction-list.rct"));
	                            
	                            SpeciesPoolWriter spWriter = new SpeciesPoolWriter(new File(destRList + File.separator + target.getRef() + File.separator + config + "_species-pool_median.csv"));

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
	                        writer.set(destRList + File.separator + config + ".txt");
	                        writer.write();
	                    } catch (Exception e) {
	                    }
	                }
	            }
	        }
	    }
		
		/**
		 * Displays data and metadata of a set of species.
		 * 
		 * @param species
		 */
		private void showSpeciesData(List<Species> species, String message){
	        System.out.println("- - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - ");
	        System.out.println(message+": ");
	        for(Species spRef : species) {
	        	System.out.println("spRef.getRef(): " + spRef.getRef());
	        	System.out.println("spRef.getHf(): " + spRef.getHf());
	        	System.out.println("spRef.getTotalEnergy(): " + spRef.getTotalEnergy());
	        	Map<String, String> atomicMap = spRef.getAtomMap();
	        	System.out.println("Atomic map:");
	        	for(Map.Entry<String, String> atMap:atomicMap.entrySet()) {
	            System.out.println("- " + atMap.getKey() + " " + atMap.getValue());
	        	}
	        	Multiset<Element> multiSet = spRef.getAtomMultiset();
	        	showMultiSet(multiSet);
	        	Map<String, Element> elementMap = spRef.getElementMap();
	        	showElement(elementMap);
	        }
		}
		
		/**
		 * Shows multisets of each species.
		 *  
		 * @param multiSet
		 */
		private void showMultiSet(Multiset<Element> multiSet){
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
		}
		
		/**
		 * Displays the elements belonging to the current species and metadata of them.
		 * 
		 * @param elementMap
		 */
		private void showElement(Map<String, Element> elementMap){
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
}
