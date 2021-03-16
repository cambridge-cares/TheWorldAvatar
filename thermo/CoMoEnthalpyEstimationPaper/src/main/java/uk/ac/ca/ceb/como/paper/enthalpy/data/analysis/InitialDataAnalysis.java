package uk.ac.ca.ceb.como.paper.enthalpy.data.analysis;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

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
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HHDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

/**
 * 
 * @author nk510 ( caresssd@hermes.cam.ac.uk )
 * @author am2145( am2145@cam.ac.uk )
 * 
 *         This class contains extended implementation of cross validation
 *         algorithm implemented by Philipp Buerger (pb556). It includes initial analysis validation that estimates enthalpy of
 *         formation for one target species based on a set of reference species.
 *         The code also generates reactions for target species, calculates error between
 *         estimated and reference enthalpy of formation for target species.
 *         
 *         ThiS class include setting that we use to run the code on High Performance Computing (HPC) machine and to run the code on local Windows machine.
 *
 */

public class InitialDataAnalysis {
	
	/**
	 * 
	 * @author nk510 ( caresssd@hermes.cam.ac.uk )
	 * @param loop the number of loops through a set of invalid species. 
	 * @param addedSpeciesToValidSet Logical constant that is true is a species is added to the set of valid species. Otherwise is false.  
	 * @param iteration the number of iterations inside each loop.
	 * @param destRList  the destination folder where generated results of cross validation algorithm are saved.
	 * @param ctrRadicals the number of radicals.
	 * @param ctrRuns the number of runs of cross validation procedure.
	 * @param ctrRes the number of reactions that are generated. 
	 * @param refSpecies the set of reference species.
	 * @param soiSpecies the set of species of interest (one target species).
	 * @param srcCompoundsRef destination folder where Gaussian  (g09) files are stored.
	 * @param spinMultiplicity the hash map that contains information about spin multiplicity for target species. 
	 * @param mapElPairing
	 * @param tempFolder the folder that stores information used by LP solver.
	 * @param invalids the set of invalid species.
	 * @param all the set of all species (invalid and valid species).
	 * @param validSpecies the set of valid species. 
	 * @param targetSpeciesEnthalpy the value of enthalpy of target species.
	 * @param currentErrorBar the current error bar for selected species.
	 * @throws Exception the exception.
	 * 
	 */
	
	public void getInitialDataAnalysisCrossValidation(int loop, boolean addedSpeciesToValidSet, int iteration,
			String destRList, int[] ctrRadicals, int[] ctrRuns, int[] ctrRes, List<Species> refSpecies,
			List<Species> soiSpecies, String srcCompoundsRef, Map<Species, Integer> spinMultiplicity,
			Map<String, Integer[]> mapElPairing, String tempFolder, Collection<Species> invalids, Set<Species> all,
			LinkedHashSet<Species> validSpecies, double targetSpeciesEnthalpy, double currentErrorBar, BufferedWriter printedResultsFile, ReactionType reactionType) throws Exception {

		for (int z = 0; z < ctrRadicals.length; z++) {

			int maxRadical = ctrRadicals[z];

			int timeout = 1500;// 1500;

			for (int i = 0; i < ctrRuns.length; i++) {

				for (int k = 0; k < ctrRes.length; k++) {

//	                int timeout = 60 * ctrRes[k];
					
					String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";

					System.out.println("Process configuration " + config);
					
					printedResultsFile.write("Process configuration " + config);
					
					printedResultsFile.write("\n");

					/**
					 * 
					 * @author nk510 (caresssd@hermes.cam.ac.uk)
					 * Settings on PC machine.
					 * 
					 */
					
//					if (new File(destRList +"\\" + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_" + iteration + "\\" + config + ".txt").exists()) {
					
						/**
						 * 
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * HPC settings
						 * 
						 */
					if (new File(destRList +"/"+ "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + config + ".txt").exists()) {

						/**
						 * 
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * Settings on PC machine.
						 * 
						 */
						
//					System.out.println("Skipping " + destRList+"\\" + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + config);
						
						/**
						 * 
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * HPC settings
						 * 
						 */
					System.out.println("Skipping " + destRList +"/"+ "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + config);

					printedResultsFile.write("Skipping " + destRList+"/" + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + config);
					
//					printedResultsFile.write("Skipping " + destRList+"\\" + "initial-analysis" + "\\" + "loop_" + loop + "\\" + "iteration_" + iteration + "\\" + config);
					
					printedResultsFile.write("\n");
					
						continue;
					}

					/**
					 * 
					 * @author nk510 (caresssd@hermes.cam.ac.uk)
					 * Next two lines of the code are commented and were used in original Philipp's code.
					 * 
					 */
					
//					Collections.shuffle(refSpecies);
//					Collections.shuffle(soiSpecies);

					int ctr = 1;

					/**
					 * @author nk510 (caresssd@hermes.cam.ac.uk)
					 * Iterates through the set of species of interest (target species) for which the code generates reaction and estimates enthalpy of formation.
					 * 
					 */
					for (Species target : soiSpecies) {

						LPSolver solver = new TerminalGLPKSolver(15000, false, true);//15000 -> 30000
						
						/**
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * Settings on PC machine.
						 */

//						new File(tempFolder + "loop_" + loop +"\\"+"iteration_" + iteration +"\\"+ target.getRef().replace(".g09", "") + "\\.temp\\").mkdirs();
						/**
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * HPC settings
						 */
						new File(tempFolder+"/" + "loop_" + loop + "/" + "iteration_" + iteration + "/"+ target.getRef().replace(".g09", "") + "/.temp/").mkdirs();
						
						/**
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * Settings on PC machine.
						 */

//						solver.setDirectory(new File(tempFolder + "loop_" + loop +"\\"+"iteration_" + iteration +"\\"+ target.getRef().replace(".g09", "") + "\\"));
						/**
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * HPC settings
						 * 
						 */
						solver.setDirectory(new File(tempFolder+"/" + "loop_" + loop + "/" + "iteration_" + iteration + "/"+ target.getRef().replace(".g09", "") + "/"));

//	                        System.out.println("REF: Processing " + ctr + " / " + all.size());
//	                        ctr++;

						File f = new File(srcCompoundsRef + target.getRef().replace(".g09", "") + ".g09");

						if (f.exists()) {

							System.out.print(f.getName() + ": ");

							try {

								Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));

								if (e != null) {

									spinMultiplicity.put(target, e[1] + 1);

									mapElPairing.put(target.getRef(), e);

								} else {

									System.out.println("REF: e- pairing could not be determined for " + target.getRef());
									
									printedResultsFile.write("REF: e- pairing could not be determined for " + target.getRef());
									
									printedResultsFile.write("\n");
									

									invalids.add(target);

									continue;
								}

							} catch (NullPointerException npe) {

								if (target.getRef().compareTo("Ti5O6Cl8") == 0) {

									spinMultiplicity.put(target, 1);

								} else {

									System.out.println(target.getRef());
									

									printedResultsFile.write(target.getRef());
									
									printedResultsFile.write("\n");
									
								}
							}

						} else {

							System.out.println("REF: No file found for " + target.getRef());
							
							printedResultsFile.write("REF: No file found for " + target.getRef());
							
							printedResultsFile.write("\n");
							
							invalids.add(target);
							
							continue;
							
						}

						refSpecies.removeAll(invalids);
						all.removeAll(invalids);
						soiSpecies.removeAll(invalids);

						SolverHelper.add(mapElPairing);

						Map<Species, Collection<ReactionList>> results = new HashMap<>();

						System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + soiSpecies.size() + ")");

						printedResultsFile.write("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + soiSpecies.size() + ")");
						
						printedResultsFile.write("\n");
						
						ctr++;

						/**
						 * @author nk510 (caresssd@hermes.cam.ac.uk)
						 * Settings on PC machine.
						 * 
						 */
						
//						if (new File(destRList+"\\" +  "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\"+ target.getRef() + "\\" + config + "_reaction-list.rct").exists()) {
						
							/**
							 * @author nk510 ( caresssd@hermes.cam.ac.uk )
							 * HPC settings
							 * 
							 */
						if (new File(destRList +"/"+ "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + target.getRef() + "/" + config + "_reaction-list.rct").exists()) {

							continue;

						}

						List<Species> refPool = new ArrayList<>();

						refPool.addAll(refSpecies);
						refPool.remove(target);

						// filter for radicals
						for (Species sSpin : spinMultiplicity.keySet()) {

							try {

								if (spinMultiplicity.get(sSpin) != null
										&& spinMultiplicity.get(sSpin) - 1 > maxRadical) {
									refPool.remove(sSpin);
								}
							} catch (NullPointerException ex) {
							}
						}

						Collections.shuffle(refPool);
						
						ExecutorService executor = Executors.newSingleThreadExecutor();

//						PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver,new MPSFormat(false, new ISDReactionType()));
						
						PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver,new MPSFormat(false, reactionType));
						
						poolModCalc.setMaximumSearchDepth(50);

						MultiRunCalculator c = new MultiRunCalculator(poolModCalc);
						c.setNumberOfRuns(ctrRuns[i]);

						EnthalpyEstimationThread t = new EnthalpyEstimationThread(c, target,
						
						EvaluationUtils.getPool(refPool, true));
						
						Future<Map<Species, Collection<ReactionList>>> future = executor.submit(t);

						try {
							try {
								Map<Species, Collection<ReactionList>> r = (Map<Species, Collection<ReactionList>>) future
										.get(timeout, TimeUnit.SECONDS);
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
								
								printedResultsFile.write("Terminated!");
								
								printedResultsFile.write("\n");

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

									System.out.println("rList:");
									
									printedResultsFile.write("rList:");
									
									printedResultsFile.write("\n");
									
									

									for (Reaction r : rList) {

										System.out.println("Species name : " + r.getSpecies().getRef() + " enthalpy: " + r.getSpecies().getHf());
										
										printedResultsFile.write("Species name : " + r.getSpecies().getRef() + " enthalpy: " + r.getSpecies().getHf());
										
										printedResultsFile.write("\n");
										
									}

									completeRList.addAll(rList);

									ReactionSelector selector = new MedianReactionSelector();

									List<Reaction> reactionList = selector.select(rList);

									System.out.println("reactionList.size(): " + reactionList.size());
									
									printedResultsFile.write("reactionList.size(): " + reactionList.size());
									
									printedResultsFile.write("\n");
									

									for (Reaction r : reactionList) {

										System.out.println("Species name : " + r.getSpecies().getRef() + " enthalpy: " + r.getSpecies().getHf());
										
										printedResultsFile.write("Species name : " + r.getSpecies().getRef() + " enthalpy: " + r.getSpecies().getHf());
										
										printedResultsFile.write("\n");
										
									}

									Reaction r = selector.select(rList).get(0);

									s.setHf(r.calculateHf());

									ttipSpecies.add(s);

								} catch (ArrayIndexOutOfBoundsException | NullPointerException aioobe) {
									
									System.out.println("No data were calculated for " + s.getRef());	
									
									printedResultsFile.write("No data were calculated for " + s.getRef());
									
									printedResultsFile.write("\n");
									
								}
							}

							/**
							 * 
							 * @author nk510 (caresssd@hermes.cam.ac.uk)
							 * Settings on PC machine.
							 * 
							 */
							
//							if (!new File(destRList+"\\" + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\"+ target.getRef() + "\\").exists()) {
//								new File(destRList +"\\"+ "initial-analysis" + "\\"+ "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + target.getRef() + "\\").mkdirs();
							
							/**
							 * 
							 * @author nk510 (caresssd@hermes.cam.ac.uk)
							 * HPC settings
							 * 
							 */
							
							if (!new File(destRList+"/" + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + target.getRef() + "/").exists()) {
								new File(destRList +"/"+ "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"	 + iteration + "/" + target.getRef() + "/").mkdirs();
							}
							
							/**
							 * 
							 * @author nk510 (caresssd@hermes.cam.ac.uk)
							 * Settings on PC machine.
							 * 
							 */

//							ReactionListWriter rListWriter = new ReactionListWriter(new File(
//									destRList +"\\" + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct"));
							
							/**
							 * 
							 * @author nk510 (caresssd@hermes.cam.ac.uk)
							 * HPC settings
							 * 
							 */
							ReactionListWriter rListWriter = new ReactionListWriter(new File(
									destRList+"/" + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"+ iteration + "/" + target.getRef() + "/" + config + "_reaction-list.rct"));
							/**
							 * 
							 * @author nk510 (caresssd@hermes.cam.ac.uk)
							 * Settings on PC machine.
							 * 
							 */
							
//							SpeciesPoolWriter spWriter = new SpeciesPoolWriter(new File(
//									destRList +"\\" + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + target.getRef() + "\\" + config + "_species-pool_median.csv"));
							
							/**
							 * 
							 * @author nk510 (caresssd@hermes.cam.ac.uk)
							 * HPC settings
							 * 
							 */
							
							SpeciesPoolWriter spWriter = new SpeciesPoolWriter(new File(destRList+"/" + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"+ iteration + "/" + target.getRef() + "/" + config + "_species-pool_median.csv"));

							
							BufferedWriter printedJsonFileMedianEnthalpyForSpeciesInitialAnalysis = new BufferedWriter(new FileWriter(destRList+"/" + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"+ iteration + "/" + target.getRef() + "/" + config + "_species-pool_median.json", true));
							
//							BufferedWriter printedJsonFileMedianEnthalpyForSpeciesInitialAnalysis = new BufferedWriter(new FileWriter(destRList+"\\" + "initial-analysis" + "\\" + "loop_" + loop + "\\" + "iteration_"+ iteration + "/" + target.getRef() + "\\" + config + "_species-pool_median.json", true));
							
							if (!completeRList.isEmpty()) {
								
								System.out.println("Writting complete reaction list...");
								
								printedResultsFile.write("Writting complete reaction list...");
								
								printedResultsFile.write("\n");
								
								rListWriter.set(completeRList);
								rListWriter.overwrite(true);
								rListWriter.write();

								/**
								 * 
								 * @author nk510 (caresssd@hermes.cam.ac.uk)
								 * Sets the reference enthalpy for target species
								 * 
								 */
								 JSONArray meadianEnthalpySpeciesJsonList = new JSONArray();
								 
							for (Species r : refSpecies) {

									if (r.getRef().equals(target.getRef())) {

										System.out.println("Ref species name: " + r.getRef() + " = " + " Target species name : " + target.getRef() + " Median species enthalpy: " + target.getHf());
										
										printedResultsFile.write("Ref species name: " + r.getRef() + " = " + " Target species name : " + target.getRef() + " Median species enthalpy: " + target.getHf());
										
										printedResultsFile.write("\n");
										
										JSONObject jsonMedianEnthalpySpecies = new JSONObject();
							     		
							     	    JSONObject jsonAllMedianEnthalpySpecies = new JSONObject();	    
							     	    
							     	    /**
							     	     * 
							     	     * @author NK510 (caresssd@hermes.cam.ac.uk)
							     		 * Store information about median enthalpy for species in JSON file.
							     		 * 
							     		 */
							     	    
							     	   jsonMedianEnthalpySpecies.put("speciesName",target.getRef().toString());
							     		
							       	   jsonMedianEnthalpySpecies.put("medianEnthalpy",target.getHf());
							     		
							     	   jsonAllMedianEnthalpySpecies.put("species", jsonMedianEnthalpySpecies);
							     		
							     	  meadianEnthalpySpeciesJsonList.add(jsonAllMedianEnthalpySpecies);
							     	   
										target.setHf(r.getHf());
							     	   

									}
								}

								printedJsonFileMedianEnthalpyForSpeciesInitialAnalysis.write(meadianEnthalpySpeciesJsonList.toJSONString());
								printedJsonFileMedianEnthalpyForSpeciesInitialAnalysis.close();
								
								double errorSum = 0d;
								
								/**
								 * 
								 * @author nk510 (caresssd@hermes.cam.ac.uk)
								 * Number of reactions to be used as an error count in calculating error bar.
								 * 
								 */
								int errorCount = completeRList.size();

								double errorBar = 0d;

								/**
								 * 
								 * @author nk510 (caresssd@hermes.cam.ac.uk)                                                                                                                                        
								 * Sets enthalpy of formation (Hf) to target species as given in reference set of species.
								 * 
								 */
								
								target.setHf(targetSpeciesEnthalpy);

								for (int ri = 0; ri < completeRList.size(); ri++) {

//	                              double error = Math.abs(completeRList.get(ri).getSpecies().getHf()-completeRList.get(ri).calculateHf());

								  Double error = Math.abs(targetSpeciesEnthalpy - completeRList.get(ri).calculateHf());

								  System.out.println("target species name: " + target.getRef() + " ,  target ref enthalpy: " + targetSpeciesEnthalpy); 
								  
								  printedResultsFile.write("target species name: " + target.getRef() + " ,  target ref enthalpy: " + targetSpeciesEnthalpy);
									
								  printedResultsFile.write("\n");

								  System.out.println(" Reaction(" + ri + "): " + completeRList.get(ri).toString()
											+ " Species (target ref) enthalpy: " + targetSpeciesEnthalpy
											+ " Calculated Hf for reaction(" + ri + "): "
											+ completeRList.get(ri).calculateHf() + " error: " + error);

								  printedResultsFile.write(" Reaction(" + ri + "): " + completeRList.get(ri).toString()
											+ " Species (target ref) enthalpy: " + targetSpeciesEnthalpy
											+ " Calculated Hf for reaction(" + ri + "): "
											+ completeRList.get(ri).calculateHf() + " error: " + error);
									
								  printedResultsFile.write("\n");

								  
									errorSum = errorSum + error;

								}

								errorBar = errorSum / errorCount;

								System.out.println("species name: " + target.getRef() + " error bar: " + errorBar);
								
								printedResultsFile.write("species name: " + target.getRef() + " error bar: " + errorBar);
								
								printedResultsFile.write("\n");
								  
								
								/**
								 * 
								 * @author nk510 (caresssd@hermes.cam.ac.uk)
								 * Checks whether error bar for current species is smaller that error bar for
								 * that species calculated in pre-processing step.
								 * 
								 */
								
								if (errorBar < currentErrorBar) {

									/**
									 * @author nk510 (caressssd@hermes.cam.ac.uk)
									 * Adds species into valid list of species
									 */
									
									validSpecies.add(target);

									addedSpeciesToValidSet = true;

								}

							}

							if (!ttipSpecies.isEmpty()) {
								
								System.out.println("Writting species list...");
								

								printedResultsFile.write("Writting species list...");
								
								printedResultsFile.write("\n");
								
								spWriter.set(ttipSpecies, false);
								
								spWriter.write();

								System.out.println("TTIP Species: ");


								printedResultsFile.write("TTIP Species: ");
								
								printedResultsFile.write("\n");
								
								for (Species ttip : ttipSpecies) {

								System.out.println(ttip.getRef() + " " + ttip.getHf());
								
								printedResultsFile.write(ttip.getRef() + " " + ttip.getHf());
								
								printedResultsFile.write("\n");
								
								}
							}

						} catch (OutOfMemoryError e) {
							System.gc();
						}
					}

					try {
						
						StringWriter writer = new StringWriter();
						
						writer.setContent("completed!");
						
						writer.overwrite(true);
						
						/**
						 * @author nk510 ( caresssd@hermes.cam.ac.uk )
						 * Settings on PC machine.
						 */
						
//						writer.set(destRList+"\\" + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + config + ".txt");
						
						/**
						 * @author nk510 ( caresssd@hermes.cam.ac.uk )
						 * HPC settings
						 */
						
						writer.set(destRList+"/" + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"+ iteration + "/" + config + ".txt");
						
						writer.write();

					} catch (Exception e) {
					
					}
				}
			}
		} // for (int z = 0; z < ctrRadicals.length; z++)
	}
}