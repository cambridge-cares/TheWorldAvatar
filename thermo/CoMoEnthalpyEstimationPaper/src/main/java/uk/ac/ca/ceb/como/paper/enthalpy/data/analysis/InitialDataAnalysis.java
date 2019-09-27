package uk.ac.ca.ceb.como.paper.enthalpy.data.analysis;

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
 * @author nk510 ( caresssd@hermes.cam.ac.uk )
 * @author am2145( am2145@cam.ac.uk )
 * 
 *         This class contains extended implementation of cross validation
 *         algorithm implemented by Philipp Buerger (pb556). It includes initial analysis validation that estimates enthalpy of
 *         formation for one target species based on a set of reference species.
 *         The code also generates reactions for target species, calculates error between
 *         estimated and reference enthalpy of formation for target species.
 *         
 *         
 *
 */
public class InitialDataAnalysis {

	/**
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
	 */
	public void getInitialDataAnalysisCrossValidation(int loop, boolean addedSpeciesToValidSet, int iteration,
			String destRList, int[] ctrRadicals, int[] ctrRuns, int[] ctrRes, List<Species> refSpecies,
			List<Species> soiSpecies, String srcCompoundsRef, Map<Species, Integer> spinMultiplicity,
			Map<String, Integer[]> mapElPairing, String tempFolder, Collection<Species> invalids, Set<Species> all,
			Set<Species> validSpecies, double targetSpeciesEnthalpy, double currentErrorBar) throws Exception {

		for (int z = 0; z < ctrRadicals.length; z++) {

			int maxRadical = ctrRadicals[z];

			int timeout = 1500;// 1500;

			for (int i = 0; i < ctrRuns.length; i++) {

				for (int k = 0; k < ctrRes.length; k++) {

//	                  int timeout = 60 * ctrRes[k];
					String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_"
							+ timeout + "s";

					System.out.println("Process configuration " + config);

					if (new File(destRList  + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_" + iteration + "\\" + config + ".txt").exists()) {
//					if (new File(destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + config + ".txt").exists()) {

						System.out.println("Skipping " + destRList + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + config);
//						System.out.println("Skipping " + destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + config);

						continue;

					}

					/**
					 * Next two line of the code were used in original Philipp's code.
					 */
//					Collections.shuffle(refSpecies);
//					Collections.shuffle(soiSpecies);

					int ctr = 1;

					/**
					 * Iterates through the set of species of interest (target species) for which the code generates reaction and estimates enthalpy of formation.
					 */
					for (Species target : soiSpecies) {

						LPSolver solver = new TerminalGLPKSolver(15000, false, true);//15000

						new File(tempFolder + "loop_" + loop +"\\"+"iteration_" + iteration +"\\"+ target.getRef().replace(".g09", "") + "\\.temp\\").mkdirs();
//						new File(tempFolder + "loop_" + loop + "/" + "iteration_" + iteration + "/"+ target.getRef().replace(".g09", "") + "/.temp/").mkdirs();

						solver.setDirectory(new File(tempFolder + "loop_" + loop +"\\"+"iteration_" + iteration +"\\"+ target.getRef().replace(".g09", "") + "\\"));
//						solver.setDirectory(new File(tempFolder + "loop_" + loop + "/" + "iteration_" + iteration + "/"+ target.getRef().replace(".g09", "") + "/"));

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

									System.out
											.println("REF: e- pairing could not be determined for " + target.getRef());

									invalids.add(target);

									continue;
								}

							} catch (NullPointerException npe) {

								if (target.getRef().compareTo("Ti5O6Cl8") == 0) {

									spinMultiplicity.put(target, 1);

								} else {

									System.out.println(target.getRef());
								}
							}

						} else {

							System.out.println("REF: No file found for " + target.getRef());
							invalids.add(target);
							continue;
						}

						refSpecies.removeAll(invalids);
						all.removeAll(invalids);
						soiSpecies.removeAll(invalids);

						SolverHelper.add(mapElPairing);

						Map<Species, Collection<ReactionList>> results = new HashMap<>();

						System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / "
								+ soiSpecies.size() + ")");

						ctr++;

						if (new File(destRList +  "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\"+ target.getRef() + "\\" + config + "_reaction-list.rct").exists()) {
//						if (new File(destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + target.getRef() + "/" + config + "_reaction-list.rct").exists()) {

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

						PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver,new MPSFormat(false, new ISGReactionType(true)));
						
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

								Map<Species, Collection<ReactionList>> re = (Map<Species, Collection<ReactionList>>) t
										.getCalculator().get();

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

									for (Reaction r : rList) {

										System.out.println("Species name : " + r.getSpecies().getRef() + " enthalpy: "
												+ r.getSpecies().getHf());
									}

									completeRList.addAll(rList);

									ReactionSelector selector = new MedianReactionSelector();

									List<Reaction> reactionList = selector.select(rList);

									System.out.println("reactionList.size(): " + reactionList.size());

									for (Reaction r : reactionList) {

										System.out.println("Species name : " + r.getSpecies().getRef() + " enthalpy: "
												+ r.getSpecies().getHf());
									}

									Reaction r = selector.select(rList).get(0);

									s.setHf(r.calculateHf());

									ttipSpecies.add(s);

								} catch (ArrayIndexOutOfBoundsException | NullPointerException aioobe) {
									System.out.println("No data were calculated for " + s.getRef());
								}
							}

							if (!new File(destRList + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\"+ target.getRef() + "\\").exists()) {
								new File(destRList + "initial-analysis" + "\\"+ "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + target.getRef() + "\\").mkdirs();
//							if (!new File(destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_" + iteration + "/" + target.getRef() + "/").exists()) {
//								new File(destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"	 + iteration + "/" + target.getRef() + "/").mkdirs();
							}

							ReactionListWriter rListWriter = new ReactionListWriter(new File(
									destRList  + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct"));
//									destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"+ iteration + "/" + target.getRef() + "/" + config + "_reaction-list.rct"));
							SpeciesPoolWriter spWriter = new SpeciesPoolWriter(new File(
									destRList  + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + target.getRef() + "\\" + config + "_species-pool_median.csv"));
//									destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"+ iteration + "/" + target.getRef() + "/" + config + "_species-pool_median.csv"));

							if (!completeRList.isEmpty()) {
								System.out.println("Writting complete reaction list...");
								rListWriter.set(completeRList);
								rListWriter.overwrite(true);
								rListWriter.write();

								/**
								 * Sets the reference enthalpy for target species
								 */
								for (Species r : refSpecies) {

									if (r.getRef().equals(target.getRef())) {

										System.out.println("Ref species name: " + r.getRef() + " = "
												+ " Target species name : " + target.getRef()
												+ " Median species enthalpy: " + target.getHf());

										target.setHf(r.getHf());

									}
								}

								double errorSum = 0.0;
								/**
								 * Number of reactions to be used as an error count in calculating error bar.
								 */
								int errorCount = completeRList.size();

								double errorBar = 0.0;

								/**
								 * Sets enthalpy of formation (Hf) to target species as given in reference set of species.
								 */
								target.setHf(targetSpeciesEnthalpy);

								for (int ri = 0; ri < completeRList.size(); ri++) {

//	                              double error = Math.abs(completeRList.get(ri).getSpecies().getHf()-completeRList.get(ri).calculateHf());

									double error = Math.abs(targetSpeciesEnthalpy - completeRList.get(ri).calculateHf());

									System.out.println("target species name: " + target.getRef() + " ,  target ref enthalpy: " + targetSpeciesEnthalpy); 

									System.out.println(" Reaction(" + ri + "): " + completeRList.get(ri).toString()
											+ " Species (target ref) enthalpy: " + targetSpeciesEnthalpy
											+ " Calculated Hf for reaction(" + ri + "): "
											+ completeRList.get(ri).calculateHf() + " error: " + error);

									errorSum = errorSum + error;

								}

								errorBar = errorSum / errorCount;

								System.out.println("species name: " + target.getRef() + " error bar: " + errorBar);
								/**
								 * Checks whether error bar for current species is smaller that error bar for
								 * that species calculated in pre-processing step.
								 */
								if (errorBar < currentErrorBar) {

									/**
									 * Adds species into valid list of species
									 */
									validSpecies.add(target);

									addedSpeciesToValidSet = true;

								}

							}

							if (!ttipSpecies.isEmpty()) {
								System.out.println("Writting species list...");
								spWriter.set(ttipSpecies, false);
								spWriter.write();

								System.out.println("TTIP Species: ");

								for (Species ttip : ttipSpecies) {

									System.out.println(ttip.getRef() + " " + ttip.getHf());
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
						writer.set(destRList + "initial-analysis" + "\\" + "loop_" + loop +"\\"+"iteration_"+ iteration + "\\" + config + ".txt");
//						writer.set(destRList + "initial-analysis" + "/" + "loop_" + loop + "/" + "iteration_"+ iteration + "/" + config + ".txt");
						writer.write();

					} catch (Exception e) {
					}
				}
			}
		} // for (int z = 0; z < ctrRadicals.length; z++)

	}
}
