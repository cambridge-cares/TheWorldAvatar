/*
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
 */

package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;

import org.apache.log4j.Logger;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Bond;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 * 
 */

public class Solver {

    private Logger logger = Logger.getLogger(getClass());
    
    
    /**
     * 
     * @author nk510 (caresssd@hermes.cam.ac.uk)
     * Line below is commented. We use Set<Species> speciesSet -> LinkedHashSet<Species> speciesSet;
     * 
     */
    
//   private List<Species> sortedSpeciesSetList;

    private LinkedHashSet<Species> speciesSet;

    
    private LPSolver lpSolver;
    private VariableFactory vfactory;
    private VariableSet vSet;
    private LPFormat fFormat;

    public Solver(LPSolver solver, LPFormat format, VariableFactory factory) {
    	
        this.lpSolver = solver;
        this.speciesSet = new LinkedHashSet<Species>();
        /**
         * 
         * @author nk510 (caresssd@hermes.cam.ac.uk)
         * Line below is commented because we use Set<Species> speciesSet -> LinkedHashSet<Species> speciesSet;
         * 
         */
        
//      this.sortedSpeciesSetList=new ArrayList<Species>();
         
        this.fFormat = format;
        this.vfactory = factory;
        this.vSet = new VariableSet(vfactory);
    }

    /**
     * 
     * Add reference species (as well as their enthalpy of formation, electronic.
     * energy + (H(T)-H(0)) + ZPE at the same temperature T for all species).
     *
     * @param species add reference species
     * 
     */
    
    public void addReferenceSpecies(Species species) {
    
    speciesSet.add(species);
    
    /**
     * 
     * @author nk510 (caresssd@hermes.cam.ac.uk)
     * Sorted list of Species.
     * 
     * Sorted species set is commneted because we use Set<Species> speciesSet -> LinkedHashSet<Species> speciesSet;
     * 
     */
    
//    sortedSpeciesSetList = speciesSet.stream()
//   			.sorted(Comparator.comparing(Species::getRef)
//   			.reversed())
//   			.collect(Collectors.toList());
    
//  System.out.println("Solver class: Added species into species set (method : addReferenceSpecies): ");
        
    for(Species ss: speciesSet) {        
        	
//  System.out.println(" - Atom name and enthalpy in speciesSet: " + ss.getRef() + " " + ss.getHf()  );
        	
//  System.out.println(" - Atom multiset in speciesSet");
    
    for(Element refpms : ss.getAtomMultiset()){
        		
//  System.out.println("name: " + refpms.getName() + " atom number: " + refpms.getAtomicNumber()+ " group: " + refpms.getGroup() + " mass number: " + refpms.getMassNumber());
        	
    }

//  System.out.println(" - Atom map in speciesSet:");
        	
    for(Map.Entry<String, String> atomM : ss.getAtomMap().entrySet()){
        		
//  System.out.println(atomM.getKey() + " " + atomM.getValue());

    }
        	
//  System.out.println(" - Bond map in speciesSet:");
        	
    for(Bond bondM : ss.getBondMap()){
        	
//  System.out.println("bond type value: " + bondM.getBondType().getValue() + " atom A: " + bondM.getRefAtomA().toString() + " atom B: " + bondM.getRefAtomB().toString());
        	
    }
    
    }
        
//  System.out.println("SORTED SPECIES SET:");
        
//  System.out.println("sortedSpeciesSetList.isEmpty(): " + sortedSpeciesSetList.isEmpty());
        
    int k =1;
        
    for(Species s: speciesSet) {		

//  System.out.println(k++ + ". name: " + s.getRef() + " Hf:" + s.getHf());
        	
    }
    
    }
    
    public void removeReferenceSpecies(Species species) {
    	
//    	sortedSpeciesSetList.remove(species);
    	speciesSet.remove(species);
        
    }

    private Map<String, Number> solveProblem(Species targetSpecies) throws NoFeasibleSolutionException, LpSolverException {
        
     vSet.getVariableOf(targetSpecies);
      
//   for (Species s : sortedSpeciesSetList) {
    	  
        /**
         * 
         * @author nk510 (caresssd@hermes.cam.ac.uk)
         * Line below is commented from original source code/
         * 
         */

  for(Species s: speciesSet) {
      	  
    vSet.getVariableOf(s);
      
    }
    
     /**
      * 
      * @author nk510 (caresssd@hermes.cam.ac.uk)
      * Creates input string for GLPK solver.
      * 
      */
    //create equation file
    String lpInputString = buildLpSolveInputString(targetSpecies);
    
//  logger.debug(lpInputString);
    
    /**
     * 
     * @author nk510 (caresssd@hermes.cam.ac.uk)
     * Printing out input string for GLPK solver.
     *  
     */
//  System.out.println("Solver class: method solveProblem: lpInputString: " + lpInputString);
    
    try {
            
        	// call lp_solve input:
            Map<String, Number> solutions = lpSolver.solve(lpInputString);
            
            LinkedHashSet<String> absNamesToBeRemoved = new LinkedHashSet<String>();
            
            // speciesToVariables should now be populated:
            for (Map.Entry<String, Number> entry : solutions.entrySet()) {
            	
                String varName = entry.getKey();
                
                Species sp = vSet.findSpeciesByVariableName(varName);
                
                Variable variable = vSet.getVariableOf(sp);
                
                absNamesToBeRemoved.add(variable.absName);
            }
            
            for (String absName : absNamesToBeRemoved) {
            	
            solutions.remove(absName);
            
            }
            
            return solutions;
            
        } catch (NoFeasibleSolutionException ex) {
        	
            throw ex;
            
        } catch (LpSolverException ex) {
        	
            throw ex;
        }
    }

    /**
     * 
     * Solve isodemic reaction problem for the targetSpecies. All energies
     * (except enthalpy of formation must be provided at the same temperature as
     * those of the reference species).
     *
     * @param targetSpecies target species whose enthalpy of formation needs to
     * be calculated
     * @return An isodesmic reaction. The enthalpy of formation can be evaluated
     * using its method.
     * @throws NoFeasibleSolutionException if there is not solution to the
     * problem.
     * @throws LpSolverException if there is a porblem with IO
     * 
     */
    public Reaction solve(Species targetSpecies) throws NoFeasibleSolutionException, LpSolverException {
        
    	vfactory.reset();
    	
        vSet = new VariableSet(vfactory);
        
        HashSet<Variable> variableSet = vSet.getSet();
        
        /**
         * 
         * @author nk510 (caresssd@hermes.cam.ac.uk)
         * Lined below is commneted from original source code.
         * 
         */
//      Set<Variable> variableSet = vSet.getSet();
        
//      System.out.println("Variable value inside Solver class: " + " v. set is empty: " + variableSet.isEmpty());
        
        for(Variable v: variableSet) {
        	
//      System.out.println("variable value: " + v.value + " variable name: "  + v.name + " Absolute name: " + v.absName);
      	
        }
        
//      for (Species ref : speciesSet) {
//            if (ref.equals(targetSpecies, true)) {
//                Reaction reaction = new Reaction(targetSpecies);
//                reaction.addReactant(targetSpecies, 1);
//                reaction.addProduct(ref, 1);
//                return reaction;
//            }
//        }
        
        Map<String, Number> solutions = solveProblem(targetSpecies);
        
//      System.out.println("Solver class: Empty solution: " + solutions.isEmpty());
        
        Reaction reaction = new Reaction(targetSpecies);
        
        for (Map.Entry<String, Number> entry : solutions.entrySet()) {
        	
        String varName = entry.getKey();
            
        Species sp = vSet.findSpeciesByVariableName(varName);
            
//      System.out.println("Inside Solver class: vSet.findSpeciesByVariableName(varName).getRef(): " + vSet.findSpeciesByVariableName(varName).getRef() + " " + varName);
            
        if (sp == null) {
        	
                throw new RuntimeException("Expecting a species for variable name " + varName + " but it was not found. Possible cause is that "
                        + getClass().getSimpleName() + " was modified externally before the solver finished. This solver is not thread-safe");
            }
            
        Double count = (Double) entry.getValue().doubleValue();
            
        if (count > 0) {
            	
            reaction.addReactant(sp, count);
                
            } else if (count < 0) {
            	
            reaction.addProduct(sp, -count);
                
            } else if (count == 0) {
            	
            // do nothing
            	
            } else {

            throw new RuntimeException("Unexpected count value for species. This should not happen.");
            
            }
        }
        
    return reaction;
    
    }

    private String buildLpSolveInputString(Species targetSpecies) {
     
    return fFormat.getInputString(targetSpecies, speciesSet, vSet);
    
    }
}