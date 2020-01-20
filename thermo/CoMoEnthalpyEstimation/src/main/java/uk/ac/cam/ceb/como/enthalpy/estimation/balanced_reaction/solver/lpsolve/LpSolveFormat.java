/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.lpsolve;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.apache.log4j.Logger;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 */
public class LpSolveFormat implements LPFormat {
    
    private String lineSep = System.getProperty("line.separator");
    private Species targetSpecies;
    
    private LinkedHashSet<Species> speciesSet;
 
    private VariableSet vSet;
    private boolean intOnly;
    protected ReactionType reactionType;
    private Logger logger = Logger.getLogger(getClass());

    public LpSolveFormat(boolean intOnly, ReactionType reactionType) {
        this.intOnly = intOnly;
        this.reactionType = reactionType;
    }
    
    public ReactionType getReactionType() {
        return reactionType;
    }
    
    public boolean isIntegerProgramming() {
        return intOnly;
    }
    
    
    @Override
    public String getInputString(Species species, LinkedHashSet<Species> speciesSet, VariableSet vSet) {
        this.targetSpecies = species;
        this.speciesSet = speciesSet;
        this.vSet = vSet;
        return buildLpSolveInputString();
    }

    private String buildLpSolveInputString() {
        StringBuilder objSb = new StringBuilder("/* Objective function */");
        objSb.append(lineSep);
        StringBuilder objConstSb = new StringBuilder(lineSep);
        objConstSb.append("/* Objective variable constraints */");
        objConstSb.append(lineSep);
        StringBuilder atomConstSb = new StringBuilder(lineSep);
        atomConstSb.append("/* Mass balance constraints */");
        atomConstSb.append(lineSep);
        StringBuilder bondConstSb = new StringBuilder(lineSep);
        bondConstSb.append("/* Bond type conservation constraints */");
        bondConstSb.append(lineSep);
        StringBuilder varConstSb = new StringBuilder(lineSep);
        varConstSb.append("/* Variable type and value constraints */");
        varConstSb.append(lineSep);
        
        writeObjectiveFunction(objSb, objConstSb, targetSpecies);
        writeMassBalanceConstraints(atomConstSb, targetSpecies, speciesSet);
        writeConvervationTypeConstraints(bondConstSb, targetSpecies, speciesSet);
        writeVariableTypeAndValueConstraints(varConstSb, targetSpecies);

        logDebuggingInfo(targetSpecies);

        return objSb.append(objConstSb).append(atomConstSb).append(bondConstSb).append(varConstSb).toString();
    }

    private void writeObjectiveFunction(StringBuilder objSb, StringBuilder objConstSb, Species targetSpecies) {
        Collection<String> allBondTypes = reactionType.getAllConservationTypes(SolverHelper.combine(targetSpecies, speciesSet));
        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);

        Multiset<String> objTerms = HashMultiset.create();
        Map<String, String> absNameToName = new TreeMap<String, String>();
        
        // group terms
//        for (String bondType : allBondTypes) {
//            for (Iterator<Species> it = allSpeciesList.iterator(); it.hasNext();) {
//                Species sp = it.next();
//                Variable variable = vSet.getVariableOf(sp);
//
//                // There must be a better way!
//                int count = SolverHelper.countBondType(bondType, sp);
//                
//                // use absName and will need to add additional constraints later
//                objTerms.add(variable.absName, count);
//                absNameToName.put(variable.absName, variable.name);
//            }
//        }
        
        // group terms
        //for (String bondType : allBondTypes) {
        for (Variable v : vSet.getSet()) {
            List<Integer> ctr = reactionType.getConservationTypeConstraints(allSpeciesList, vSet).get(v);
            int sum = 0;
            for (Integer c : ctr) {
                sum += c;
            }
            objTerms.add(v.absName, sum);
            absNameToName.put(v.absName, v.name);
        }
        
        // make a string of each term
        List<String> terms = new ArrayList<String>();
        List<String> objTermList = new ArrayList<String>(objTerms.elementSet());
        Collections.sort(objTermList);
        for (String objTermVariable : objTermList) {
            int count = objTerms.count(objTermVariable);
            if (count > 1) {
                terms.add(count + " * " + objTermVariable);
            } else if (count == 1) {
                terms.add(objTermVariable);
            }
        }
        String objFuncStr = SolverHelper.concatinate(terms, "min: ", " + ", ";");
        objSb.append(objFuncStr).append(lineSep);
        // add absoulte value constraints according to lp_solve documentation
        for (Map.Entry<String, String> entry : absNameToName.entrySet()) {
            String varAbsName = entry.getKey();
            String varName = entry.getValue();
            objConstSb.append(varName).append(" <= ").append(varAbsName).append(";").append(lineSep);
            objConstSb.append("-").append(varName).append(" <= ").append(varAbsName).append(";").append(lineSep);
        }
    }

    private void writeVariableTypeAndValueConstraints(StringBuilder varConstSb, Species targetSpecies) {
        Variable variable = vSet.getVariableOf(targetSpecies);
        // add label main to force extra row in lp_solve' otherwise the result will always be zero.
        varConstSb.append("main: ").append(variable.name).append(" >= 1;").append(lineSep);
        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);
        List<String> vars = new ArrayList<String>();
        for (Species sp : allSpeciesList) {
            Variable spVar = vSet.getVariableOf(sp);
            vars.add(spVar.name);
            vars.add(spVar.absName);
        }

        Collections.sort(vars);
        String allVarsString = SolverHelper.concatinate(vars, "", ", ", ";");

        if (intOnly)
            varConstSb.append("int ").append(allVarsString).append(lineSep);
        varConstSb.append("free ").append(allVarsString).append(lineSep);
    }

    private void logDebuggingInfo(Species targetSpecies) {
        // for debugging only
        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);
        for (Iterator<Species> it = allSpeciesList.iterator(); it.hasNext();) {
            Species sp = it.next();
            Variable variable = vSet.getVariableOf(sp);
            logger.trace(sp + " -> " + variable.name);
        }
    }

    /**
     * Line below is commented from original source code
     */
//    private void writeMassBalanceConstraints(StringBuilder sb, Species targetSpecies, Set<Species> species) {
  private void writeMassBalanceConstraints(StringBuilder sb, Species targetSpecies, Set<Species> species) {
        HashMap<Variable, List<Integer>> vars = reactionType.getMassBalanceConstraints(SolverHelper.combine(targetSpecies, species), vSet);
        Collection<String> constraintSet = reactionType.getAllElementSymbols(targetSpecies, species);
        writeConstraints(sb, constraintSet, vars);
    }
    /**
     * Line below is commented from original source code.
     */
//    private void writeConvervationTypeConstraints(StringBuilder sb, Species targetSpecies, Set<Species> species) {
  private void writeConvervationTypeConstraints(StringBuilder sb, Species targetSpecies, Set<Species> species) {
        HashMap<Variable, List<Integer>> vars = reactionType.getConservationTypeConstraints(SolverHelper.combine(targetSpecies, species), vSet);
        Collection<String> constraintSet = reactionType.getAllConservationTypes(SolverHelper.combine(targetSpecies, species));
        writeConstraints(sb, constraintSet, vars);
    }
    
    private void writeConstraints(StringBuilder sb, Collection<String> constraintSet, HashMap<Variable, List<Integer>> vars) {
        //HashMap<Variable, List<Integer>> elements = reactionType.getMassBalanceConstraints(SolverHelper.combine(targetSpecies, speciesSet), vSet);
        //writeConservationConstraints(atomConstSb, targetSpecies, elements, new ISDLpSolveFormat.AtomMultisetSelector());
        
        for (int i = 0; i < constraintSet.size(); i++) {
            List<String> terms = new ArrayList<String>();
            for (Variable v : vars.keySet()) {
                if (vars.get(v).size() != constraintSet.size()) {
                    System.out.println("ERROR!");
                    return;
                }
                int count = vars.get(v).get(i);
                if (count > 1) {
                    terms.add(count + " * " + v.name);
                } else if (count == 1) {
                    terms.add(v.name);
                }
            }
            String constraintStr = SolverHelper.concatinate(terms, "", " + ", " = 0;");
            sb.append(constraintStr).append(lineSep);
        }
        
                    
            
            
            //logger.trace("Writing convervation equation for constraint of type : " + constraint);
            // make a string of each term
//            List<String> terms = new ArrayList<String>();
//            for (Iterator<Species> it = allSpeciesList.iterator(); it.hasNext();) {
//                Species sp = it.next();
//                Variable variable = vSet.getVariableOf(sp);
//                int count = ms.getMultiset(sp).count(constraint);
//                if (count > 1) {
//                    terms.add(count + " * " + variable.name);
//                } else if (count == 1) {
//                    terms.add(variable.name);
//                }
//            }
    }
    

//    private void writeBondTypeConvervationConstraints(StringBuilder bondConstSb, Species targetSpecies) {
//        Collection<String> bondTypes = reactionType.getAllConservationTypes(SolverHelper.combine(targetSpecies, speciesSet));
//        writeConservationConstraints(bondConstSb, targetSpecies, bondTypes, new ISDLpSolveFormat.BondMultisetSelector());
//    }

//    private void writeConservationConstraints(StringBuilder sb, Species targetSpecies, Set constraintSet, MultisetSelector ms) {
//        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);
//        for (Object constraint : constraintSet) {
//            //logger.trace("Writing convervation equation for constraint of type : " + constraint);
//            // make a string of each term
//            List<String> terms = new ArrayList<String>();
//            for (Iterator<Species> it = allSpeciesList.iterator(); it.hasNext();) {
//                Species sp = it.next();
//                Variable variable = vSet.getVariableOf(sp);
//                int count = ms.getMultiset(sp).count(constraint);
//                if (count > 1) {
//                    terms.add(count + " * " + variable.name);
//                } else if (count == 1) {
//                    terms.add(variable.name);
//                }
//            }
//
//            String constraintStr = SolverHelper.concatinate(terms, "", " + ", " = 0;");
//            sb.append(constraintStr).append(lineSep);
//        }
//    }
}
