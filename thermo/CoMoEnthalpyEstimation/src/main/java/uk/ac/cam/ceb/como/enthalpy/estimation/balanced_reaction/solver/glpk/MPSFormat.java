/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

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
 * 
 */
public class MPSFormat implements LPFormat {

    protected String description = "enthalpy_estimator";
    private String lineSep = System.getProperty("line.separator");
    private Species targetSpecies;

    /**
     * We use here Set<Species> speciesSet -> LinkedHashSet<Species> speciesSet 
     */
    private LinkedHashSet<Species> speciesSet;    
    
    private VariableSet vSet;
    private boolean intOnly;
    protected ReactionType reactionType;
    
    private Logger logger = Logger.getLogger(getClass());

    public MPSFormat(boolean intOnly, ReactionType reactionType) {

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
        
        /**
         * 
         * @author nk510 (caresssd@hermes.cam.ac.uk).
         * Added sorted species set.
         * 
         * Commnented because HashSet<Species> -> LinkedHashSet<Species>
         * 
         */
//        LinkedHashSet<Species> sortedSpeciesSet = new LinkedHashSet<Species>();
//        
//        sortedSpeciesSet = speciesSet.stream()
//       			.sorted(Comparator.comparing(Species::getRef)
//       			.reversed())
//       			.collect(Collectors.toSet());
        
        this.speciesSet = speciesSet;
        this.vSet = vSet;
        return buildLpSolveInputString();
    }

    private String buildLpSolveInputString() {

        StringBuilder strBuilderFile = new StringBuilder();
        StringBuilder strBuilderMetaData = new StringBuilder();
        StringBuilder strBuilderName = new StringBuilder();
        StringBuilder strBuilderRows = new StringBuilder();
        StringBuilder strBuilderColumns = new StringBuilder();
        StringBuilder strBuilderRHS = new StringBuilder();
        StringBuilder strBuilderBounds = new StringBuilder();
        StringBuilder strBuilderFinish = new StringBuilder();

        // create the actual file
        //writeMetaData(strBuilderMetaData, targetSpecies);
        writeName(strBuilderName, targetSpecies);
        writeRows(strBuilderRows, targetSpecies);
        writeColumns(strBuilderColumns, targetSpecies);
        writeRHS(strBuilderRHS, targetSpecies);
        writeBounds(strBuilderBounds, targetSpecies);
        finish(strBuilderFinish, targetSpecies);

        //logDebuggingInfo(targetSpecies);

        return strBuilderFile.append(strBuilderMetaData).append(strBuilderName).
                append(strBuilderRows).append(strBuilderColumns).
                append(strBuilderRHS).append(strBuilderBounds).
                append(strBuilderFinish).toString();
    }

    private void writeMetaData(StringBuilder strBuilder, Species targetSpecies) {

        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
        Date date = new Date();

        strBuilder.append("*<meta creator='").append(description).append("'>").append(lineSep);
        strBuilder.append("*<meta author='").append("pb556").append("'>").append(lineSep);
        strBuilder.append("*<meta variables=").append(speciesSet.size() + 1).append(">").append(lineSep);
        strBuilder.append("*<meta equations=").append(
                reactionType.getConservationTypeConstraints(SolverHelper.combine(targetSpecies, speciesSet), vSet).size() + 
                reactionType.getMassBalanceConstraints(SolverHelper.combine(targetSpecies, speciesSet), vSet).size()).append(">").append(lineSep);
        strBuilder.append("*<meta job_type='MIN'>").append(lineSep);
        strBuilder.append("*<meta date_time='").append(dateFormat.format(date)).append("'>").append(lineSep);
    }

    private void writeName(StringBuilder strBuilder, Species targetSpecies) {
        strBuilder.append("NAME  ").append(targetSpecies.getRef().toString()).append(System.getProperty("line.separator"));
    }

    // OK
    private void writeRows(StringBuilder strBuilder, Species targetSpecies) {
        strBuilder.append("ROWS").append(lineSep);
        strBuilder.append(" N  R0").append(lineSep); // objective function

        int ctr = 1;
        for (int i = 0; i < vSet.getSet().size(); i++) {
            // var
            strBuilder.append(" L  R").append(ctr).append(lineSep);
            ctr++;
            // absvar
            strBuilder.append(" L  R").append(ctr).append(lineSep);
            ctr++;
        }
        
        // Mass balance and bond type constraints
        for (int i = 0; i < reactionType.getAllElements(targetSpecies, speciesSet).size(); i++) {
            strBuilder.append(" E  R").append(ctr).append(lineSep);
            ctr++;
        }
//        for (String conType : reactionType.getAllConservationTypes(SolverHelper.combine(targetSpecies, speciesSet))){
//            System.out.println(conType);
//        }
        for (int i = 0; i < reactionType.getAllConservationTypes(SolverHelper.combine(targetSpecies, speciesSet)).size(); i++) {
            strBuilder.append(" E  R").append(ctr).append(lineSep);
            ctr++;
        }

        strBuilder.append(" G  main").append(lineSep); // value constraint
    }

    private Multiset<String> getObjectiveFunction(Species targetSpecies) {
        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);
        Multiset<String> objTerms = HashMultiset.create();
        // group terms
        for (Variable v : vSet.getSet()) {
            List<Integer> ctr = reactionType.getConservationTypeConstraints(allSpeciesList, vSet).get(v);
            int sum = 0;
            for (Integer c : ctr) {
                sum += c;
            }
            objTerms.add(v.absName, sum);
        }
        return objTerms;
    }

    private void writeColumns(StringBuilder strBuilder, Species targetSpecies) {
        strBuilder.append("COLUMNS").append(lineSep);
        HashMap<String, ArrayList<String>> cols = new HashMap<String, ArrayList<String>>();
        writeColumnsObjectiveFunction(cols, targetSpecies);
        writeColumnsVarConstraints(cols, targetSpecies);
        writeColumnsEquation(cols, targetSpecies);
        
        String col = "    v[0]"; // need to be adjusted dynamically too
        for (int i = "v[0]".length(); i < 11; i++) {
            col = col + " ";
        }
        col = col + "main";
        // buffer space must be adjusted dynamically
        for (int i = "main".length(); i < 10; i++) {
            col = col + " ";
        }
        col = col + getValue(1, 12) + lineSep;
        if (!cols.containsKey("v[0]")) {
            cols.put("v[0]", new ArrayList<String>());
        }
        cols.get("v[0]").add(col);
        
        if (intOnly) {
            strBuilder.append("    MARK0000  'MARKER'                 'INTORG'").append(lineSep);
        }
        // organise the data
        for (String var : cols.keySet()) {
            for (String c : cols.get(var)) {
                strBuilder.append(c);
            }
        }
        if (intOnly) {
            strBuilder.append("    MARK0001  'MARKER'                 'INTEND'").append(lineSep);
        }
    }

    private void writeColumnsObjectiveFunction(HashMap<String, ArrayList<String>> cols, Species targetSpecies) {
        Multiset<String> objTerms = getObjectiveFunction(targetSpecies);
        // make a string of each term
        List<String> objTermList = new ArrayList<String>(objTerms.elementSet());
        Collections.sort(objTermList);
        for (String objTermVariable : objTermList) {
            int count = objTerms.count(objTermVariable);
            String col;
            if (count > 0) {
                col = "    " + objTermVariable;
                for (int i = objTermVariable.length(); i < 11; i++) {
                    col = col + " ";
                }
                col = col + "R0";
                for (int i = 1; i < 9; i++) {
                    col = col + " ";
                }
                col = col + getValue(count, 12) + lineSep;
                if (!cols.containsKey(objTermVariable)) {
                    cols.put(objTermVariable, new ArrayList<String>());
                }
                cols.get(objTermVariable).add(col);
            }
        }
    }

    private void writeColumnsVarConstraints(HashMap<String, ArrayList<String>> cols, Species targetSpecies) {
        Set<Variable> vars = vSet.getSet();
        int ctr = 1;
        for (Variable v : vars) {
            addColumnEntry(cols, v.absName, -1, ctr);
            addColumnEntry(cols, v.name, 1, ctr);
            ctr++;
            addColumnEntry(cols, v.absName, -1, ctr);
            addColumnEntry(cols, v.name, -1, ctr);
            ctr++;
        }
    }

    private void addColumnEntry(HashMap<String, ArrayList<String>> cols, String varName, int value, int ctr) {
        if (value == 0) {
            return;
        }
        String col = "    " + varName.trim(); // need to be adjusted dynamically too
        for (int i = varName.trim().length(); i < 11; i++) {
            col = col + " ";
        }
        col = col + "R" + ctr;
        // buffer space must be adjusted dynamically
        String strCtr = String.valueOf(ctr);
        for (int i = strCtr.length(); i < 9; i++) {
            col = col + " ";
        }
        col = col + getValue(value, 12) + lineSep;
        if (!cols.containsKey(varName)) {
            cols.put(varName, new ArrayList<String>());
        }
        cols.get(varName).add(col);
    }

    private void writeColumnsEquation(HashMap<String, ArrayList<String>> cols, Species targetSpecies) {
        addEquations(cols, reactionType.getMassBalanceConstraints(SolverHelper.combine(targetSpecies, speciesSet), vSet), vSet.getSet().size() * 2 + 1);
        addEquations(cols, reactionType.getConservationTypeConstraints(SolverHelper.combine(targetSpecies, speciesSet), vSet),
                vSet.getSet().size() * 2 + 1 + reactionType.getAllElements(targetSpecies, speciesSet).size());
    }

    private void addEquations(HashMap<String, ArrayList<String>> cols, HashMap<Variable, List<Integer>> balance, int ctrStart) {
        int ctr;
        for (Variable v : balance.keySet()) {
            ctr = ctrStart;
            for (int value : balance.get(v)) {
                addColumnEntry(cols, v.name, value, ctr);
                ctr++;
            }
        }
    }

    private String getValue(int v, int length) {
        String strV = String.valueOf(v);
        if (strV.length() > length) {
            return null;
        } else if (strV.length() > length) {
            return strV;
        } else {
            strV = strV + ".";
            for (int i = strV.length() + 1; i <= length; i++) {
                strV = strV + "0";
            }
        }
        return strV;
    }

    private void writeRHS(StringBuilder strBuilder, Species targetSpecies) {
        strBuilder.append("RHS").append(lineSep);
        strBuilder.append("    RHS       main      1.0000000000").append(lineSep);
    }

    private void writeBounds(StringBuilder strBuilder, Species targetSpecies) {
        strBuilder.append("BOUNDS").append(lineSep);
        Set<Variable> vars = vSet.getSet();
        for (Variable v : vars) {
            strBuilder.append(" FR BND       ").append(v.absName).append(lineSep);
            strBuilder.append(" FR BND       ").append(v.name).append(lineSep);
        }

    }

    private void finish(StringBuilder strBuilder, Species targetSpecies) {
        strBuilder.append("ENDATA");
    }

    private void logDebuggingInfo(Species targetSpecies) {
        // for debugging only
        List<Species> allSpeciesList = SolverHelper.combine(targetSpecies, speciesSet);
        for (Iterator<Species> it = allSpeciesList.iterator(); it.hasNext();) {
            Species sp = it.next();
            Variable variable = vSet.getVariableOf(sp);
            //logger.trace(sp + " -> " + variable.name);
        }
    }   
}
