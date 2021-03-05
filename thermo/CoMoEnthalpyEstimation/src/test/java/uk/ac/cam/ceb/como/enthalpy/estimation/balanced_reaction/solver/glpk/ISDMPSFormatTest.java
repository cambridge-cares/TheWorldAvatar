/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.LinkedHashSet;

import org.junit.Ignore;
import org.junit.Test;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 */

public class ISDMPSFormatTest {

    @Test
    @Ignore
    public void checkMPSFormatFromCSVFile() throws FileNotFoundException, Exception {
        String soiPath = "W:\\Data\\TTIP\\hf\\data_ti_test.csv";
        String refPath = "W:\\Data\\TTIP\\hf\\data_simple.csv";
        
        SpeciesPoolParser soiParser = new SpeciesPoolParser(new File(soiPath));
        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(refPath));
        
        soiParser.parse();
        refParser.parse();
        
        Collection<Species> soi = soiParser.getSpeciesOfInterest();
        
        /**
         * 
         * @author nk510 (caresssd@hermes.cam.ac.uk)
         * Line below is commented from original source code.
         * 
         */
        
      LinkedHashSet<Species> ref = (LinkedHashSet<Species>) refParser.getRefSpecies();
        
        
        for (Species target : soi) {
            MPSFormat f = new MPSFormat(true, new ISDReactionType());
            System.out.println(f.getInputString(target, ref, new VariableSet(new VariableFactory("v"))));
        }
        
    }
    
    public ObjectPool<Species> getPool(Collection<Species> sp) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.addAll(sp);
        pool.validateAll();
        return pool;
    }
    
//    @Test
//    public void getInputStringRowsTest() {
//    }
//
//    @Test
//    public void getInputStringColumnsTest1() throws Exception {
//        
//        // order!
//        ISDSpecies target = MockISDSpecies.getC2H4();
//
//        HashSet<ISDSpecies> pool = new HashSet<ISDSpecies>();
//        pool.add(MockISDSpecies.getC2H6());
//        pool.add(MockISDSpecies.getC3H6());
//        pool.add(MockISDSpecies.getC3H8());
//
//        String mps = new ISDMPSFormat().getInputString(
//                target, pool, new VariableSet(new VariableFactory("v")));
//
//        FileUtils.writeStringToFile(new File("test_data/test_mps.mps"), mps, "UTF-8");
//
//        // check constraints      
//        ArrayList<int[]> v = new ArrayList<int[]>();
//        v.add(new int[]{2, 4, 1, 0, 4});
//        v.add(new int[]{3, 8, 0, 2, 8});
//        v.add(new int[]{2, 6, 0, 1, 6});
//        v.add(new int[]{3, 6, 1, 1, 6});
//
//        ArrayList<Boolean> b = new ArrayList<Boolean>();
//        b.add(false);
//        b.add(false);
//        b.add(false);
//        b.add(false);
//
//        List<String> lines = FileUtils.readLines(new File("test_data/test_mps.mps"));
//
//        ArrayList<String> eq = getEquationRows(lines);
//
//        HashMap<String, HashMap<String, Integer>> mat = new HashMap<String, HashMap<String, Integer>>();
//
//        for (String row : eq) {
//            mat.put(row, extractEquation(row, lines));
//        }
//
//        // check the equations
//        HashMap<String, ArrayList<Integer>> stoichCoeff = compress(mat);
//        for (String var : stoichCoeff.keySet()) {
//            boolean identified = false;
//            for (int i = 0; i < v.size(); i++) {
//                if (!b.get(i)) {
//                    identified |= stoichCoeff.get(var).contains(v.get(i)[0])
//                            && stoichCoeff.get(var).contains(v.get(i)[1])
//                            && stoichCoeff.get(var).contains(v.get(i)[2])
//                            && stoichCoeff.get(var).contains(v.get(i)[3]);
//                }
//            }
//            assert (identified);
//        }
//
//        // check minimisation function
//        HashMap<String, Integer> objFunction = extractEquation(getObjectiveFunctionRow(lines), lines);
//        assert (objFunction.containsValue(5));
//        assert (objFunction.containsValue(10));
//        assert (objFunction.containsValue(7));
//        assert (objFunction.containsValue(8));
//        
//        FileUtils.deleteQuietly(new File("test_data/test_mps.mps"));
//    }
//
//    @Test
//    public void getInputStringColumnsTest2() throws Exception {
//
//        ISDSpecies target = MockISDSpecies.getC2H4();
//
//        HashSet<ISDSpecies> pool = new HashSet<ISDSpecies>();
//        pool.add(MockISDSpecies.getC2H6());
//        pool.add(MockISDSpecies.getC3H6());
//        pool.add(MockISDSpecies.getC3H8());
//
//        String mps = new ISDMPSFormat().getInputString(
//                target, pool, new VariableSet(new VariableFactory("v")));
//
//        FileUtils.writeStringToFile(new File("test_data/test_mps.mps"), mps, "UTF-8");
//
//        List<String> linesCrt = FileUtils.readLines(new File("test_data/test_mps.mps"));
//        List<String> linesCmp = FileUtils.readLines(new File("test_data/glpk/input/simple_input.mps"));
//
//        ArrayList<String> crtEq = getEquationRows(linesCrt);
//        ArrayList<String> cmpEq = getEquationRows(linesCmp);
//        assert (crtEq.size() == cmpEq.size());
//
//        HashMap<String, HashMap<String, Integer>> matCrt = new HashMap<String, HashMap<String, Integer>>();
//        HashMap<String, HashMap<String, Integer>> matCmp = new HashMap<String, HashMap<String, Integer>>();
//
//        for (String row : crtEq) {
//            matCrt.put(row, extractEquation(row, linesCrt));
//        }
//        for (String row : cmpEq) {
//            matCmp.put(row, extractEquation(row, linesCmp));
//        }
//
//        // check the equations
//        HashMap<String, ArrayList<Integer>> stoichCoeffCtr = compress(matCrt);
//        HashMap<String, ArrayList<Integer>> stoichCoeffCmp = compress(matCmp);
//
//        for (String var : stoichCoeffCmp.keySet()) {
//            assert (stoichCoeffCtr.containsKey(var));
//            assert (stoichCoeffCmp.get(var).size() == stoichCoeffCtr.get(var).size());
//            ArrayList<Integer> coveredIndices = new ArrayList<Integer>();
//            for (Integer value : stoichCoeffCmp.get(var)) {
//                boolean identified = false;
//                for (int i = 0; i < stoichCoeffCtr.get(var).size(); i++) {
//                    if (coveredIndices.contains(i)) {
//                        continue;
//                    }
//                    if (value == stoichCoeffCtr.get(var).get(i)) {
//                        coveredIndices.add(i);
//                        identified = true;
//                        break;
//                    }
//                }
//                assert (identified);
//            }
//        }
//        
//        FileUtils.deleteQuietly(new File("test_data/test_mps.mps"));
//    }
//
//    private HashMap<String, ArrayList<Integer>> compress(HashMap<String, HashMap<String, Integer>> mat) {
//        HashMap<String, ArrayList<Integer>> stoichCoeff = new HashMap<String, ArrayList<Integer>>();
//        for (String row : mat.keySet()) {
//            HashMap<String, Integer> eq = new HashMap<String, Integer>();
//            for (String var : eq.keySet()) {
//                if (!stoichCoeff.containsKey(var)) {
//                    stoichCoeff.put(var, new ArrayList<Integer>());
//                }
//                stoichCoeff.get(var).add(eq.get(var));
//            }
//        }
//        return stoichCoeff;
//    }
//    
//    private ArrayList<String> getRows(List<String> lines, String marker) {
//        ArrayList<String> eq = new ArrayList<String>();
//        boolean rowSec = false;
//        for (String line : lines) {
//            if (rowSec && !line.startsWith(" ")) {
//                break;
//            }
//            if (rowSec && line.trim().startsWith(marker)) {
//                // parse line
//                String[] items = line.trim().split(" ");
//                ArrayList<String> validItems = new ArrayList<String>();
//                for (int i = 0; i < items.length; i++) {
//                    if (!items[i].trim().isEmpty()) {
//                        validItems.add(items[i].trim());
//                    }
//                }
//                if (validItems.size() == 2) {
//                    eq.add(validItems.get(1));
//                }
//            }
//            if (line.startsWith("ROWS")) {
//                rowSec = true;
//            }
//        }
//        return eq;
//    }
//
//    private ArrayList<String> getEquationRows(List<String> lines) {
//        return getRows(lines, "E");
//    }
//
//    private String getObjectiveFunctionRow(List<String> lines) {
//        List<String> rows = getRows(lines, "N");
//        if (rows.size() > 0) {
//            return rows.get(0);
//        }
//        return null;
//    }
//
//    private HashMap<String, Integer> extractEquation(String row, List<String> lines) {
//        HashMap<String, Integer> eq = new HashMap<String, Integer>();
//        boolean colSec = false;
//        for (String line : lines) {
//            if (colSec && !line.startsWith(" ")) {
//                break;
//            }
//            if (colSec && line.contains(" " + row + " ")) {
//                // parse line
//                String[] items = line.trim().split(" ");
//                ArrayList<String> validItems = new ArrayList<String>();
//                for (int i = 0; i < items.length; i++) {
//                    if (!items[i].trim().isEmpty()) {
//                        validItems.add(items[i].trim());
//                    }
//                }
//                if (validItems.size() == 3) {
//                    eq.put(validItems.get(0), getInt(validItems.get(2)));
//                }
//                if (validItems.size() == 5) {
//                    if (validItems.get(1).equals(row)) {
//                        eq.put(validItems.get(0), getInt(validItems.get(2)));
//                    }
//                    if (validItems.get(3).equals(row)) {
//                        eq.put(validItems.get(0), getInt(validItems.get(4)));
//                    }
//                }
//            }
//            if (line.startsWith("COLUMNS")) {
//                colSec = true;
//            }
//        }
//        return eq;
//    }
//
//    private int getInt(String integerVal) {
//        String strVal = "";
//        for (int i = 0; i < integerVal.length(); i++) {
//            if (integerVal.charAt(i) == '.') {
//                return Integer.parseInt(strVal);
//            } else {
//                strVal = strVal + integerVal.charAt(i);
//            }
//        }
//        return 0;
//    }
}