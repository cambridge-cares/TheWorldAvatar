package uk.ac.cam.ceb.como.io.chem.file.parser.formula;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author pb556
 */

public class EmpiricalFormula {

    private HashMap<String, Integer> atoms = new HashMap<String, Integer>();

    public void add(String elementSymbol, int count) {
        if (count > 0) {
            Integer pcount = atoms.get(elementSymbol);
            int ncount = (pcount == null) ? count : pcount + count;
            atoms.put(elementSymbol, ncount);
        }
    }

    public void add(EmpiricalFormula empiricalFormula) {
        Iterator<String> it = empiricalFormula.atoms.keySet().iterator();
        while (it.hasNext()) {
            String symbol = it.next();
            int n = empiricalFormula.atoms.get(symbol);
            add(symbol, n);
        }
    }

    @Override
    public String toString() {
        List<String> sorted_elem = getElementList();
        String emp_form = "";
        for (int i = 0; i < sorted_elem.size(); i++) {
            String symbol = sorted_elem.get(i);
            int n = atoms.get(symbol);
            String n_str = (n == 1) ? "" : Integer.toString(n);
            emp_form += symbol + n_str;
        }
        return emp_form;
    }

    private List<String> getElementList() {
        List<String> vec_elem = new ArrayList<String>();
        Iterator<String> it = atoms.keySet().iterator();
        while (it.hasNext()) {
            vec_elem.add(it.next());
        }
        Collections.sort(vec_elem, new ElementComparer());
        return vec_elem;
    }

    private class ElementComparer implements Comparator {

        @Override
        public int compare(Object obj1, Object obj2) {
            return ((String) obj1).compareTo((String) obj2);
        }
    }
}
