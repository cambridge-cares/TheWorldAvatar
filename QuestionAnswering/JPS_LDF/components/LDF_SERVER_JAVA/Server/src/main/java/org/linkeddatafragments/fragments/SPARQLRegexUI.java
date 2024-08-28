package org.linkeddatafragments.fragments;

import java.util.ArrayList;
import java.util.List;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;

public class SPARQLRegexUI {

	public static String[] test_set =  {
	    "O + CH3CHCO [=] CO + CH3CHO"
	  , 
	   "CH3CO + C2H5CHO =] CO + C2H5 + CH3CHO"
	  , 
	   "CH2CHO + C2H5CHO =] CO + C2H5 + CH3CHO"
	  , 
	    "CH3CO + CH2OHCHO =] CO + CH2OH + CH3CHO"
	  
	  , 
	    "CH3CO + CH2OHCHO =] CO + CH2O + C2H3"
	  , 
	     "CH2CHO + CH2OHCHO =] CO + CH2OH + CH3CHO"
	  , 
	     "CH3CO + CHOCHO =] CO + HCO + CH3CHO"
	  , 
	     "CH2CHO + CHOCHO =] CO + HCO + CH3CHO"
	  , 
	    "CH3CO + C2H3CHO =] CO + C2H3 + CH3CHO"
	  , 
	     "CH2CHO + C2H3CHO =] CO + C2H3 + CH3CHO"
	  , 
	    "CH3CO + CH2OHCH2CHO =] CO + PC2H4OH + CH3CHO"
	  , 
	    "CH2CHO + CH2OHCH2CHO =] CO + PC2H4OH + CH3CHO"
	  , 
	     "QALD3OO =] CO + OH + C2H4O2H + CH3CHO"
	  
	  , 	     "QALD3OO =] XXCS + DFD + SDFDS + ASDFD"

	  , 
	     "CH3CO + ETIBALDGG =] CO + CH2O + C2H3 + CH3CHO"
	  , 
	     "CH2CHO + ETIBALDGG =] CO + CH2O + C2H3 + CH3CHO"};
	
	
	public static void main(String[] args) {
		
        String reactant_template = ".* %s .*=\\].*|.*\\\"%s .*=\\].*";
        String product_template =  ".*=] %s .*|.*=].* %s\\\".*";
        
        String product1 = "CO";
        String product2 = "CH2O";
        String product3 = "C2H3";
    	//RegExp rr = new RegExp(String.format(reactant_template, "", reactant));
    	RegExp pr1 = new RegExp(String.format(product_template, product1, product1));
    	RegExp pr2 = new RegExp(String.format(product_template, product2, product2));
    	RegExp pr3 = new RegExp(String.format(product_template, product3, product3));
    	
    	
        List<Automaton> autos = new ArrayList<Automaton>(); 
        
    	autos.add(pr1.toAutomaton());
    	autos.add(pr2.toAutomaton());
    	
    	ArrayList<String> rst = new ArrayList<String>();
    	// autos.add(pr3.toAutomaton());
    	
		 System.out.println("starting ");

        for (Automaton aXX : autos) {
        	System.out.println("=========== auto ===============");
 
        	for (String t: test_set) {
        			if (aXX.run(t)) {
        				 System.out.println(t);

        			}
        		}   
			 System.out.println("=================");

        }
    	System.out.println(rst);
		}
	


}
