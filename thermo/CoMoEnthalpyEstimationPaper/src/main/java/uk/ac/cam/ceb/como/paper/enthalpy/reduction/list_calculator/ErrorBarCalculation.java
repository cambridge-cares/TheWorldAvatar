package uk.ac.cam.ceb.como.paper.enthalpy.reduction.list_calculator;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

public class ErrorBarCalculation {


	   /**
  * @author nk510 ( caresssd@hermes.cam.ac.uk )
  * @author am2145( am2145@cam.ac.uk )
  * 
  * Method stores invalid or valid set of reactions in a txt file.
  * 
  * @param reactionFile The file that contains valid or invalid list of reactions
  * @param reactionList Generated reaction list
  * @throws IOException
  * 
  */
 public void generateInitialReactionListFile(BufferedWriter reactionFile, Map<Reaction, Double> reactionList) throws IOException {

 	for(Map.Entry<Reaction, Double> v: reactionList.entrySet()) {
     	
     	System.out.println("Reaction: " + v.getKey().toString() + " Calc enthalpy: " + v.getKey().calculateHf() + " Ref enthalpy: " + v.getKey().getSpecies().getHf() + " (error: " + v.getValue()+ " )");
     	
     	reactionFile.write("Reaction: " + v.getKey().toString() + " Calc enthalpy: " + v.getKey().calculateHf() + " Ref enthalpy: " + v.getKey().getSpecies().getHf() + " (error: " + v.getValue()+ " )");
     	
     	reactionFile.write("\n");
     }
 	
     reactionFile.close();
 }
 
 /**
  * @author nk510 ( caresssd@hermes.cam.ac.uk )
  * @author am2145( am2145@cam.ac.uk )
  * 
  * @param validSpeciesFile Target species file that contains valid species names.
  * @param validSpeciesSet The set of valid species.
  * @throws IOException the exception.
  */
 public void generateInitialValidSpeciesFile(BufferedWriter validSpeciesFile,  Set<Species> validSpeciesSet) throws IOException {
 	
 	for(Species s: validSpeciesSet) {
 		
         System.out.println("Species name: " + s.getRef());
        
         validSpeciesFile.write(s.getRef());
         
         validSpeciesFile.write("\n");
         
        }
 	
 	validSpeciesFile.close();
 	
 }
 
 /**
  * 
  * @author nk510 ( caresssd@hermes.cam.ac.uk )
  * @author am2145( am2145@cam.ac.uk )
  * 
  * @param invalidSpeciesFile The file where list of invalid species will be written.
  * @param invalidSpeciesSet The set of invalid species.
  * @param validSpeciesSet The set of valid species. 
  * @throws IOException the exception. 
  */
 public void generateInitialInvalidSpeciesFile(BufferedWriter invalidSpeciesFile, Set<Species> invalidSpeciesSet, Set<Species> validSpeciesSet) throws IOException {
 	
 	for(Species invs: invalidSpeciesSet) {
         
 	       if(!validSpeciesSet.contains(invs)) {
 	       
 	       System.out.println("Species name: " + invs.getRef());
 	       
 	       invalidSpeciesFile.write(invs.getRef());
 	       
            invalidSpeciesFile.write("\n");
 	       
 	       }
 	       
 	}
 	       
 	invalidSpeciesFile.close();
 	
 	
 }
 
 /**
 * @param invalidSpeciesFile The txt file where invlid species will be listed remaining after appliation cross validation algorithm in initial analysis.
 * @param tempInvalidSetOfSpecies The set of invalid species after completing initial analysis
 * @param invalidSpeciesSet Invalid set of species
 * @param validSpeciesSet  Valid set of species.
 * @throws IOException The IO exception.
 */
public void generateInvalidSpeciesFileAfterInitialAnalysis(int loop, BufferedWriter invalidSpeciesFile, Set<Species> tempInvalidSetOfSpecies, Map<Species, Double> sortedInvalidSpeciesErrorBar, Set<Species> invalidSpeciesSet, Set<Species> validSpeciesSet) throws IOException {
	 	
	System.out.println("Remaining invalid species after ("+ loop + ") loop: " + invalidSpeciesSet.size());
	
	 	for(Species invs: invalidSpeciesSet) {
	         
	 	       if(!validSpeciesSet.contains(invs)) {
	 	       
	 	       System.out.println("Species name: " + invs.getRef());
	 	       
	 	       tempInvalidSetOfSpecies.add(invs);
	 	       
//	 	       Double removedSpeciesErrorBar = (Double)sortedInvalidSpeciesErrorBar.remove(invs);
//	 	       
//	 	       System.out.println("Removed species : " + invs.getRef() + " with error bar: " + removedSpeciesErrorBar);
	 	       
	 	       invalidSpeciesFile.write(invs.getRef());
	 	       
	            invalidSpeciesFile.write("\n");
	 	       
	 	       }
	 	       
	 	}
	 	       
	 	invalidSpeciesFile.close();
	 	
	 	
	 }
 
	
    /**
     * 
     * @author nk510 ( caresssd@hermes.cam.ac.uk )
     * @author am2145( am2145@cam.ac.uk )
     * 
     * Calculation error bar for each species in invalid set of reactions.
     * 
     * @param invalidReaction The set of invalid reactions
     * @param validSpecies The initial set of valid species generated in pre-processing step of cross validation algorithm 
     * @param invalidSpecies The initial set of invalid species generated in pre-processing step of cross validation algorithm.
     * @return The hash map of species and their error bars.
     * 
     */
    
    public Map<Species, Double> calculateSpeciesErrorBar(Map<Reaction, Double> invalidReaction, Set<Species> validSpecies, Set<Species> invalidSpecies ) {
    
      Map<Species, Double> speciesErrorBarMap = new HashMap<Species, Double>();
    	
      Set<Species> uniqueinvSetOfSpecies  = new HashSet<Species>();
      
      for(Map.Entry<Reaction, Double> invspm: invalidReaction.entrySet()) {
     	 
      if(!uniqueinvSetOfSpecies.contains(invspm.getKey().getSpecies())) {
     	
     	uniqueinvSetOfSpecies.add(invspm.getKey().getSpecies());
     
      }
      
      }
     
      System.out.println("- - - -  - - - Unique species names from invalid reactions:  - - - - - -  - - - - - - ");
      
      for(Species s : uniqueinvSetOfSpecies) {
     	 
     	 System.out.println("Species name: "  + s.getRef());    
      }
      
      System.out.println("- - - -  - - -  Species names from invalid reactions and their error bars: - - - - - -  - - - - - - ");
      
      for(Species usp: uniqueinvSetOfSpecies ) {

          double errorSum = 0.0;
          int errorCount=0;
     	 double errorBar = 0.0;
     	 
     	 for(Map.Entry<Reaction, Double> m: invalidReaction.entrySet()) {
     		 
     		 if(m.getKey().getSpecies().getRef().equals(usp.getRef())) {
     			 
     			 errorSum = errorSum + m.getValue();
     			 
     			 errorCount++;
     		 }    		 
     	 }
     	 
      errorBar=errorSum/errorCount;
      
      System.out.println("Species name: " + usp.getRef() + " , error bar: "  + errorBar );
     	 
      speciesErrorBarMap.put(usp, errorBar);
      
      }
      
      Map<Species,Double> invalidSpeciesErrorBarMap = new HashMap<Species,Double>();
      
      for(Map.Entry<Species,Double> speciesMap : speciesErrorBarMap.entrySet()) {
     	 
     	 if(invalidSpecies.contains(speciesMap.getKey()) && (!validSpecies.contains(speciesMap.getKey()))) {
     	 
     	 invalidSpeciesErrorBarMap.put(speciesMap.getKey(), speciesMap.getValue());
     	 
           }
      }
      
      System.out.println();
      
      System.out.println("-----------------Species from invalid (rejected) list with error bar: ");
      
      for(Map.Entry<Species, Double> invmap: invalidSpeciesErrorBarMap.entrySet()) {
     	 
     	 System.out.println(invmap.getKey().getRef()+ " " + invmap.getValue()) ;
      }
    
    return invalidSpeciesErrorBarMap;
    
    }
    
    /**
     * 
     * @author nk510 ( caresssd@hermes.cam.ac.uk )
     * @author am2145( am2145@cam.ac.uk )
     * 
     * @param invalidSpeciesErrorBarMap The hash map that contains species names from invalid list of species and their error bars.
     * @return maximum error bar.
     */
    public double getMaximumErrorBar(Map<Species, Double> invalidSpeciesErrorBarMap) {
    	
    	 double maxErrorBar = Collections.max(invalidSpeciesErrorBarMap.values());
         
         for(Map.Entry<Species,Double> invspeciesMap : invalidSpeciesErrorBarMap.entrySet()) {
        
        	 if(invspeciesMap.getValue()==maxErrorBar) {
        		 
       		 System.out.println("Species name : " + invspeciesMap.getKey().getRef() + " error bar from map : " +  invspeciesMap.getValue() + " max error bar: " + maxErrorBar);
        		 
        	 break;
        	 }
        	 
        }
         
return maxErrorBar;

    }
    
}
