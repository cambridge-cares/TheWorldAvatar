package uk.ac.cam.ceb.como.paper.enthalpy.io;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

public class PrintingCrossValidationResults {

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
}
