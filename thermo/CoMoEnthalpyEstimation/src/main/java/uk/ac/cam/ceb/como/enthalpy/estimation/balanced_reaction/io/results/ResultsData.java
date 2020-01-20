/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results;

import java.util.List;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */
public class ResultsData {
    
    // id, speciesid, energy, selected enthalpy of formation, all other calculated enthalpies, comment
    protected String id;
    protected Species species;
    protected ResultsValueDistributionList dist;
    protected String comment;
    private Logger logger = Logger.getLogger(getClass());
    
    public ResultsData(String id, String speciesId, double energy, double hf, ResultsValueDistributionList dist, String comment) {
        this.id = id;
        this.species = new Species(speciesId, hf, energy);
        this.dist = dist;
        this.comment = comment;
    }
    
    public ResultsData(String id, Species species, ResultsValueDistributionList dist, String comment) {
        this.id = id;
        this.species = species;
        this.dist = dist;
        this.comment = comment;
    }
    
    public ResultsData(String id, Species species, List<ReactionList> rList, String comment) throws Exception {
        this.id = id;
        this.species = species;
        validate(species, rList);
        this.dist = get(rList);
        this.comment = comment;
    }
    
    protected final void validate(Species s, List<ReactionList> rList) throws Exception {
        for (ReactionList list : rList) {
            for (Reaction r : list) {
                if (!s.equals(r.getSpecies(), true)) {
                    logger.error("An inconsistency within the submitted data has been identified.", new Exception("Data inconsistency."));
                }
            }
        }
    }
    
    protected final ResultsValueDistributionList get(List<ReactionList> rList) {
        ResultsValueDistributionList d = new ResultsValueDistributionList();
        for (ReactionList list : rList) {
            d.add(get(list));
        }
        return d;
    }
    
    protected final ResultsValueDistribution get(ReactionList list) {
        ResultsValueDistribution d = new ResultsValueDistribution();
        for (Reaction r : list) {
            if (r != null) {
                d.add(r.calculateHf());
            }
        }
        return d;
    }
    
    public String getId() {
        return id;
    }
    
    public String getComment() {
        return comment;
    }
    
    public Species getSpecies() {
        return species;
    }
    
    public double getEnergy() {
        return species.getTotalEnergy();
    }
    
    public double getHf() {
        return species.getHf();
    }
    
    public ResultsValueDistributionList getValueDistributions() {
        return dist;
    }
    
    @Override
    public String toString() {
        String sep = ", ";
        return "\"" + id + "\"" + sep + 
                "\"" + species.getRef() + "\"" + sep + 
                "\"" + getEnergy() + "\"" + sep + 
                "\"" + getHf() + "\"" + sep + 
                "\"" + dist.toString() + "\"" + sep + 
                "\"" + comment + "\"";
    }
    
    public boolean equals(ResultsData data) {
        double tolerance = 0.001;
        boolean equal = id.trim().compareToIgnoreCase(data.getId().trim()) == 0;
        equal &= species.getRef().trim().compareToIgnoreCase(data.getSpecies().getRef().trim()) == 0;
        equal &= Math.abs(getEnergy() - data.getEnergy()) < tolerance;
        equal &= Math.abs(getHf() - data.getHf()) < tolerance;
        //equal &= dist.toString().trim().compareToIgnoreCase(data.getValueDistributions().toString().trim()) == 0;
        return equal;
    }
}
