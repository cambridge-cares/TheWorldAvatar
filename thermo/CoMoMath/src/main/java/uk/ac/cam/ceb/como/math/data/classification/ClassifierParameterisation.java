/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.classification;

/**
 *
 * @author pb556
 */
public interface ClassifierParameterisation {
    
    // returns a parameterised classifier
    public abstract Classifier trainClassifier() throws Exception;
}
