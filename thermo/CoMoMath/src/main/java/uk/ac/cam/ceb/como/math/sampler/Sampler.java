/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.sampler;

import java.util.Collection;
import java.util.List;

/**
 *
 * @author pb556
 */
public interface Sampler {
    
    // http://eyalsch.wordpress.com/2010/04/01/random-sample/
    public Collection randomSample(List c, int m);
    
}
