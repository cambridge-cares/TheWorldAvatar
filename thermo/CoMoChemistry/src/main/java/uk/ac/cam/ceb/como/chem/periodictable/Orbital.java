/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.chem.periodictable;

/**
 *
 * @author pb556
 */
public enum Orbital {

    s ("s", 1, 2), p ("p", 2, 6), d ("d", 3, 10), f ("f", 4, 14), undefined ("u", 0, 0);
    
    private String value;
    private int maxNumEl;
    private int order;
    
    Orbital(String value, int order, int maxNumEl) {
        this.value = value;
        this.maxNumEl = maxNumEl;
        this.order = order;
    }
    
    public int getMaxNumberOfElectrons() {
        return maxNumEl;
    }
    
    public int getOrder() {
        return order;
    }
    
    @Override
    public String toString() {
        return value;
    }
}
