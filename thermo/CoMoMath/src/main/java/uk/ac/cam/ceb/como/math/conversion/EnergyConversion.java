/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.conversion;

/**
 *
 * @author pb556
 */
public class EnergyConversion {
    
    private static double factorHaTokCalMol = 627.503;
    private static double factorHaTokJMol = 2625.5;
    private static double factorkCalMolTokJMol = 4.18400;
    
    public static double HaTokCalPerMol(double val) {
        return val * factorHaTokCalMol;
    }
    
    public static double kCalPerMolToHa(double val) {
        return val / factorHaTokCalMol;
    }
    
    public static double HaTokJPerMol(double val) {
        return val * factorHaTokJMol;
    }
    
    public static double kJPerMolToHa(double val) {
        return val / factorHaTokJMol;
    }
    
    public static double kCalPerMolTokJPerMol(double val) {
        return val * factorkCalMolTokJMol;
    }
    
    public static double kJPerMolTokCalPerMol(double val) {
        return val / factorkCalMolTokJMol;
    }
}
