/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.classification;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.ceb.como.math.data.distance.DistanceMeasure;

/**
 *
 * @author pb556
 */

public abstract class ClassificationClass<T> extends ArrayList {
    
    protected String id = "";
    protected Rules cRule = new Rules();
    protected Rules pRule = new Rules();
    
    public ClassificationClass(String id, Rule classificationRule, Rule processingRule) {
        cRule = new Rules();
        cRule.add(classificationRule);
        pRule = new Rules();
        pRule.add(processingRule);
        this.id = id;
    }
    
    public ClassificationClass(String id, Rules classificationRules, Rules processingRules) {
        cRule = classificationRules;
        pRule = processingRules;
        this.id = id;
    }
    
//    public ClassificationClass(String id, Collection<Rule> classificationRules, Collection<Rule> processingRules) {
//        cRule = new Rules();
//        cRule.addAll(classificationRules);
//        pRule = new Rules();
//        pRule.addAll(processingRules);
//        this.id = id;
//    }
    
    public Rule getClassificationRule() {
        if (cRule == null || cRule.isEmpty()) {
            return null;
        }
        return (Rule) cRule.get(0);
    }
    
    public Rule getProcessingRule() {
        if (pRule == null || pRule.isEmpty()) {
            return null;
        }
        return (Rule) pRule.get(0);
    }
    
    public Rules getClassificationRules() {
        return cRule;
    }
    
    public Rules getProcessingRules() {
        return pRule;
    }
    
    public void setClassificationRule(Rule rule) {
        cRule = new Rules();
        cRule.add(rule);
    }
    
    public void setProcessingRule(Rule rule) {
        pRule = new Rules();
        pRule.add(rule);
    }
    
    public void set(String id, Rule classificationRule, Rule processingRule) {
        setClassificationRule(classificationRule);
        setProcessingRule(processingRule);
        setId(id);
    }
    
    public void setRules(Rule classificationRule, Rule processingRule) {
        setClassificationRule(classificationRule);
        setProcessingRule(processingRule);
    }
    
    public void setId(String id) {
        this.id = id;
    }
    
    public String getId() {
        return id;
    }
    
    public abstract boolean isMember(T obj, Object...additionalParameters) throws Exception;
    
    public abstract double getDistance(T obj, DistanceMeasure dist, Object...additionalParameters) throws Exception;
    
    public abstract Map getDistanceByAttribute(T obj, DistanceMeasure dist, Object...additionalParameters) throws Exception;
    
    public ArrayList<Boolean> applyClassificationRule(Object...additionalParameters) throws Exception {
        ArrayList<Boolean> success = new ArrayList<Boolean>();
        for (int i = 0; i < size(); i++) {
            success.add(applyClassificationRule(i, additionalParameters));
        }
        return success;
    }
    
    public abstract boolean applyClassificationRule(int index, Object...additionalParameters) throws Exception;
    
    public Map<T, Boolean> applyProcessingRule(Object ...additionalParameters) throws Exception {
        Map<T, Boolean> m = new HashMap<T, Boolean>();
        for (int i = 0; i < size(); i++) {
            m.put((T) get(i), applyProcessingRule(i, additionalParameters));
        }
        return m;
    }
    
    public abstract boolean applyProcessingRule(int index, Object...additionalParameters) throws Exception;
    
    @Override
    public String toString() {
        return id;
    }
}
