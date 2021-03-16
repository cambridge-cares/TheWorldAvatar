/* 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.classification;

import java.util.ArrayList;
import java.util.Collection;

/**
 *
 * @author pb556
 */
public abstract class Classifier<T> extends ArrayList<ClassificationClass> {

    protected int defaultClassIndex = -1;
    protected ClassificationClass defaultClass = null;

    public Classifier() {
    }

    public Classifier(ClassificationClass defaultClass) throws ClassificationException {
        setDefaultClass(defaultClass);
    }

    public Classifier(int defaultClassIndex) {
        this.defaultClassIndex = defaultClassIndex;
    }

    public abstract boolean classify(T obj) throws Exception;

    public boolean classify(Collection<T> set) throws Exception {
        boolean res = true;
        for (T t : set) {
            res &= classify(t);
        }
        return res;
    }

    public final void setDefaultClass(ClassificationClass c) throws ClassificationException {
        if (c == null) {
            throw new ClassificationException("Invalid default class!");
        }
        if (!contains(c)) {
            add(c);
        }
        defaultClass = c;
        defaultClassIndex = indexOf(c);
    }

    public final void setDefaultClass(int index) throws ClassificationException {
        if (index < 0 || index >= size()) {
            throw new ClassificationException("Invalid index!");
        }
        defaultClass = get(index);
        defaultClassIndex = index;
    }

    public ClassificationClass getDefaultClass() throws ClassificationException {
        if (defaultClass == null && defaultClassIndex < 0) {
            return null;
        }

        if (defaultClass == null) {
            defaultClass = get(defaultClassIndex);
        }

        return defaultClass;
    }

    public int getDefaultClassIndex() throws ClassificationException {
        if (defaultClass == null && defaultClassIndex < 0) {
            return -1;
        }

        if (defaultClassIndex < 0) {
            for (int i = 0; i < size(); i++) {
                if (get(i).equals(defaultClass)) {
                    defaultClassIndex = i;
                    break;
                }
            }
        }

        return defaultClassIndex;
    }

    public void clearClasses() {
        for (ClassificationClass c : this) {
            c.clear();
        }
    }

    public void applyProcessingRuleToAllClasses(Object... additionalParameters) throws Exception {
        for (ClassificationClass c : this) {
            c.applyProcessingRule(additionalParameters);
        }
    }

    public void applyProcessingRule(ClassificationClass c, Object... additionalParameters) throws Exception {
        for (ClassificationClass cObj : this) {
            if (c.getId().compareToIgnoreCase(cObj.getId()) == 0) {
                cObj.applyProcessingRule(additionalParameters);
            }
        }
    }

    public void applyProcessingRule(String classId, Object... additionalParameters) throws Exception {
        for (ClassificationClass cObj : this) {
            if (classId.compareToIgnoreCase(cObj.getId()) == 0) {
                cObj.applyProcessingRule(additionalParameters);
            }
        }
    }

    public void applyProcessingRule(int index, Object... additionalParameters) throws Exception {
        get(index).applyProcessingRule(additionalParameters);
    }
}
