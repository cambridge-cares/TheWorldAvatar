/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.pattern.composite;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author pb556
 */
public abstract class Composite extends Component {

    protected List<Component> children = new ArrayList<Component>();
    
    public boolean add(Component component) {
        return this.children.add(component);
    }
    
    public boolean addAll(Collection<Component> component) {
        return this.children.addAll(component);
    }

    public boolean remove(Component component) {
        return this.children.remove(component);
    }
    
    public boolean removeAll(Collection<Component> component) {
        return this.children.removeAll(component);
    }

    public List<Component> getChildren() {
        return this.children;
    }
}
