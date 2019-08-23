package org.cam.ceb.como.enthalpy.calculator.variable;

/**
 * A factory class for Variable objects. It ensures that the variable id and name are
 * unique within the scope of the factory. This class is for internal use.
 *
 * @author wp214
 */
public class VariableFactory {

    private final String name;
    private int currentId = 0;

    public VariableFactory(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public synchronized Variable newVariable() {
        Variable variable = new Variable(_name(currentId));
        currentId++;
        return variable;
    }

    private String _name(int id) {
        return name + "[" + id + "]";
    }
    
    public synchronized void reset() {
        currentId = 0;
    }
}
