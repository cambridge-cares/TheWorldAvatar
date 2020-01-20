/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data;

/**
 *
 * @author pb556
 */
public class DataPoint<T> {

    protected String id;
    protected T[] coordinate;
    protected Object value;

    public DataPoint(T[] coordinate, Object value) {
        this.coordinate = coordinate;
        this.value = value;
    }

    public void setCoordinate(T[] coordinate) {
        this.coordinate = coordinate;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public T[] getCoordinate() {
        return coordinate;
    }

    public Object getValue() {
        return value;
    }
}
