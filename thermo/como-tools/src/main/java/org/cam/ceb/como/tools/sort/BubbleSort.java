/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.sort;

/**
 *
 * @author pb556
 */
public class BubbleSort<T extends Comparable<T>> implements Sort<T> {

    public void sort(T[] values) {
        for (int i = 0; i < values.length; i++) {
            for (int j = i + 1; j < values.length; j++) {
                if (values[i].compareTo(values[j]) > 0) {
                    T v = values[i];
                    values[i] = values[j];
                    values[j] = v;
                }
            }
        }
    }
    
}
