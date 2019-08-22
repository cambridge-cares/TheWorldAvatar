/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.sort;

import org.cam.ceb.como.tools.sort.Sort;
import org.cam.ceb.como.tools.sort.BubbleSort;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class BubbleSortTest {

    @Test
    public void sortTest() {
        Integer[] notSorted = new Integer[10];
        notSorted[0] = 1;
        notSorted[1] = 5;
        notSorted[2] = 8;
        notSorted[3] = 15;
        notSorted[4] = 69;
        notSorted[5] = 1;
        notSorted[6] = 0;
        notSorted[7] = -8;
        notSorted[8] = -9;
        notSorted[9] = 56;
        
        Integer[] sorted = new Integer[10];
        sorted[3] = 1;
        sorted[5] = 5;
        sorted[6] = 8;
        sorted[7] = 15;
        sorted[9] = 69;
        sorted[4] = 1;
        sorted[2] = 0;
        sorted[1] = -8;
        sorted[0] = -9;
        sorted[8] = 56;
        
        Sort<Integer> s = new BubbleSort<Integer>();
        s.sort(notSorted);
        
        for (int i = 0; i < sorted.length; i++) {
            assert(sorted[i] == notSorted[i]);
        }
    }
}
