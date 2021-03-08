/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.sampler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;

/**
 *
 * @author pb556
 */
public class FloydsAlgorithmSampler implements Sampler {

    @Override
    public Collection randomSample(List c, int m) {
        int n = c.size();
        if (m > n) {
            return null;
        }
        ArrayList subSample = new ArrayList();
        return null;
    }

    
    

//function sample(list, m) {
//var n = list.length;
//if (m > n) return void console &&
//console.log('list length must be > sample');
//var sampleList = [];
//for (var i = n - m; i < n; i++) {
//var item = list[~~(Math.random() * i)];
//if (sampleList.indexOf(item) !== -1)
//sampleList.push(list[i]);
//else
//sampleList.push(item);
//}
//return sampleList;
//}
    
//    @Override
//    public Collection randomSample(List c, int m) {
//        Random rnd = new Random();
//        Collection res = new HashSet(m);
//        int n = c.size();
//        for (int i = n - m; i < n; i++) {
//            int pos = rnd.nextInt(i + 1);
//            if (res.contains(c.get(pos))) {
//                res.add(c.get(i));
//            } else {
//                res.add(c.get(pos));
//            }
//        }
//        return res;
//    }
}
