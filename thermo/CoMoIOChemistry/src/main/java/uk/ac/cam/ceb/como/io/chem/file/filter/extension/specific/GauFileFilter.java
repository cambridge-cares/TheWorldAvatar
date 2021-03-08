/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.filter.extension.specific;

import com.cmclinnovations.io.file.filter.extension.ValidExtensionFileFilter;

/**
 *
 * @author pb556
 */
public class GauFileFilter extends ValidExtensionFileFilter {

    public GauFileFilter() {
        super(new String[]{"gau"});
    }
    
}

