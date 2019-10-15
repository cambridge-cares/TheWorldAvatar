/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 *
 * @author pb556
 */
public class ForwardLineReader extends LineReader {

    private long filePos;

    public ForwardLineReader(File file, String encoding) throws IOException {
        super(file, encoding);
        filePos = 0;
    }

    @Override
    public String readLine() throws IOException {
        BufferedReader br = new BufferedReader(new FileReader(file));
        String line = "";
        for (int i = 0; i < filePos; i++) {
            line = br.readLine();
        }
        return line;
    }
}