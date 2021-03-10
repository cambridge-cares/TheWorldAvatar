/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.file.writer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import org.apache.commons.io.FileUtils;

/**
 *
 * @author pb556
 */
public class StringWriter extends Writer {

    @Override
    public void write() throws Exception {
        if (new File(this.path).exists() && !this.overwrite) {
            throw new Exception("File already exists.");
        }
        FileUtils.writeStringToFile(new File(this.path), (String) this.content, this.encoding);
    }

    @Override
    public void append() throws Exception {
        if (!new File(this.path).exists()) {
            throw new Exception("File does not exist.");
        }
        FileUtils.writeStringToFile(new File(this.path), (String) this.content, this.encoding, true);
    }

    @Override
    public void insert(int line) throws Exception {
        File outFile = new File("temp.tmp");

        // input
        File inFile = new File(this.path);
        FileInputStream fis = new FileInputStream(inFile);
        BufferedReader in = new BufferedReader(new InputStreamReader(fis));

        // output         
        FileOutputStream fos = new FileOutputStream(outFile);
        PrintWriter out = new PrintWriter(fos);

        String thisLine;
        int i = 1;
        while ((thisLine = in.readLine()) != null) {
            if (i == line) {
                out.println((String) this.content);
            }
            out.println(thisLine);
            i++;
        }
        out.flush();
        out.close();
        in.close();

        inFile.delete();
        outFile.renameTo(inFile);
    }

    @Override
    public void clear() throws Exception {
        if (!new File(this.path).exists()) {
            throw new Exception("File does not exist.");
        }
        FileUtils.writeStringToFile(new File(this.path), "", this.encoding);
    }

    @Override
    public void delete(int start, int end) throws Exception {
        File outFile = new File("temp.tmp");

        // input
        File inFile = new File(this.path);
        FileInputStream fis = new FileInputStream(inFile);
        BufferedReader in = new BufferedReader(new InputStreamReader(fis));

        // output         
        FileOutputStream fos = new FileOutputStream(outFile);
        PrintWriter out = new PrintWriter(fos);

        String thisLine = "";
        int i = 1;
        while ((thisLine = in.readLine()) != null) {
            if (i >= start && i <= end) {
                continue;
            }
            out.println(thisLine);
            i++;
        }
        out.flush();
        out.close();
        in.close();

        inFile.delete();
        outFile.renameTo(inFile);
    }

    @Override
    public void delete(int start) throws Exception {
        this.delete(start, Integer.MAX_VALUE);
    }
}
