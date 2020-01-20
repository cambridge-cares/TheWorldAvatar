/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.writer.gau;

/**
 *
 * @author pb556
 */
public interface GauWriterIntf {
    public void setRouteCard(String routeCard) throws Exception;
    public void setRMethod(String method) throws Exception;
    public void setUMethod(String method) throws Exception;
    public void setTitle(String title) throws Exception;
    public String getRouteCard() throws Exception;
    public String getRMethod() throws Exception;
    public String getUMethod() throws Exception;
    public String getTitle() throws Exception;
}
