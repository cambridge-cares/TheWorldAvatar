package uk.ac.cam.cares.jsp.integration;

public class App {
    public static void main(String[] args) {
        DataIntegration integrate = new DataIntegration();
        integrate.init();
        integrate.processRequestParameters();
    }
}
