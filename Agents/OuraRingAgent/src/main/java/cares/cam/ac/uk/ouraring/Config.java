package cares.cam.ac.uk.ouraring;

public class Config {
    public static final String NAMESPACE = (System.getenv("NAMESPACE") != null) ? System.getenv("NAMESPACE") : "kb";
}
