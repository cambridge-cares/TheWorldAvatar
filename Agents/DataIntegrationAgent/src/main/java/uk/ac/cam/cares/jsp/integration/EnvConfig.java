package uk.ac.cam.cares.jsp.integration;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String DB_3D = System.getenv("DB_3D");
    public static final String DB_2D = System.getenv("DB_2D");
    public static final String DB_2D_TABLE = System.getenv("DB_2D_TABLE");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}