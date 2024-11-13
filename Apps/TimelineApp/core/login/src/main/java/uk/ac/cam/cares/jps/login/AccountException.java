package uk.ac.cam.cares.jps.login;

public class AccountException extends Exception{
    public static String LOGIN_FAILURE = "Login failure";
    public static String CONNECTION_ERROR = "Connection error";
    public static String SKEW_SYSTEM_CLOCK = "Skew system clock";
    public static String SESSION_EXPIRED = "Token expired";
    public static String NO_UER_INFO_RETRIEVED = "No user info retrieved";

    public AccountException(String message) {
        super(message);
    }
}
