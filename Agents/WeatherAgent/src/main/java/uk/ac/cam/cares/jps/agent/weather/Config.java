package uk.ac.cam.cares.jps.agent.weather;

public class Config{
	public static String apikey = System.getenv("API_KEY"); // api key for open weather
	public static String dburl = System.getenv("POSTGRES_URL");
	public static String dbuser = System.getenv("POSTGRES_USER");
	public static String dbpassword = System.getenv("POSTGRES_PASSWORD");
    
	public static String kgurl = System.getenv("KG_URL");
	public static String kguser = System.getenv("KG_USER");
	public static String kgpassword = System.getenv("KG_PASSWORD");
}
