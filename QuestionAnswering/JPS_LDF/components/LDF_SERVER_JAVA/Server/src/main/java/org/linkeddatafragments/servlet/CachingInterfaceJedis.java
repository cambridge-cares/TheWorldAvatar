package org.linkeddatafragments.servlet;
import redis.clients.jedis.Jedis;

public class CachingInterfaceJedis {

	public final Jedis jedis; 
	
	
    public CachingInterfaceJedis() {
    	this.jedis = new Jedis("localhost");
    }
	
    
    public String getValue(String key) {
    	return this.jedis.get(key);
    }
    
    public void setValue(String key, String value) {
    	this.jedis.set(key, value);
    }
    
    public boolean exist(String key) {
    	return this.jedis.exists(key);
    }
	
}
