package org.linkeddatafragments.servlet;

import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.redisson.Redisson;
import org.redisson.RedissonKeys;
import org.redisson.api.RBucket;
import org.redisson.api.RedissonClient;
import org.redisson.config.Config;

 
public class CachingInterfaceRedisson {
 
 	public RedissonClient redisson;
	
	
    public CachingInterfaceRedisson() {
    	this.redisson = Redisson.create();

    }
    
    
    // set the value in Redis cache server, save the value as the object 
    public void setValue(String key, ILinkedDataFragment value) {
    	
    	RBucket<ILinkedDataFragment> bucket = this.redisson.getBucket(key);
    	bucket.set(value);
    	
    }
	
    // get the value in Redis 
    public ILinkedDataFragment getValue(String key) {
    	
    	RBucket<ILinkedDataFragment> bucket = this.redisson.getBucket(key);
    	return bucket.get();
    	
    }
  
 
}
