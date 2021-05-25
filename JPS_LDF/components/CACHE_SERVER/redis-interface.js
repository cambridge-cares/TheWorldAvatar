const redis = require("redis");
const client = redis.createClient();

client.on("Redis error", function(error) {
  console.error(error);
});

 
module.exports = {
  set_value: function (key, value) {
	
	// check whether the key or value is null 
	if (key == null || value == null){
		return null
	}else{
		console.log('Redis: setting value', key, value);
		 // set the value in the redis server 
		client.set(key, value);
		return 'Good'
	}
  },
  get_value: function (key, callback) {
	  
	// check whether the key is null 
		client.get(key, function(err, reply) {
		  // reply is null when the key is missing
		  console.log(reply);
		  callback(reply);
		});
  }
};

 