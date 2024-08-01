const redis = require('./redis-interface');

rst = redis.set_value('foo', JSON.stringify(['x','y']))


redis.get_value('foo', function(value){
	console.log('value', value)
});
