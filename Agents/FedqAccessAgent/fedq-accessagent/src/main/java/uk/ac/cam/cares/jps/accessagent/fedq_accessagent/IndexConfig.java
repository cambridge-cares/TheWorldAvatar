package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;
import org.springframework.beans.factory.annotation.Value;

@Configuration
public class IndexConfig {

    @Value("${spring.data.redis.host}")
    private String redisHost;

    @Value("${spring.data.redis.port}")
    private int redisPort;

    @Bean
    public RedisConnectionFactory redisConnectionFactory() {
        return new LettuceConnectionFactory(redisHost, redisPort); // Ensure DragonflyDB is running
    }

    @Bean
    public RedisTemplate<String, String> redisTemplate() {
        RedisTemplate<String, String> template = new RedisTemplate<>();
        template.setConnectionFactory(redisConnectionFactory());
        //template.setKeySerializer(new StringRedisSerializer());
        //template.setValueSerializer(new StringRedisSerializer());
        return template;
    }

    @Bean
    public StringRedisTemplate stringRedisTemplate() {
        return new StringRedisTemplate(redisConnectionFactory());
    }

    // Pub/Sub Message Listener
    @Bean
    public RedisMessageListenerContainer container(RedisConnectionFactory connectionFactory,
            MessageListenerAdapter listenerAdapter) {
        RedisMessageListenerContainer container = new RedisMessageListenerContainer();
        container.setConnectionFactory(connectionFactory);
        container.addMessageListener(listenerAdapter, new PatternTopic("stackSyncChannel"));
        // container.addMessageListener(listenerAdapter, new PatternTopic("__keyevent@0__:set"));
        // container.addMessageListener(listenerAdapter, new PatternTopic("__keyevent@0__:del"));
        // container.addMessageListener(listenerAdapter, new PatternTopic("__keyevent@0__:expired"));
        return container;
    }

    @Bean
    public MessageListenerAdapter listenerAdapter(IndexBroadcastListener listener) {
        return new MessageListenerAdapter(listener);
    }
}
