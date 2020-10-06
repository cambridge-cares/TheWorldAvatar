package uk.ac.cam.cares.jps.co2emissions.rabbitmq;
import java.io.IOException;

import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.Envelope;

public class ReceiveLogs {
  private static final String EXCHANGE_NAME = "jps.agents";
  private static int count = 0;
  
  public static int getCount() {
	  return count;
  }

  public static void setCount(int count) {
	  ReceiveLogs.count = count;
  }

  public static void main(String[] argv) throws Exception {
    ConnectionFactory factory = new ConnectionFactory();
    //factory.setHost("localhost");
	factory.setHost("www.theworldavatar.com");
	factory.setPort(83);
    Connection connection = factory.newConnection();
    Channel channel = connection.createChannel();

    channel.exchangeDeclare(EXCHANGE_NAME, BuiltinExchangeType.FANOUT);
    String queueName = channel.queueDeclare().getQueue();
    channel.queueBind(queueName, EXCHANGE_NAME, "");

    System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

    Consumer consumer = new DefaultConsumer(channel) {
      @Override
      public void handleDelivery(String consumerTag, Envelope envelope,
                                 AMQP.BasicProperties properties, byte[] body) throws IOException {
        String message = new String(body, "UTF-8");
        System.out.println(" [x] Received '" + message + "'");
        setCount(getCount() + 1);
        System.out.println(getCount());
      }
    };
    channel.basicConsume(queueName, true, consumer);
  }
}
