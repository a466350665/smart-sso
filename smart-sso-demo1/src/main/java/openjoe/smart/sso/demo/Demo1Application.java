package openjoe.smart.sso.demo;

import openjoe.smart.sso.client.ClientProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
public class Demo1Application {

    public static void main(String[] args) {
        ConfigurableApplicationContext ac = SpringApplication.run(Demo1Application.class, args);
        ClientProperties c1 = ac.getBean(ClientProperties.class);
        System.out.println(c1.getAppId());
    }
}
