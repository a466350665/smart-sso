package com.smart.sso.server;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

//@EnableRedisHttpSession
@EnableScheduling
@SpringBootApplication
public class SmartSsoServerApplication {

    public static void main(String[] args) {
        SpringApplication.run(SmartSsoServerApplication.class, args);
    }
}
