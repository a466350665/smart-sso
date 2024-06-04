package com.smart.sso.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.redis.RedisAutoConfiguration;
import org.springframework.boot.autoconfigure.data.redis.RedisRepositoriesAutoConfiguration;

//@EnableRedisHttpSession
@SpringBootApplication
//@SpringBootApplication(exclude = { RedisAutoConfiguration.class, RedisRepositoriesAutoConfiguration.class })
public class SmartSsoDemoApplication {

	public static void main(String[] args) {
		SpringApplication.run(SmartSsoDemoApplication.class, args);
	}
}
