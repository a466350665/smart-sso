package com.smart.sso.server;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Import;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@ComponentScan(value = {"com.smart.sso.server.controller", "com.smart.sso.server.service.impl"})
@Import({WebMvcConfig.class, SmartSsoConfig.class, ConfigInitListener.class})
@EnableTransactionManagement
@MapperScan(basePackages = "com.smart.sso.server.dao")
@SpringBootApplication
public class SmartSsoServerApplication {

    public static void main(String[] args) {
        SpringApplication.run(SmartSsoServerApplication.class, args);
    }
}
