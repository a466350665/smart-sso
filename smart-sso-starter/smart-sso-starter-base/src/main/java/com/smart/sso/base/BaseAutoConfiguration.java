package com.smart.sso.base;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.scheduler.ExpirationScheduler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

import java.util.List;

@EnableScheduling
@Configuration(proxyBeanMethods = false)
public class BaseAutoConfiguration {

    @Bean
    public ExpirationScheduler expirationScheduler(List<ExpirationPolicy> expirationList) {
        return new ExpirationScheduler(expirationList);
    }
}