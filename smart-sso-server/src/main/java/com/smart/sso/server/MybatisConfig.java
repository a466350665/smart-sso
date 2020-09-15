package com.smart.sso.server;

import org.mybatis.spring.boot.autoconfigure.ConfigurationCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.smart.mvc.interceptor.DynamicInterceptor;
import com.smart.mvc.interceptor.PageInterceptor;

@Configuration
public class MybatisConfig {

    @Bean
    ConfigurationCustomizer mybatisConfigurationCustomizer() {
        return new ConfigurationCustomizer() {
            @Override
            public void customize(org.apache.ibatis.session.Configuration configuration) {
                configuration.addInterceptor(new PageInterceptor());
                configuration.addInterceptor(new DynamicInterceptor());
            }
        };
    }
}
