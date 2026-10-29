package openjoe.smart.sso.base;

import openjoe.smart.sso.base.entity.ExpirationPolicy;
import openjoe.smart.sso.base.scheduler.SsoExpirationScheduler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

import java.util.List;

@EnableScheduling
@Configuration(proxyBeanMethods = false)
public class BaseAutoConfiguration {

    @Bean
    public SsoExpirationScheduler ssoExpirationScheduler(List<ExpirationPolicy> expirationList) {
        return new SsoExpirationScheduler(expirationList);
    }
}
