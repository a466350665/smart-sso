package com.smart.sso.base.scheduler;

import com.smart.sso.base.entity.ExpirationPolicy;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 *  时效管理调度任务
 */
public class ExpirationScheduler {

    private List<ExpirationPolicy> expirationList;

    public ExpirationScheduler(List<ExpirationPolicy> expirationList) {
        this.expirationList = expirationList;
    }

    /**
     * 每5分钟执行一次
     */
    @Scheduled(cron = "0 */5 * * * ?")
    public void verifyExpired() {
        if(CollectionUtils.isEmpty(expirationList)){
            return;
        }
        expirationList.forEach(e->e.verifyExpired());
    }
}