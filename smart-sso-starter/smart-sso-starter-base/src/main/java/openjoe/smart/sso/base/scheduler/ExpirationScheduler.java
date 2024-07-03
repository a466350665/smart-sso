package openjoe.smart.sso.base.scheduler;

import openjoe.smart.sso.base.entity.ExpirationPolicy;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * 时效管理调度任务
 *
 * @author Joe
 */
public class ExpirationScheduler {

    private List<ExpirationPolicy> expirationList;

    public ExpirationScheduler(List<ExpirationPolicy> expirationList) {
        this.expirationList = expirationList;
    }

    /**
     * 定时清理过期凭证入口方法，默认每5分钟执行一次
     */
    @Scheduled(cron = "0 */5 * * * ?")
    public void verifyExpired() {
        if (CollectionUtils.isEmpty(expirationList)) {
            return;
        }
        expirationList.forEach(e -> e.verifyExpired());
    }
}