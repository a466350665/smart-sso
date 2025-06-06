package openjoe.smart.sso.server.manager.local;

import openjoe.smart.sso.base.entity.ExpirationPolicy;
import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.server.entity.TicketGrantingTicketContent;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地登录凭证管理
 *
 * @author Joe
 */
public class LocalTicketGrantingTicketManager extends AbstractTicketGrantingTicketManager implements ExpirationPolicy {

    protected final Logger logger = LoggerFactory.getLogger(LocalTicketGrantingTicketManager.class);
    private Map<String, ExpirationWrapper<TicketGrantingTicketContent>> tgtMap = new ConcurrentHashMap<>();

    public LocalTicketGrantingTicketManager(int timeout, String cookieName, AbstractTokenManager tokenManager) {
        super(timeout, cookieName, tokenManager);
    }

    @Override
    public void create(String tgt, TicketGrantingTicketContent tgtContent) {
        ExpirationWrapper<TicketGrantingTicketContent> wrapper = new ExpirationWrapper<>(tgtContent, getTimeout());
        tgtMap.put(tgt, wrapper);
        logger.debug("登录凭证创建成功, tgt:{}", tgt);
    }

    @Override
    public TicketGrantingTicketContent get(String tgt) {
        ExpirationWrapper<TicketGrantingTicketContent> wrapper = tgtMap.get(tgt);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        }
        return wrapper.getObject();
    }

    @Override
    public void remove(String tgt) {
        tgtMap.remove(tgt);
        logger.debug("登录凭证删除成功, tgt:{}", tgt);
    }

    @Override
    public void refresh(String tgt) {
        ExpirationWrapper<TicketGrantingTicketContent> wrapper = tgtMap.get(tgt);
        if (wrapper != null) {
            wrapper.setExpired(System.currentTimeMillis() + getTimeout() * 1000);
        }
    }

    @Override
    public Map<String, TicketGrantingTicketContent> getTgtMap(Set<Long> userIds, Long current, Long size) {
        Map<String, TicketGrantingTicketContent> map = new LinkedHashMap<>();
        // 计算分页起始位置
        long start = (current - 1) * size;
        long end = start + size;
        long count = 0;

        for (Map.Entry<String, ExpirationWrapper<TicketGrantingTicketContent>> entry : tgtMap.entrySet()) {
            TicketGrantingTicketContent tgtContent = entry.getValue().getObject();
            if (CollectionUtils.isEmpty(userIds) || userIds.contains(tgtContent.getUserId())) {
                // 只有当count在分页范围内时才添加到结果中
                if (count >= start && count < end) {
                    map.put(entry.getKey(), tgtContent);
                }
                count++;

                // 如果已经收集到足够的数据，提前退出循环
                if (count >= end) {
                    break;
                }
            }
        }
        return map;
    }

    @Override
    public void verifyExpired() {
        tgtMap.forEach((tgt, wrapper) -> {
            if (wrapper.checkExpired()) {
                remove(tgt);
            }
        });
    }
}
