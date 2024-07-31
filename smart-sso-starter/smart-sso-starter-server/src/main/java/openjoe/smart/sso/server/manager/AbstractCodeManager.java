package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.LifecycleManager;
import openjoe.smart.sso.server.entity.CodeContent;

import java.util.UUID;

/**
 * 授权码code管理
 *
 * @author Joe
 */
public abstract class AbstractCodeManager implements LifecycleManager<CodeContent> {

    private int timeout;

    public AbstractCodeManager(int timeout) {
        this.timeout = timeout;
    }

    /**
     * 创建授权码
     *
     * @param tgt
     * @param clientId
     * @return
     */
    public String create(String tgt, String clientId) {
        String code = "Code-" + UUID.randomUUID().toString().replaceAll("-", "").substring(0, 10);
        create(code, new CodeContent(tgt, clientId));
        return code;
    }

    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }
}
