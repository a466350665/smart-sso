package com.smart.sso.server.token;

import com.smart.sso.base.entity.LifecycleManager;
import com.smart.sso.server.entity.CodeContent;

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
     * @param redirectUri
     * @return
     */
    public String create(String tgt, String redirectUri) {
        String code = "Code-" + UUID.randomUUID().toString().replaceAll("-", "").substring(0, 10);
        create(code, new CodeContent(tgt, redirectUri));
        return code;
    }

    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }
}
