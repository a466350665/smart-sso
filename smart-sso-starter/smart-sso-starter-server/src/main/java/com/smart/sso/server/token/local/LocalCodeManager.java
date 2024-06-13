package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ExpirationWrapper;
import com.smart.sso.server.entity.CodeContent;
import com.smart.sso.server.token.CodeManager;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地授权码管理
 *
 * @author Joe
 */
public class LocalCodeManager extends CodeManager implements ExpirationPolicy {

    private Map<String, ExpirationWrapper<CodeContent>> codeMap = new ConcurrentHashMap<>();

    @Override
    public void create(String code, CodeContent codeContent) {
        ExpirationWrapper<CodeContent> wrapper = new ExpirationWrapper<>(codeContent, getExpiresIn());
        codeMap.put(code, wrapper);
        logger.debug("授权码创建成功, code:{}", code);
    }

    @Override
    public CodeContent get(String code) {
        ExpirationWrapper<CodeContent> wrapper = codeMap.get(code);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        }
        return wrapper.getObject();
    }

    @Override
    public void remove(String code) {
        codeMap.remove(code);
    }

    @Override
    public void verifyExpired() {
        codeMap.forEach((code, wrapper) -> {
            if (wrapper.checkExpired()) {
                remove(code);
                logger.debug("授权码已失效, code:{}", code);
            }
        });
    }
}
