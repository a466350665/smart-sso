package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ObjectWrapper;
import com.smart.sso.server.entity.CodeContent;
import com.smart.sso.server.token.CodeManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地授权码管理
 *
 * @author Joe
 */
public class LocalCodeManager extends CodeManager implements ExpirationPolicy {

    private Map<String, ObjectWrapper<CodeContent>> codeMap = new ConcurrentHashMap<>();

    @Override
    public void create(String code, CodeContent codeContent) {
        ObjectWrapper<CodeContent> wrapper = new ObjectWrapper<>(codeContent, getExpiresIn());
        codeMap.put(code, wrapper);
        logger.info("授权码生成成功, code:{}", code);
    }

    @Override
    public CodeContent get(String code) {
        ObjectWrapper<CodeContent> wrapper = codeMap.get(code);
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
                codeMap.remove(code);
                logger.info("授权码已失效, code:{}", code);
            }
        });
    }
}
