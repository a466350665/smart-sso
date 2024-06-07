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
public class LocalCodeManager implements CodeManager, ExpirationPolicy {
	
	private final Logger logger = LoggerFactory.getLogger(getClass());

	private Map<String, ObjectWrapper<CodeContent>> codeMap = new ConcurrentHashMap<>();
	
	@Override
	public void create(String code, CodeContent codeContent) {
        ObjectWrapper<CodeContent> wrapper = new ObjectWrapper<>(codeContent, System.currentTimeMillis() + getExpiresIn() * 1000);
		codeMap.put(code, wrapper);
		logger.info("授权码生成成功, code:{}", code);
	}

	@Override
	public CodeContent getAndRemove(String code) {
        ObjectWrapper<CodeContent> wrapper = codeMap.remove(code);
        if (wrapper == null || System.currentTimeMillis() > wrapper.getExpired()) {
            return null;
        }
        return wrapper.getObject();
	}

	@Override
    public void verifyExpired() {
		codeMap.forEach((code, wrapper) -> {
            if (System.currentTimeMillis() > wrapper.getExpired()) {
                codeMap.remove(code);
                logger.info("授权码已失效, code:{}", code);
            }
        });
    }
}
