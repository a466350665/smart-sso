package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
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

	private Map<String, CodeWrapper> codeMap = new ConcurrentHashMap<>();
	
	@Override
	public void create(String code, CodeContent codeContent) {
		codeMap.put(code, new CodeWrapper(codeContent, System.currentTimeMillis() + getExpiresIn() * 1000));
		logger.info("授权码生成成功, code:{}", code);
	}

	@Override
	public CodeContent getAndRemove(String code) {
		CodeWrapper wrapper = codeMap.remove(code);
        if (wrapper == null || System.currentTimeMillis() > wrapper.expired) {
            return null;
        }
        return wrapper.codeContent;
	}

	@Override
    public void verifyExpired() {
		codeMap.forEach((code, wrapper) -> {
            if (System.currentTimeMillis() > wrapper.expired) {
                codeMap.remove(code);
                logger.info("授权码已失效, code:{}", code);
            }
        });
    }
	
    private class CodeWrapper {
    	private CodeContent codeContent;
        private long expired; // 过期时间

        public CodeWrapper(CodeContent codeContent, long expired) {
            this.codeContent = codeContent;
            this.expired = expired;
        }
    }
}
