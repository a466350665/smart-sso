package openjoe.smart.sso.server.token.local;

import openjoe.smart.sso.base.entity.ExpirationPolicy;
import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.server.entity.CodeContent;
import openjoe.smart.sso.server.token.AbstractCodeManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地授权码管理
 *
 * @author Joe
 */
public class LocalCodeManager extends AbstractCodeManager implements ExpirationPolicy {

    protected final Logger logger = LoggerFactory.getLogger(LocalCodeManager.class);
    private Map<String, ExpirationWrapper<CodeContent>> codeMap = new ConcurrentHashMap<>();

    public LocalCodeManager(int timeout) {
        super(timeout);
    }

    @Override
    public void create(String code, CodeContent codeContent) {
        ExpirationWrapper<CodeContent> wrapper = new ExpirationWrapper<>(codeContent, getTimeout());
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
