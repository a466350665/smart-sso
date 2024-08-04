package openjoe.smart.sso.client.util;

import com.fasterxml.jackson.core.type.TypeReference;
import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.util.HttpUtils;
import openjoe.smart.sso.base.util.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

/**
 * 权限辅助类
 *
 * @author Joe
 */
public class PermissionUtils {

    private static final Logger logger = LoggerFactory.getLogger(PermissionUtils.class);

    /**
     * 获取当前用户所有权限(含菜单)
     *
     * @param serverUrl
     * @param accessToken
     * @return
     */
    public static Result<TokenPermission> getUserPermission(String serverUrl, String accessToken) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put(BaseConstant.ACCESS_TOKEN, accessToken);
        String jsonStr = HttpUtils.get(serverUrl + BaseConstant.PERMISSION_PATH, paramMap);
        if (jsonStr == null || jsonStr.isEmpty()) {
            logger.error("getUserPermission return null. accessToken:{}", accessToken);
            return Result.error("获取用户权限失败");
        }
        Result<TokenPermission> result = JsonUtils.parseObject(jsonStr, new TypeReference<Result<TokenPermission>>() {
        });
        if (result == null) {
            logger.error("parse userPermissionList return null. jsonStr:{}", jsonStr);
            return Result.error("解析用户权限失败");
        }
        return result;
    }
}