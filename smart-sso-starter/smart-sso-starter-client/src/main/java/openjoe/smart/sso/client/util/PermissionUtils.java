package openjoe.smart.sso.client.util;

import com.fasterxml.jackson.core.type.TypeReference;
import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermissionDTO;
import openjoe.smart.sso.base.util.HttpUtils;
import openjoe.smart.sso.base.util.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
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
     * @param userId
     * @param appKey
     * @return
     */
    public static Result<List<TokenPermissionDTO>> getUserPermissionList(String serverUrl, Integer userId, String appKey, String appSecret) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put("userId", userId.toString());
        paramMap.put(BaseConstant.APP_KEY, appKey);
        paramMap.put(BaseConstant.APP_SECRET, appSecret);
        String jsonStr = HttpUtils.get(serverUrl + "/sso/permission/user-permission-list", paramMap);
        if (jsonStr == null || jsonStr.isEmpty()) {
            logger.error("getUserPermissionList return null. userId:{}", userId);
            return Result.createError("获取用户权限失败");
        }
        Result<List<TokenPermissionDTO>> result = JsonUtils.parseObject(jsonStr, new TypeReference<Result<List<TokenPermissionDTO>>>() {
        });
        if (result == null) {
            logger.error("parse userPermissionList return null. jsonStr:{}", jsonStr);
            return Result.createError("解析用户权限失败");
        }
        return result;
    }

    /**
     * 获取当前应用所有权限(含菜单)
     *
     * @param appKey
     * @param appSecret
     * @return
     */
    public static Result<List<TokenPermissionDTO>> getApplicationPermissionList(String serverUrl, String appKey, String appSecret) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put(BaseConstant.APP_KEY, appKey);
        paramMap.put(BaseConstant.APP_SECRET, appSecret);
        String jsonStr = HttpUtils.get(serverUrl + "/sso/permission/application-permission-list", paramMap);
        if (jsonStr == null || jsonStr.isEmpty()) {
            logger.error("getApplicationPermissionList return null. appKey:{}", appKey);
            return Result.createError("获取应用权限失败");
        }
        Result<List<TokenPermissionDTO>> result = JsonUtils.parseObject(jsonStr, new TypeReference<Result<List<TokenPermissionDTO>>>() {
        });
        if (result == null) {
            logger.error("parse applicationPermissionList return null. jsonStr:{}", jsonStr);
            return Result.createError("解析应用权限失败");
        }
        return result;
    }
}