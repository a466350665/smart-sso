package openjoe.smart.sso.server.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.server.entity.TokenContent;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import openjoe.smart.sso.server.manager.AppManager;
import openjoe.smart.sso.server.manager.PermissionManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;

/**
 * @author Joe
 */
@RestController
@RequestMapping(BaseConstant.PERMISSION_PATH)
public class SSOPermissionController {

    @Autowired
    private AbstractTokenManager tokenManager;
    @Autowired
    private AppManager appManager;
    @Autowired
    private PermissionManager permissionManager;

    @RequestMapping(method = RequestMethod.GET)
    public Result getUserPermission(@RequestParam(value = BaseConstant.ACCESS_TOKEN) String accessToken) {
        TokenContent tokenContent = tokenManager.getByAccessToken(accessToken);
        if (tokenContent == null) {
            return Result.error("accessToken有误或已过期");
        }
        Result<Long> appResult = appManager.validate(tokenContent.getClientId());
        if (!appResult.isSuccess()) {
            return Result.success(new TokenPermission(Collections.emptySet(), Collections.emptySet(), Collections.emptyList()));
        }
        return Result.success(permissionManager.getUserPermission(tokenContent.getUserId(), appResult.getData()));
    }
}