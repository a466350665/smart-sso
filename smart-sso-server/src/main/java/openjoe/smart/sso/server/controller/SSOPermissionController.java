package openjoe.smart.sso.server.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.server.entity.TokenContent;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import openjoe.smart.sso.server.manager.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Joe
 */
@RestController
@RequestMapping(BaseConstant.PERMISSION_PATH)
public class SSOPermissionController {

	@Autowired
	private AbstractTokenManager tokenManager;
	@Autowired
	private UserManager userManager;

	@RequestMapping(method = RequestMethod.GET)
    public Result getUserPermission(@RequestParam(value = BaseConstant.ACCESS_TOKEN) String accessToken) {
		TokenContent tokenContent = tokenManager.getByAccessToken(accessToken);
		if(tokenContent == null){
			return Result.error("accessToken有误或已过期");
		}
        return Result.success(userManager.getUserPermission(tokenContent.getTokenUser().getId(), tokenContent.getClientId()));
    }
}