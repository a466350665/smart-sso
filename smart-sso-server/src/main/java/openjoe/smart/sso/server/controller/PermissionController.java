package openjoe.smart.sso.server.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;

/**
 * @author Joe
 */
@RestController
@RequestMapping("/sso/permission")
public class PermissionController {

    @GetMapping(value = "/user-permission-list")
    public Result getUserPermissionList(
			@RequestParam Integer userId,
			@RequestParam(value = BaseConstant.APP_KEY) String appKey,
			@RequestParam(value = BaseConstant.APP_SECRET) String appSecret) {
        return Result.createSuccess(Collections.emptyList());
    }

	@GetMapping(value = "/application-permission-list")
	public Result getApplicationPermissionList(
			@RequestParam(value = BaseConstant.APP_KEY) String appKey,
			@RequestParam(value = BaseConstant.APP_SECRET) String appSecret) {
		return Result.createSuccess(Collections.emptyList());
	}
}