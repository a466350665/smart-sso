package com.smart.sso.server.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.service.RoleService;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.service.UserService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * @author Joe
 */
@Api(tags = "用户角色管理")
@Controller
@RequestMapping("/admin/userRole")
@SuppressWarnings("rawtypes")
public class UserRoleController extends BaseController {

	@Autowired
	private UserService userService;
	@Autowired
	private RoleService roleService;
	@Autowired
	private UserRoleService userRoleService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(
	        @ValidateParam(name = "userId", value = { Validator.NOT_BLANK }) Integer userId, 
	        Model model) {
		model.addAttribute("user", userService.get(userId));
		model.addAttribute("roleList", roleService.getRoleList(userId));
		return "/admin/userRole";
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
            @ValidateParam(name = "userId", value = { Validator.NOT_BLANK }) Integer userId,
    		@ValidateParam(name = "角色ids") String roleIds) {
	    userRoleService.allocate(userId, convertToIdList(roleIds));
		return Result.success();
	}
}