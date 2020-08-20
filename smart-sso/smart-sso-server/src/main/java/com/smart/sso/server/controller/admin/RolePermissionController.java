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
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.RolePermissionService;
import com.smart.sso.server.service.RoleService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

/**
 * @author Joe
 */
@Api(tags = "用户角色关系管理")
@Controller
@RequestMapping("/admin/rolePermission")
@SuppressWarnings("rawtypes")
public class RolePermissionController extends BaseController {

	@Autowired
	private RoleService roleService;
	@Autowired
	private AppService appService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String edit(
			@ApiParam(value = "角色id", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer roleId, Model model) {
		model.addAttribute("role", roleService.selectById(roleId));
		model.addAttribute("appList", appService.selectAll(true));
		return "/admin/rolePermission";
	}
	
	@ApiOperation("角色授权提交")
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ApiParam(value = "应用id", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer appId,
			@ApiParam(value = "角色id", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer roleId,
			@ApiParam(value = "权限ids") String permissionIds) {
		rolePermissionService.allocate(appId, roleId, convertToIdList(permissionIds));
		return Result.createSuccess().setMessage("授权成功");
	}
}