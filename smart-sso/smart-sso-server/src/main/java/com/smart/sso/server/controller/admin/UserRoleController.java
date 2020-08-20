package com.smart.sso.server.controller.admin;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.sso.server.enums.TrueFalseEnum;
import com.smart.sso.server.model.Role;
import com.smart.sso.server.model.UserRole;
import com.smart.sso.server.service.RoleService;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.service.UserService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;

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
	public String execute(@ApiParam(value = "userId", required = true) Integer userId, Model model) {
		model.addAttribute("user", userService.selectById(userId));
		model.addAttribute("roleList", getRoleList(userId));
		return "/admin/userRole";
	}

	@ApiOperation("新增/修改提交")
	@ApiResponse(response = Result.class, code = 200, message = "success")
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(@ApiParam(value = "userId") Integer userId,
			@ApiParam(value = "角色ids") String roleIds) {
		userRoleService.allocate(userId, createUserRoleList(userId, convertToIdList(roleIds)));
		return Result.success();
	}

	private List<UserRole> createUserRoleList(Integer userId, List<Integer> roleIdList) {
		List<UserRole> userRoleList = new ArrayList<UserRole>();
		UserRole bean;
		for (Integer roleId : roleIdList) {
			bean = new UserRole();
			bean.setUserId(userId);
			bean.setRoleId(roleId);
			userRoleList.add(bean);
		}
		return userRoleList;
	}

	private List<Role> getRoleList(Integer userId) {
		List<Role> list = roleService.selectAll(TrueFalseEnum.TRUE.getValue());
		if (userId != null) {
			for (Role role : list) {
				UserRole userRole = userRoleService.selectByUserRoleId(userId, role.getId());
				if (null != userRole) {
					role.setIsChecked(true);
				}
				else {
					role.setIsChecked(false);
				}
			}
		}
		return list;
	}
}