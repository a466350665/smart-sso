package com.smart.sso.server.controller.admin;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.App;
import com.smart.sso.server.model.Role;
import com.smart.sso.server.model.RolePermission;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.RolePermissionService;
import com.smart.sso.server.service.RoleService;

/**
 * 角色管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/role")
public class RoleController extends BaseController {

	@Resource
	private RoleService roleService;
	@Resource
	private AppService appService;
	@Resource
	private RolePermissionService rolePermissionService;

	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		model.addAttribute("appList", getAppList());
		return "/admin/role";
	}

	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
		Role role;
		if (id == null) {
			role = new Role();
		}
		else {
			role = roleService.get(id);
		}
		model.addAttribute("role", role);
		model.addAttribute("appList", getAppList());
		return "/admin/roleEdit";
	}

	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(@ValidateParam(name = "角色名") String name,
			@ValidateParam(name = "应用ID ") Integer appId,
			@ValidateParam(name = "开始页码", validators = { Validator.NOT_BLANK }) Integer pageNo,
			@ValidateParam(name = "显示条数 ", validators = { Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(roleService.findPaginationByName(name, appId, new Pagination<Role>(pageNo, pageSize)));
	}

	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public @ResponseBody Result enable(
			@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK })String ids,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable) {
		roleService.enable(isEnable, getAjaxIds(ids));
		return Result.createSuccessResult();
	}

	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(@ValidateParam(name = "ID") Integer id,
			@ValidateParam(name = "应用ID ", validators = { Validator.NOT_BLANK }) Integer appId,
			@ValidateParam(name = "角色名", validators = { Validator.NOT_BLANK }) String name,
			@ValidateParam(name = "排序", validators = { Validator.NOT_BLANK }) Integer sort,
			@ValidateParam(name = "描述") String description,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable) {
		Role role;
		if (id == null) {
			role = new Role();
		}
		else {
			role = roleService.get(id);
		}
		role.setAppId(appId);
		role.setName(name);
		role.setSort(sort);
		role.setDescription(description);
		role.setIsEnable(isEnable);
		roleService.saveOrUpdate(role);
		return Result.createSuccessResult();
	}
	
	@RequestMapping(value = "/allocate", method = RequestMethod.GET)
	public @ResponseBody Result allocate(
			@ValidateParam(name = "角色ID", validators = { Validator.NOT_BLANK }) Integer roleId) {
		return Result.createSuccessResult().setData(rolePermissionService.findByRoleId(roleId));
	}
	
	@RequestMapping(value = "/allocateSave", method = RequestMethod.POST)
	public @ResponseBody Result allocateSave(
			@ValidateParam(name = "应用ID ", validators = { Validator.NOT_BLANK }) Integer appId,
			@ValidateParam(name = "角色ID", validators = { Validator.NOT_BLANK }) Integer roleId,
			@ValidateParam(name = "权限IDS ", validators = { Validator.NOT_BLANK }) String permissionIds) {
		List<Integer> idList = getAjaxIds(permissionIds);
		List<RolePermission> list = new ArrayList<RolePermission>();
		Integer permissionId;
		for (Iterator<Integer> i$ = idList.iterator(); i$.hasNext(); list.add(new RolePermission(appId, roleId, permissionId))) {
			permissionId = i$.next();
		}
		return Result.createSuccessResult().setData(rolePermissionService.allocate(roleId, list)).setMessage("授权成功");
	}

	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids) {
		return Result.createSuccessResult().setData(roleService.deleteById(getAjaxIds(ids)));
	}

	private List<App> getAppList() {
		List<App> appList = null;
		if (appList == null) {
			appList = appService.findByAll(null);
		}
		return appList;
	}
}