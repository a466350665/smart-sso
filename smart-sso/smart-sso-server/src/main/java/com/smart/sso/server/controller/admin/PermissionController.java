package com.smart.sso.server.controller.admin;

import java.util.List;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.App;
import com.smart.sso.server.model.Permission;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.PermissionService;

/**
 * 权限管理(含菜单权限)
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/permission")
public class PermissionController extends BaseController {

	@Resource
	private PermissionService permissionService;
	@Resource
	private AppService appService;

	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		model.addAttribute("appList", getAppList());
		return "/admin/permission";
	}

	@RequestMapping(value = "/nodes", method = RequestMethod.GET)
	public @ResponseBody List<Permission> nodes(
			@ValidateParam(name = "应用ID ") Integer appId,
			@ValidateParam(name = "名称") String name,
			@ValidateParam(name = "是否启用 ") Boolean isEnable) {
		List<Permission> list = permissionService.findByName(name, appId, isEnable);
		Permission permission = new Permission();
		permission.setId(null);
		permission.setParentId(-1);
		permission.setName("根节点");
		permission.setAppId(appId);
		list.add(0, permission);
		return list;
	}

	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ValidateParam(name = "ID") Integer id,
			@ValidateParam(name = "应用ID", validators = { Validator.NOT_BLANK }) Integer appId,
			@ValidateParam(name = "父ID") Integer parentId,
			@ValidateParam(name = "图标") String icon,
			@ValidateParam(name = "名称", validators = { Validator.NOT_BLANK }) String name,
			@ValidateParam(name = "权限URL", validators = { Validator.NOT_BLANK }) String url,
			@ValidateParam(name = "排序", validators = { Validator.NOT_BLANK }) Integer sort,
			@ValidateParam(name = "是否菜单", validators = { Validator.NOT_BLANK }) Boolean isMenu,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable) {
		Permission permission;
		if (id == null) {
			permission = new Permission();
		}
		else {
			permission = permissionService.get(id);
		}
		permission.setAppId(appId);
		permission.setParentId(parentId);
		permission.setIcon(icon);
		permission.setName(name);
		permission.setUrl(url);
		permission.setSort(sort);
		permission.setIsMenu(isMenu);
		permission.setIsEnable(isEnable);
		permissionService.save(permission);
		return Result.createSuccessResult().setMessage("保存成功");
	}

	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(
			@ValidateParam(name = "id", validators = { Validator.NOT_BLANK }) Integer id,
			@ValidateParam(name = "应用ID", validators = { Validator.NOT_BLANK }) Integer appId) {
		permissionService.deletePermission(id, appId);
		return Result.createSuccessResult().setMessage("删除成功");
	}

	private List<App> getAppList() {
		return appService.findByAll(null);
	}
}