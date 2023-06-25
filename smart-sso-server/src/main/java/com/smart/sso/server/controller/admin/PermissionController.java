package com.smart.sso.server.controller.admin;

import com.smart.core.entity.Result;
import com.smart.sso.server.controller.BaseController;
import com.smart.sso.server.dto.PermissionDto;
import com.smart.sso.server.model.Permission;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.validator.ValidateParam;
import com.smart.sso.server.validator.Validator;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;

/**
 * @author Joe
 */
@Api(tags = "权限(含菜单)管理")
@Controller
@RequestMapping("/admin/permission")
@SuppressWarnings("rawtypes")
public class PermissionController extends BaseController {

	@Autowired
	private PermissionService permissionService;
	@Autowired
	private AppService appService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		model.addAttribute("appList", appService.selectAll(true));
		return "/admin/permission";
	}
	
	@ApiOperation("获取")
    @ResponseBody
    @RequestMapping(value = "/get", method = RequestMethod.GET)
    public Result get(@ValidateParam(name = "id", value = {Validator.NOT_BLANK}) Integer id) {
        return Result.createSuccess(permissionService.getById(id));
    }

	@ApiOperation("权限树节点")
	@ResponseBody
	@RequestMapping(value = "/tree", method = RequestMethod.GET)
	public List<PermissionDto> tree(
	    @ValidateParam(name = "应用id") Integer appId,
	    @ValidateParam(name = "角色id") Integer roleId,
	    @ValidateParam(name = "是否启用 ") Boolean isEnable) {
		return permissionService.selectTree(appId, roleId, isEnable);
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
	        @ValidateParam(name = "id") Integer id,
	        @ValidateParam(name = "应用id", value = { Validator.NOT_BLANK }) Integer appId,
	        @ValidateParam(name = "父id") Integer parentId,
	        @ValidateParam(name = "图标") String icon,
	        @ValidateParam(name = "名称", value = { Validator.NOT_BLANK }) String name,
			@ValidateParam(name = "权限URL", value = { Validator.NOT_BLANK }) String url,
			@ValidateParam(name = "排序", value = { Validator.NOT_BLANK }) Integer sort,
			@ValidateParam(name = "是否菜单", value = { Validator.NOT_BLANK }) Boolean isMenu,
			@ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable) {
		Permission permission;
		if (id == null) {
			permission = new Permission();
		}
		else {
			permission = permissionService.getById(id);
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
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@ValidateParam(name = "id", value = { Validator.NOT_BLANK }) Integer id,
			@ValidateParam(name = "应用id", value = { Validator.NOT_BLANK }) Integer appId) {
		permissionService.delete(id, appId);
		return Result.createSuccess().setMessage("删除成功");
	}
}