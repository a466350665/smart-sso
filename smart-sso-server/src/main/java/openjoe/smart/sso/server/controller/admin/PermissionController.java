package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.base.annotation.RequiresPermission;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.entity.Permission;
import openjoe.smart.sso.server.enums.ErrorCodeEnum;
import openjoe.smart.sso.server.enums.PermissionTypeEnum;
import openjoe.smart.sso.server.service.PermissionService;
import openjoe.smart.stage.core.entity.Result;
import openjoe.smart.stage.exception.ApplicationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.util.StringUtils;

import java.util.List;

/**
 * @author Joe
 */
@Tag(name = "权限(含菜单)管理")
@RestController
@RequestMapping("/admin/permission")
@SuppressWarnings("rawtypes")
public class PermissionController {

	private static final String CODE_PATTERN = "^[a-z][a-z0-9-]*(?::[a-z][a-z0-9-]*)+$";

	@Autowired
	private PermissionService permissionService;

	@Operation(summary = "获取")
	@RequiresPermission("permission:read")
    @ResponseBody
    @RequestMapping(value = "/get", method = RequestMethod.GET)
    public Result get(@RequestParam Long id) {
        return Result.success(permissionService.getById(id));
    }

	@Operation(summary = "权限树节点")
	@RequiresPermission("permission:read")
	@ResponseBody
	@RequestMapping(value = "/tree", method = RequestMethod.GET)
	public List<PermissionDTO> tree(
			@RequestParam(required = false) Long appId,
			@RequestParam(required = false) Long roleId,
			@RequestParam(required = false) Boolean isEnable) {
		return permissionService.selectTree(appId, roleId, isEnable);
	}

	@Operation(summary = "新增/修改提交")
	@RequiresPermission("permission:write")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam(required = false) Long id,
			@RequestParam Long appId,
			@RequestParam(required = false) Long parentId,
				@RequestParam(required = false) String icon,
				@RequestParam String name,
				@RequestParam String code,
				@RequestParam(required = false) String url,
				@RequestParam Integer sort,
				@RequestParam String type,
				@RequestParam Boolean isEnable) {
		if (!PermissionTypeEnum.isValid(type)) {
			throw new ApplicationException(ErrorCodeEnum.E010005);
		}
		if (!code.matches(CODE_PATTERN)) {
			throw new ApplicationException(ErrorCodeEnum.E010006);
		}
		if (PermissionTypeEnum.isMenu(type) && !StringUtils.hasText(url)) {
			throw new ApplicationException(ErrorCodeEnum.E010007);
		}
		if (PermissionTypeEnum.isFunction(type) && StringUtils.hasText(url)) {
			throw new ApplicationException(ErrorCodeEnum.E010008);
		}
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
		permission.setCode(code);
		permission.setUrl(PermissionTypeEnum.isMenu(type) ? url : null);
		permission.setSort(sort);
		permission.setType(type);
		permission.setIsEnable(isEnable);
		permissionService.saveOrUpdate(permission);
		return Result.success();
	}

	@Operation(summary = "删除")
	@RequiresPermission("permission:delete")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@RequestParam Long id,
			@RequestParam Long appId) {
		permissionService.delete(id, appId);
		return Result.success().setMessage("删除成功");
	}
}
