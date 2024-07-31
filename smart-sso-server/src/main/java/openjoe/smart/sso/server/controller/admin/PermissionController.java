package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.server.stage.core.Result;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.entity.Permission;
import openjoe.smart.sso.server.service.AppService;
import openjoe.smart.sso.server.service.PermissionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;

/**
 * @author Joe
 */
@Api(tags = "权限(含菜单)管理")
@Controller
@RequestMapping("/admin/permission")
@SuppressWarnings("rawtypes")
public class PermissionController {

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
    public Result get(@RequestParam Long id) {
        return Result.success(permissionService.getById(id));
    }

	@ApiOperation("权限树节点")
	@ResponseBody
	@RequestMapping(value = "/tree", method = RequestMethod.GET)
	public List<PermissionDTO> tree(
			@RequestParam(required = false) Long appId,
			@RequestParam(required = false) Long roleId,
			@RequestParam(required = false) Boolean isEnable) {
		return permissionService.selectTree(appId, roleId, isEnable);
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam(required = false) Long id,
			@RequestParam Long appId,
			@RequestParam(required = false) Long parentId,
			@RequestParam(required = false) String icon,
			@RequestParam String name,
			@RequestParam String url,
			@RequestParam Integer sort,
			@RequestParam Boolean isMenu,
			@RequestParam Boolean isEnable) {
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
		permissionService.saveOrUpdate(permission);
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@RequestParam Long id,
			@RequestParam Long appId) {
		permissionService.delete(id, appId);
		return Result.success().setMessage("删除成功");
	}
}