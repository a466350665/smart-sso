package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.server.entity.Organization;
import openjoe.smart.sso.server.service.OrganizationService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;

/**
 * @author Joe
 */
@Tag(name = "机构")
@RestController
@RequestMapping("/admin/organization")
@SuppressWarnings("rawtypes")
public class OrganizationController {

	@Autowired
	private OrganizationService organizationService;

	@Operation(summary = "列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@RequestParam Long current,
			@RequestParam Long size) {
		return Result.success(organizationService.selectList(null, null, null, "--"));
	}

	@Operation(summary = "全部启用机构")
	@ResponseBody
	@RequestMapping(value = "/all", method = RequestMethod.GET)
	public Result<List<Organization>> all(
			@RequestParam(required = false) Long excludeId) {
		return Result.success(organizationService.selectList(true, null, excludeId, "--"));
	}

	@Operation(summary = "机构信息")
	@ResponseBody
	@RequestMapping(value = "/get", method = RequestMethod.GET)
	public Result<Organization> get(@RequestParam Long id) {
		return Result.success(organizationService.getById(id));
	}

	@Operation(summary = "新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam(required = false) Long id,
			@RequestParam(required = false) Long parentId,
			@RequestParam String name,
			@RequestParam Integer sort,
			@RequestParam Boolean isEnable
			) {
		Organization organization;
		if (id == null) {
			organization = new Organization();
		}
		else {
			organization = organizationService.getById(id);
		}
		organization.setParentId(parentId);
		organization.setName(name);
		organization.setSort(sort);
		organization.setIsEnable(isEnable);
		organizationService.saveOrUpdate(organization);
		return Result.success();
	}

	@Operation(summary = "启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
			@RequestParam String ids,
			@RequestParam Boolean isEnable) {
		organizationService.enable(isEnable, ConvertUtils.convertToIdList(ids));
		return Result.success();
	}

	@Operation(summary = "删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@RequestParam String ids) {
		organizationService.deleteByIds(ConvertUtils.convertToIdList(ids));
		return Result.success();
	}
}
