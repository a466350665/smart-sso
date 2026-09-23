package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.server.entity.Office;
import openjoe.smart.sso.server.service.OfficeService;
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
@RequestMapping("/admin/office")
@SuppressWarnings("rawtypes")
public class OfficeController {

	@Autowired
	private OfficeService officeService;

	@Operation(summary = "列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@RequestParam Long current,
			@RequestParam Long size) {
		return Result.success(officeService.selectList(null, null, null, "--"));
	}

	@Operation(summary = "全部启用机构")
	@ResponseBody
	@RequestMapping(value = "/all", method = RequestMethod.GET)
	public Result<List<Office>> all(
			@RequestParam(required = false) Long excludeId) {
		return Result.success(officeService.selectList(true, null, excludeId, "--"));
	}

	@Operation(summary = "机构信息")
	@ResponseBody
	@RequestMapping(value = "/get", method = RequestMethod.GET)
	public Result<Office> get(@RequestParam Long id) {
		return Result.success(officeService.getById(id));
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
		Office office;
		if (id == null) {
			office = new Office();
		}
		else {
			office = officeService.getById(id);
		}
		office.setParentId(parentId);
		office.setName(name);
		office.setSort(sort);
		office.setIsEnable(isEnable);
		officeService.saveOrUpdate(office);
		return Result.success();
	}

	@Operation(summary = "启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
			@RequestParam String ids,
			@RequestParam Boolean isEnable) {
		officeService.enable(isEnable, ConvertUtils.convertToIdList(ids));
		return Result.success();
	}

	@Operation(summary = "删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@RequestParam String ids) {
		officeService.removeByIds(ConvertUtils.convertToIdList(ids));
		return Result.success();
	}
}
