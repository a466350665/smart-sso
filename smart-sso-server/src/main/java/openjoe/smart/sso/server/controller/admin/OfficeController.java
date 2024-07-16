package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.server.entity.Office;
import openjoe.smart.sso.server.service.OfficeService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * @author Joe
 */
@Api(tags = "机构")
@Controller
@RequestMapping("/admin/office")
@SuppressWarnings("rawtypes")
public class OfficeController {

	@Autowired
	private OfficeService officeService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "/admin/office";
	}
	
	@ApiOperation("列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@RequestParam Long current,
			@RequestParam Long size) {
		return Result.success(officeService.selectList(null, null, null, "----"));
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@RequestParam(required = false) Long id, Model model) {
		Office office;
		if (id == null) {
			office = new Office();
			office.setIsEnable(true);
		}
		else {
			office = officeService.getById(id);
		}
		model.addAttribute("officeList", officeService.selectList(null, null, id, "----"));
		model.addAttribute("office", office);
		return "/admin/office-edit";
	}

	@ApiOperation("新增/修改提交")
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

	@ApiOperation("启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
			@RequestParam String ids,
			@RequestParam Boolean isEnable) {
		officeService.enable(isEnable, ConvertUtils.convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@RequestParam String ids) {
		officeService.removeByIds(ConvertUtils.convertToIdList(ids));
		return Result.success();
	}
}