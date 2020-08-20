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
import com.smart.sso.server.model.Office;
import com.smart.sso.server.service.OfficeService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

/**
 * @author Joe
 */
@Api(tags = "机构")
@Controller
@RequestMapping("/admin/office")
@SuppressWarnings("rawtypes")
public class OfficeController extends BaseController {

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
			@ApiParam(value = "开始页码", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageNo,
			@ApiParam(value = "显示条数", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccess(officeService.selectList(null, null, null, "----"));
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ApiParam(value = "id") Integer id, Model model) {
		Office office;
		if (id == null) {
			office = new Office();
		}
		else {
			office = officeService.selectById(id);
		}
		model.addAttribute("officeList", officeService.selectList(null, null, id, "----"));
		model.addAttribute("office", office);
		return "/admin/officeEdit";
	}

	@ApiOperation("新增/修改提交")
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "父ID") Integer parentId,
			@ApiParam(value = "名称", required = true) @ValidateParam({ Validator.NOT_BLANK }) String name,
			@ApiParam(value = "排序", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer sort,
			@ApiParam(value = "是否启用", required = true) @ValidateParam({ Validator.NOT_BLANK }) Boolean isEnable
			) {
		Office office;
		if (id == null) {
			office = new Office();
		}
		else {
			office = officeService.selectById(id);
		}
		office.setParentId(parentId);
		office.setName(name);
		office.setSort(sort);
		office.setIsEnable(isEnable);
		officeService.save(office);
		return Result.success();
	}

	@ApiOperation("启用/禁用")
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public @ResponseBody Result enable(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids,
			@ApiParam(value = "是否启用", required = true) @ValidateParam({ Validator.NOT_BLANK }) Boolean isEnable) {
		officeService.enable(isEnable, convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("删除")
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		officeService.deleteByIds(convertToIdList(ids));
		return Result.success();
	}
}