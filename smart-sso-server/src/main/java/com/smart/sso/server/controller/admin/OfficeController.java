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
	    @ValidateParam(name = "开始页码", defaultValue = DEFAULT_PAGE_NO) Integer pageNo,
        @ValidateParam(name = "显示条数", defaultValue = DEFAULT_PAGE_SIZE) Integer pageSize) {
		return Result.createSuccess(officeService.selectList(null, null, null, "----"));
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
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
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
	        @ValidateParam(name = "id") Integer id,
	        @ValidateParam(name = "父ID") Integer parentId,
	        @ValidateParam(name = "名称", value = { Validator.NOT_BLANK }) String name,
	        @ValidateParam(name = "排序", value = { Validator.NOT_BLANK }) Integer sort,
	        @ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable
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
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids,
	    @ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable) {
		officeService.enable(isEnable, convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids) {
		officeService.deleteByIds(convertToIdList(ids));
		return Result.success();
	}
}