package com.smart.demo.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.demo.model.Demo;
import com.smart.demo.service.DemoService;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;

/**
 * @author Joe
 */
@Api(tags = "测试管理")
@Controller
@RequestMapping("/admin/demo")
public class DemoController extends BaseController {

	@Resource
	private DemoService demoService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute() {
		return "/admin/demo";
	}
	
	@ApiOperation("列表")
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(
			@ApiParam(value = "名称") String name,
			@ApiParam(value = "开始页码", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageNo,
			@ApiParam(value = "显示条数", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(
				demoService.findPaginationByName(name, new Pagination<Demo>(pageNo, pageSize)));
	}
	
	@ApiOperation("验证名称")
	@RequestMapping(value = "/validateName", method = RequestMethod.POST)
	public @ResponseBody Result validateName(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "名称", required = true) @ValidateParam({ Validator.NOT_BLANK }) String name) {
		Result result = Result.createSuccessResult();
		Demo demo = demoService.findByName(name);
		if (null != demo && !demo.getId().equals(id)) {
			result.setCode(ResultCode.ERROR).setMessage("名称已存在");
		}
		return result;
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ApiParam(value = "id") Integer id, Model model) {
		Demo demo;
		if (id == null) {
			demo = new Demo();
		}
		else {
			demo = demoService.get(id);
		}
		model.addAttribute("demo", demo);
		return "/admin/demoEdit";
	}

	@ApiOperation("新增/修改提交")
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "名称", required = true) @ValidateParam({ Validator.NOT_BLANK }) String name) {
		Result result = null;
		if (!(result = validateName(id, name)).isSuccess()) {
			return result;
		}
		Demo demo;
		if (id == null) {
			demo = new Demo();
		}
		else {
			demo = demoService.get(id);
		}
		demo.setName(name);
		demoService.save(demo);
		return Result.createSuccessResult();
	}

	@ApiOperation("删除")
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		demoService.deleteById(getAjaxIds(ids));
		return Result.createSuccessResult();
	}
}