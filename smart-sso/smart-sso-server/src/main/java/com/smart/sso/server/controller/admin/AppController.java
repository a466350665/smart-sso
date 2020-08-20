package com.smart.sso.server.controller.admin;

import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.constant.ResultConstant;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Page;
import com.smart.mvc.model.Result;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.App;
import com.smart.sso.server.service.AppService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * @author Joe
 */
@Api(tags = "应用管理")
@Controller
@RequestMapping("/admin/app")
@SuppressWarnings("rawtypes")
public class AppController extends BaseController {

	@Autowired
	private AppService appService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute() {
		return "/admin/app";
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
		App app;
		if (id == null) {
			app = new App();
		}
		else {
			app = appService.selectById(id);
		}
		model.addAttribute("app", app);
		return "/admin/appEdit";
	}

    @ApiOperation("列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
	    @ValidateParam(name = "名称 ") String name,
			@ValidateParam(name = "开始页码", defaultValue = DEFAULT_PAGE_NO) Integer pageNo,
	        @ValidateParam(name = "显示条数", defaultValue = DEFAULT_PAGE_SIZE) Integer pageSize) {
		return Result.createSuccess(appService.selectPage(name, Page.create(pageNo, pageSize)));
	}

	@ApiOperation("验证应用编码")
	@ResponseBody
	@RequestMapping(value = "/validateCode", method = RequestMethod.POST)
	public Result validateCode(
	        @ValidateParam(name = "id") Integer id,
	        @ValidateParam(name = "应用编码", value = { Validator.NOT_BLANK }) String code) {
		App db = appService.selectByCode(code);
		if (null != db && !db.getId().equals(id)) {
			return Result.create(ResultConstant.ERROR, "应用编码已存在");
		}
		return Result.success();
	}

	@ApiOperation("启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
	        @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids,
	        @ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable) {
		appService.enable(isEnable, convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
	        @ValidateParam(name = "id") Integer id,
	        @ValidateParam(name = "名称", value = { Validator.NOT_BLANK }) String name,
	        @ValidateParam(name = "应用编码", value = { Validator.NOT_BLANK }) String code,
	        @ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable,
	        @ValidateParam(name = "排序", value = { Validator.NOT_BLANK, Validator.INT }) Integer sort) {
		App app;
		if (id == null) {
			app = new App();
			app.setCreateTime(new Date());
		}
		else {
			app = appService.selectById(id);
		}
		app.setName(name);
		app.setSort(sort);
		app.setIsEnable(isEnable);
		app.setCode(code);
		appService.save(app);
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids) {
		appService.deleteByIds(convertToIdList(ids));
		return Result.success();
	}
}