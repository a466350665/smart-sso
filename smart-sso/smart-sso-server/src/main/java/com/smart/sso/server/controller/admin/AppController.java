package com.smart.sso.server.controller.admin;

import java.util.Date;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.util.SpringUtils;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.App;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.impl.PermissionSubject;
import com.smart.util.StringUtils;

/**
 * 应用管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/app")
public class AppController extends BaseController {

	@Resource
	private AppService appService;

	@RequestMapping(method = RequestMethod.GET)
	public String execute() {
		return "/admin/app";
	}

	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
		App app;
		if (id == null) {
			app = new App();
		}
		else {
			app = appService.get(id);
		}
		model.addAttribute("app", app);
		return "/admin/appEdit";
	}

	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(
			@ValidateParam(name = "名称 ") String name,
			@ValidateParam(name = "开始页码", validators = { Validator.NOT_BLANK }) Integer pageNo,
			@ValidateParam(name = "显示条数 ", validators = { Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(appService.findPaginationByName(name, new Pagination<App>(pageNo, pageSize)));
	}

	@RequestMapping(value = "/validateCode", method = RequestMethod.POST)
	public @ResponseBody Result validateCode(
			@ValidateParam(name = "id") Integer id,
			@ValidateParam(name = "应用编码 ", validators = { Validator.NOT_BLANK }) String code) {
		Result result = Result.createSuccessResult();
		if (StringUtils.isNotBlank(code)) {
			App db = appService.findByCode(code);
			if (null != db && !db.getId().equals(id)) {
				result.setCode(ResultCode.ERROR).setMessage("应用编码已存在");
			}
		}
		return result;
	}

	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public @ResponseBody Result enable(
			@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable) {
		appService.enable(isEnable, getAjaxIds(ids));
		return Result.createSuccessResult();
	}

	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ValidateParam(name = "ID") Integer id,
			@ValidateParam(name = "名称 ", validators = { Validator.NOT_BLANK }) String name,
			@ValidateParam(name = "应用编码 ", validators = { Validator.NOT_BLANK }) String code,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable,
			@ValidateParam(name = "排序 ", validators = { Validator.NOT_BLANK, Validator.INT }) Integer sort) {
		App app;
		if (id == null) {
			app = new App();
			app.setCreateTime(new Date());
		}
		else {
			app = appService.get(id);
		}
		app.setName(name);
		app.setSort(sort);
		app.setIsEnable(isEnable);
		app.setCode(code);
		appService.save(app);
		return Result.createSuccessResult();
	}

	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(
			@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids) {
		appService.deleteById(getAjaxIds(ids));
		return Result.createSuccessResult();
	}
	
	@RequestMapping(value = "/sync/permissions", method = RequestMethod.POST)
	public @ResponseBody Result syncPermissions(
			@ValidateParam(name = "应用编码集合", validators = { Validator.NOT_BLANK }) String codes) {
		PermissionSubject permissionSubject = SpringUtils.getBean(PermissionSubject.class);
		if (permissionSubject != null) {
			String[] codeArray = StringUtils.split(codes, ",");
			for (String code : codeArray) {
				permissionSubject.update(code);
			}
		}
		return Result.createSuccessResult().setMessage("权限同步成功");
	}
}