package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.enums.ErrorCodeEnum;
import openjoe.smart.sso.server.service.AppService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.stage.core.entity.Result;
import openjoe.smart.stage.exception.ApplicationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.Date;

/**
 * @author Joe
 */
@Api(tags = "应用管理")
@Controller
@RequestMapping("/admin/app")
@SuppressWarnings("rawtypes")
public class AppController {

	@Autowired
	private AppService appService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute() {
		return "/admin/app";
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@RequestParam(required = false) Long id, Model model) {
		App app;
		if (id == null) {
			app = new App();
		}
		else {
			app = appService.getById(id);
		}
		model.addAttribute("app", app);
		return "/admin/appEdit";
	}

    @ApiOperation("列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@RequestParam(required = false) String name,
			@RequestParam Long current,
			@RequestParam Long size) {
		return Result.success(appService.selectPage(name, current, size));
	}

	@ApiOperation("验证应用编码")
	@ResponseBody
	@RequestMapping(value = "/validateCode", method = RequestMethod.POST)
	public Result validateCode(
			@RequestParam(required = false) Long id,
			@RequestParam String code) {
		App db = appService.selectByCode(code);
		if (null != db && !db.getId().equals(id)) {
			throw new ApplicationException(ErrorCodeEnum.E1003);
		}
		return Result.success();
	}

	@ApiOperation("启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
			@RequestParam String ids,
			@RequestParam Boolean isEnable) {
		appService.enable(isEnable, ConvertUtils.convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam(required = false) Long id,
			@RequestParam String name,
			@RequestParam String code,
			@RequestParam Boolean isEnable,
			@RequestParam Integer sort) {
		App app;
		if (id == null) {
			app = new App();
			app.setCreateTime(new Date());
		}
		else {
			app = appService.getById(id);
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
	public Result delete(@RequestParam String ids) {
		appService.deleteByIds(ConvertUtils.convertToIdList(ids));
		return Result.success();
	}
}