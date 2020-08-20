package com.smart.sso.server.controller.admin;

import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.constant.ResultConstant;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.exception.ValidateException;
import com.smart.mvc.model.Page;
import com.smart.mvc.model.Result;
import com.smart.mvc.util.ConfigUtils;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.Office;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.OfficeService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.util.PasswordHelper;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

/**
 * @author Joe
 */
@Api(tags = "用户管理")
@Controller
@RequestMapping("/admin/user")
@SuppressWarnings("rawtypes")
public class UserController extends BaseController {

	@Autowired
	private UserService userService;
	@Autowired
	private OfficeService officeService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "/admin/user";
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ApiParam(value = "id") Integer id, Model model) {
		User user;
		if (id == null) {
			user = new User();
		}
		else {
			user = userService.selectById(id);
		}
		model.addAttribute("user", user);
		model.addAttribute("officeList", officeService.selectList(true, null, null, "----"));
		return "/admin/userEdit";
	}

	@ApiOperation("列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@ApiParam(value = "登录名") String account,
			@ApiParam(value = "姓名") String name,
			@ApiParam(value = "机构ID") Integer officeId,
			@ApiParam(value = "开始页码", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageNo,
			@ApiParam(value = "显示条数", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccess(userService.selectPage(account, name, officeId, Page.create(pageNo, pageSize)));
	}

	@ApiOperation("验证登录名")
	@ResponseBody
	@RequestMapping(value = "/validateAccount", method = RequestMethod.POST)
	public Result validateAccount(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "登录名", required = true) @ValidateParam({ Validator.NOT_BLANK }) String account) {
		User user = userService.selectByAccount(account);
		if (null != user && !user.getId().equals(id)) {
		    return Result.create(ResultConstant.ERROR, "登录名已存在");
		}
		return Result.success();
	}

	@ApiOperation("启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids,
			@ApiParam(value = "是否启用", required = true) @ValidateParam({ Validator.NOT_BLANK }) Boolean isEnable) {
		userService.enable(isEnable, convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "机构ID", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer officeId,
			@ApiParam(value = "姓名") String name,
			@ApiParam(value = "登录名", required = true) @ValidateParam({ Validator.NOT_BLANK }) String account,
			@ApiParam(value = "密码 ") String password,
			@ApiParam(value = "是否启用", required = true) @ValidateParam({ Validator.NOT_BLANK }) Boolean isEnable) {
		User user;
		if (id == null) {
			if (StringUtils.isEmpty(password)) {
				throw new ValidateException("密码不能为空");
			}
			user = new User();
			user.setCreateTime(new Date());
		}
		else {
			user = userService.selectById(id);
		}
		user.setOfficeId(officeId);
		user.setName(name);
		user.setAccount(account);
		if (!StringUtils.isEmpty(password)) {
			user.setPassword(PasswordHelper.encrypt(password));
		}
		user.setIsEnable(isEnable);
		userService.save(user);
		return Result.success();
	}

	@ApiOperation("重置密码")
	@ResponseBody
	@RequestMapping(value = "/resetPassword", method = RequestMethod.POST)
	public Result resetPassword(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		userService.resetPassword(PasswordHelper.encrypt(ConfigUtils.getProperty("system.reset.password")), convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		userService.deleteByIds(convertToIdList(ids));
		return Result.success();
	}
	
	@ApiOperation("机构树")
	@ResponseBody
	@RequestMapping(value = "/office/tree", method = RequestMethod.GET)
	public List<Office> officeTree() {
		List<Office> list = officeService.selectList(true, null, null, "");
		Office office = new Office();
		office.setId(null);
		office.setParentId(-1);
		office.setName("机构");
		list.add(0, office);
		return list;
	}
}